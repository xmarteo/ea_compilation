open Printf
open Print
open ERTL

let reg () r =
  Register.print r

let lab () l =
  Label.print l

let hwr () h =
  sprintf "$%s" (MIPS.print h)

type phr =
  | Hardware of MIPS.register
  | Pseudo of Register.t

let phr () = function
  | Hardware h ->
      hwr () h
  | Pseudo r ->
      reg () r

let phrs () (rs, hs) =
  seplist comma phr ()
    (List.map (fun r -> Pseudo r) (Register.Set.elements rs) @
     List.map (fun hwr -> Hardware hwr) (MIPS.RegisterSet.elements hs))  

let slo () = function
  | SlotIncoming o ->
      sprintf "in(%ld)" o
  | SlotOutgoing o ->
      sprintf "out(%ld)" o

let instruction () = function
  | INewFrame l ->
      sprintf "newframe" ::
      sprintf " --> %a" lab l ::
      []
  | IDeleteFrame l ->
      sprintf "delframe" ::
      sprintf " --> %a" lab l ::
      []
  | IGetHwReg (destr, sourcehwr, l) ->
      sprintf "move  %a, %a" reg destr hwr sourcehwr ::
      sprintf " --> %a" lab l ::
      []
  | ISetHwReg (desthwr, sourcer, l) ->
      sprintf "move  %a, %a" hwr desthwr reg sourcer ::
      sprintf " --> %a" lab l ::
      []
  | IGetStack (destr, slot, l) ->
      sprintf "gets  %a, %a" reg destr slo slot ::
      sprintf " --> %a" lab l ::
      []
  | ISetStack (slot, sourcer, l) ->
      sprintf "sets  %a, %a" slo slot reg sourcer ::
      sprintf " --> %a" lab l ::
      []
  | IConst (r, i, l) ->
      sprintf "li    %a, %ld" reg r i ::
      sprintf " --> %a" lab l ::
      []
  | IUnOp (op, destr, sourcer, l) ->
      sprintf "%a" (PrintOps.unop reg) (op, destr, sourcer) ::
      sprintf " --> %a" lab l ::
      []
  | IBinOp (op, destr, sourcer1, sourcer2, l) ->
      sprintf "%s %a, %a, %a" (PrintOps.binop op) reg destr reg sourcer1 reg sourcer2 ::
      sprintf " --> %a" lab l ::
      []
  | ICall (callee, nparams, l) ->
      sprintf "call  %s(%ld)" (PrintPrimitive.callee callee) nparams ::
      sprintf " --> %a" lab l ::
      []
  | ITailCall (callee, nparams) ->
      sprintf "tail  %s(%ld)" (PrintPrimitive.callee callee) nparams ::
      "" ::
      []
  | ILoad (destr, sourcer, offset, l) ->
      sprintf "lw    %a, %ld(%a)" reg destr offset reg sourcer ::
      sprintf " --> %a" lab l ::
      []
  | IStore (addressr, offset, valuer, l) ->
      sprintf "sw   %a, %ld(%a)" reg valuer offset reg addressr ::
      sprintf " --> %a" lab l ::
      []
  | IGetGlobal (destr, offset, l) ->
      sprintf "getg  %a, %ld" reg destr offset ::
      sprintf " --> %a" lab l ::
      []
  | ISetGlobal (offset, valuer, l) ->
      sprintf "setg  %ld, %a" offset reg valuer ::
      sprintf " --> %a" lab l ::
      []
  | IGoto l ->
      sprintf "j" ::
      sprintf " --> %a" lab l ::
      []
  | IUnBranch (cond, sourcer, l1, l2) ->
      sprintf "%a" (PrintOps.uncon reg) (cond, sourcer) ::
      sprintf " --> %a, %a" lab l1 lab l2 ::
      []
  | IBinBranch (cond, sourcer1, sourcer2, l1, l2) ->
      sprintf "%s %a, %a" (PrintOps.bincon cond) reg sourcer1 reg sourcer2 ::
      sprintf " --> %a, %a" lab l1 lab l2 ::
      []
  | IReturn with_a_result ->
      sprintf "jr    $ra" ::
      (if with_a_result then " (xmits $v0)" else "") ::
      []

let successors = function
  | IReturn _
  | ITailCall _ ->
      []
  | INewFrame l
  | IDeleteFrame l
  | IGetHwReg (_, _, l)
  | ISetHwReg (_, _, l)
  | IGetStack (_, _, l)
  | ISetStack (_, _, l)
  | IGetGlobal (_, _, l)
  | ISetGlobal (_, _, l)
  | IConst (_, _, l)
  | IUnOp (_, _, _, l)
  | IBinOp (_, _, _, _, l)
  | ICall (_, _, l)
  | ILoad (_, _, _, l)
  | IStore (_, _, _, l)
  | IGoto l ->
      [ l ]
  | IUnBranch (_, _, l1, l2)
  | IBinBranch (_, _, _, l1, l2) ->
      [ l2; l1 ]

let proc live () (name, proc) =

  let line =
    if live then
      let liveafter = Liveness.analyze proc in
      fun l i ->
	  [ Label.print l; ": " ] @ instruction () i @ [ "    "; sprintf "%a" phrs (liveafter l) ]
    else
      fun l i ->
	[ Label.print l; ": " ] @ instruction () i
  in

  Settings.delimit name (
    sprintf "procedure %s(%ld)\nvar %a\nentry %a\n%a"
      name
      proc.formals
      (seplist (atmost 7 comma (nlspace 4)) reg) (Register.Set.elements proc.locals)
      lab proc.entry
      (PrintCFG.print_graph line successors) (proc.graph, proc.entry)
  )

let print_program live () p =
  sprintf "program\n\nglobals %ld\n\n%a"
    p.globals
    (termlist nlnl (proc live)) (StringMap.to_association_list p.defs)

