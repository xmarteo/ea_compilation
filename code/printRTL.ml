open Printf
open Print
open RTL

let reg () r =
  Register.print r

let lab () l =
  Label.print l

let instruction () = function
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
  | ICall (destro, callee, sourcers, l) ->
      sprintf "call  %a%s(%a)"
	(Option.print (fun () destr -> sprintf "%a, " reg destr)) destro
	(PrintPrimitive.callee callee)
	(seplist comma reg) sourcers ::
      sprintf " --> %a" lab l ::
      []
  | ITailCall (callee, sourcers) ->
      sprintf "tail  %s(%a)"
	(PrintPrimitive.callee callee)
	(seplist comma reg) sourcers ::
      "" ::
      []
  | ILoad (destr, sourcer, offset, l) ->
      sprintf "lw    %a, %ld(%a)" reg destr offset reg sourcer ::
      sprintf " --> %a" lab l ::
      []
  | IStore (addressr, offset, valuer, l) ->
      sprintf "sw    %a, %ld(%a)" reg valuer offset reg addressr ::
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

let successors = function
  | ITailCall _ ->
      []
  | IGetGlobal (_, _, l)
  | ISetGlobal (_, _, l)
  | IConst (_, _, l)
  | IUnOp (_, _, _, l)
  | IBinOp (_, _, _, _, l)
  | ICall (_, _, _, l)
  | ILoad (_, _, _, l)
  | IStore (_, _, _, l)
  | IGoto l ->
      [ l ]
  | IUnBranch (_, _, l1, l2)
  | IBinBranch (_, _, _, l1, l2) ->
      [ l2; l1 ]

let line l i =
  [ Label.print l; ": " ] @ instruction () i

let presult () r =
  sprintf " : %a" reg r

let proc () (name, proc) =
  Settings.delimit name (
    sprintf "%s %s(%a)%a\nvar %a\nentry %a\nexit %a\n%a"
      (match proc.result with None -> "procedure" | Some _ -> "function")
      name
      (seplist semicolon reg) proc.formals
      (Option.print presult) proc.result
      (seplist (atmost 7 comma (nlspace 4)) reg) (Register.Set.elements proc.locals)
      lab proc.entry
      lab proc.exit
      (PrintCFG.print_graph line successors) (proc.graph, proc.entry)
  )

let print_program () p =
  sprintf "program\n\nglobals %ld\n\n%a"
    p.globals
    (termlist nlnl proc) (StringMap.to_association_list p.defs)

