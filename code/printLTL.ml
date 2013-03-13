open Printf
open Print
open LTL

let reg () r =
  sprintf "$%s" (MIPS.print r)

let lab () l =
  Label.print l

let slo () = function
  | SlotLocal o ->
      sprintf "local(%ld)" o
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
  | ICall (callee, l) ->
      sprintf "call  %s" (PrintPrimitive.callee callee) ::
      sprintf " --> %a" lab l ::
      []
  | ITailCall callee ->
      sprintf "tail  %s" (PrintPrimitive.callee callee) ::
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
  | IReturn ->
      sprintf "jr    $ra" ::
      "" ::
      []

let successors = function
  | IReturn
  | ITailCall _ ->
      []
  | INewFrame l
  | IDeleteFrame l
  | IGetStack (_, _, l)
  | ISetStack (_, _, l)
  | IConst (_, _, l)
  | IUnOp (_, _, _, l)
  | IBinOp (_, _, _, _, l)
  | ICall (_, l)
  | ILoad (_, _, _, l)
  | IStore (_, _, _, l)
  | IGoto l ->
      [ l ]
  | IUnBranch (_, _, l1, l2)
  | IBinBranch (_, _, _, l1, l2) ->
      [ l2; l1 ]

let proc () (name, proc) =

  let line l i =
    [ Label.print l; ": " ] @ instruction () i
  in

  Settings.delimit name (
    sprintf "procedure %s(%ld)\nvar %ld\nentry %a\n%a"
      name
      proc.formals
      proc.locals
      lab proc.entry
      (PrintCFG.print_graph line successors) (proc.graph, proc.entry)
  )

let print_program () p =
  sprintf "program\n\nglobals %ld\n\n%a"
    p.globals
    (termlist nlnl proc) (StringMap.to_association_list p.defs)

