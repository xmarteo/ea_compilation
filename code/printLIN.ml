open Printf
open Print
open LIN

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
  | INewFrame ->
      sprintf "newframe"
  | IDeleteFrame ->
      sprintf "delframe"
  | IGetStack (destr, slot) ->
      sprintf "gets  %a, %a" reg destr slo slot
  | ISetStack (slot, sourcer) ->
      sprintf "sets  %a, %a" slo slot reg sourcer
  | IConst (r, i) ->
      sprintf "li    %a, %ld" reg r i
  | IUnOp (op, destr, sourcer) ->
      sprintf "%a" (PrintOps.unop reg) (op, destr, sourcer)
  | IBinOp (op, destr, sourcer1, sourcer2) ->
      sprintf "%s %a, %a, %a" (PrintOps.binop op) reg destr reg sourcer1 reg sourcer2
  | ICall callee ->
      sprintf "call  %s" (PrintPrimitive.callee callee)
  | ITailCall callee ->
      sprintf "tail  %s" (PrintPrimitive.callee callee)
  | ILoad (destr, sourcer, offset) ->
      sprintf "lw    %a, %ld(%a)" reg destr offset reg sourcer
  | IStore (addressr, offset, valuer) ->
      sprintf "sw    %a, %ld(%a)" reg valuer offset reg addressr
  | IGoto l ->
      sprintf "j     %a" lab l
  | IUnBranch (cond, sourcer, l) ->
      sprintf "%a, %a" (PrintOps.uncon reg) (cond, sourcer) lab l
  | IBinBranch (cond, sourcer1, sourcer2, l) ->
      sprintf "%s %a, %a, %a" (PrintOps.bincon cond) reg sourcer1 reg sourcer2 lab l
  | IReturn ->
      sprintf "jr    $ra"
  | ILabel l ->
      sprintf "%a:" lab l

let proc () (name, proc) =
  Settings.delimit name (
    sprintf "procedure %s(%ld)\nvar %ld\n%a"
      name
      proc.formals
      proc.locals
      (seplist nl instruction) proc.code
  )

let print_program () p =
  sprintf "program\n\nglobals %ld\n\n%a"
    p.globals
    (termlist nlnl proc) (StringMap.to_association_list p.defs)

