open Printf
open Print
open MIPSOps
open Primitive
open PP

(* Types. *)

let rec typ () = function
  | TypInt ->
      sprintf "integer"
  | TypBool ->
      sprintf "boolean"
  | TypArray t ->
      sprintf "array of %a" typ t

(* Constants. *)

let constant () = function
  | ConstBool true ->
      sprintf "true"
  | ConstBool false ->
      sprintf "false"
  | ConstInt i ->
      sprintf "%ld" i

(* Binary operators. *)

let binop = function
  | OpAdd ->
      "+"
  | OpSub ->
      "-"
  | OpMul ->
      "*"
  | OpDiv ->
      "/"
  | OpLt ->
      "<"
  | OpLe ->
      "<="
  | OpGt ->
      ">"
  | OpGe ->
      ">="
  | OpEq ->
      "="
  | OpNe ->
      "<>"

(* Expressions are separated in several levels, so as to properly
   respect operator priorities and insert parentheses where needed.
   In principle, the composition (both ways) of this printer with the
   parser is the identity, but that would probably be difficult to
   prove. Ideas? *)

let rec expr0 () = function
  | EConst c ->
      sprintf "%a" constant c
  | EGetVar x ->
      sprintf "%s" x
  | EFunCall (c, es) ->
      sprintf "%s(%a)" (PrintPrimitive.callee c) (seplist comma expr) es
  | EArrayAlloc (t, e) ->
      sprintf "new array of %a [%a]" typ t expr e
  | e ->
      sprintf "(%a)" expr e

and expr1 () = function
  | EArrayGet (ea, ei) ->
      sprintf "%a[%a]" expr1 ea expr ei
  | e ->
      expr0 () e

and expr2 () = function
  | EUnOp (UOpNeg, e) ->
      sprintf "-%a" expr1 e
  | e ->
      expr1 () e

and expr3 () = function
  | EBinOp ((OpMul | OpDiv) as op, e1, e2) ->
      sprintf "%a %s %a" expr3 e1 (binop op) expr2 e2
  | e ->
      expr2 () e

and expr4 () = function
  | EBinOp ((OpSub | OpAdd) as op, e1, e2) ->
      sprintf "%a %s %a" expr4 e1 (binop op) expr3 e2
  | e ->
      expr3 () e

and expr5 () = function
  | EBinOp ((OpLt | OpLe | OpGt | OpGe | OpEq | OpNe) as op, e1, e2) ->
      sprintf "%a %s %a" expr4 e1 (binop op) expr4 e2
  | e ->
      expr4 () e

and expr () e =
  expr5 () e

(* Conditions are similarly separated into several levels. *)

let rec cond0 () = function
  | CExpression e ->
      expr () e
  | CNot c ->
      sprintf "not %a" cond0 c
  | c ->
      sprintf "(%a)" cond c

and cond1 () = function
  | CAnd (c1, c2) ->
      sprintf "%a and %a" cond1 c1 cond0 c2
  | c ->
      cond0 () c

and cond2 () = function
  | COr (c1, c2) ->
      sprintf "%a or %a" cond2 c1 cond1 c2
  | c ->
      cond1 () c

and cond () c =
  cond2 () c

(* Instructions and blocks. *)

(* Instructions are separated in two levels in order to deal with the
   dangling [else] problem. TEMPORARY do it. tricky? *)

let mkblock contents () c =
  sprintf "begin%a%tend" (indent 2 contents) c nl

let rec instr () = function
  | IProcCall (c, es) ->
      sprintf "%s(%a)" (PrintPrimitive.callee c) (seplist comma expr) es
  | ISetVar (x, e) ->
      sprintf "%s := %a" x expr e
  | IArraySet (ea, ei, ev) ->
      sprintf "%a[%a] := %a" expr1 ea expr ei expr ev
  | IIf (c, i1, i2) ->
      sprintf "if %a then%a%telse%a"
	cond c
	(indent 2 instr) i1
	nl
	(indent 2 instr) i2
  | IWhile (c, i) ->
      sprintf "while %a do%a"
	cond c
	(indent 2 instr) i
  | ISeq is ->
      sprintf "%a" (mkblock (seplist seminl instr)) is

and block () i =
  match i with
  | ISeq _ ->
      instr () i
  | i ->
      sprintf "%a" (mkblock instr) i

(* Function definitions. *)

let binding () (x, t) =
  sprintf "%s : %a" x typ t

let vars () vars =
  annlist var (indent 2 (termlist seminl binding)) () vars
  (* TEMPORARY affichage pas tout-à-fait correct, le dernier seminl indente trop loin *)

let proc () (name, proc) =
  Settings.delimit name (
    match proc.result with
    | None ->
	sprintf "procedure %s(%a);\n%a%a;"
	  name
	  (seplist semicolon binding) proc.formals
	  vars (StringMap.to_association_list proc.locals)
	  block proc.body
    | Some t ->
	sprintf "function %s(%a) : %a;\n%a%a;"
	  name
	  (seplist semicolon binding) proc.formals
	  typ t
	  vars (StringMap.to_association_list proc.locals)
	  block proc.body
  )

(* Programs. *)

let print_program () p =
  sprintf "program\n\n%a\n%a%a.\n"
    vars (StringMap.to_association_list p.globals)
    (termlist nlnl proc) (StringMap.to_association_list p.defs)
    block p.main

