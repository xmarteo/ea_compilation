open Printf
open Print
open MIPSOps
open Primitive
open UPP

(* Unary operators. *)

let unop = function
  | UOpAddi i ->
      sprintf "(%ld +)" i
  | UOpSlli i ->
      sprintf "(<< %ld)" i
  | UOpSlti i ->
      sprintf "(< %ld)" i

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
   respect operator priorities and insert parentheses where needed. *)

let rec expr0 () = function
  | EConst i ->
      sprintf "%ld" i
  | EGetVar x ->
      sprintf "%s" x
  | EGetGlobal offset ->
      sprintf "global(%ld)" offset
  | EFunCall (c, es) ->
      sprintf "%s(%a)" (PrintPrimitive.callee c) (seplist comma expr) es
  | ENewArray e ->
      sprintf "newarray len %a" expr e
  | e ->
      sprintf "(%a)" expr e

and expr1 () = function
  | ELoad (ea, offset) ->
      sprintf "%a[%ld]" expr1 ea offset
  | e ->
      expr0 () e

and expr2 () = function
  | EUnOp (op, e) ->
      sprintf "%s%a" (unop op) expr1 e
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
  | COr (c1, c2) ->
      sprintf "%a or %a" cond1 c1 cond0 c2
  | c ->
      cond0 () c

and cond () c =
  cond1 () c

(* Instructions and blocks. *)

let mkblock contents () c =
  sprintf "begin%a%tend" (indent 2 contents) c nl

let rec instr () = function
  | IProcCall (c, es) ->
      sprintf "%s(%a)" (PrintPrimitive.callee c) (seplist comma expr) es
  | ISetVar (x, e) ->
      sprintf "%s := %a" x expr e
  | ISetGlobal (offset, e) ->
      sprintf "global(%ld) := %a" offset expr e
  | IStore (ea, offset, ev) ->
      sprintf "%a[%ld] := %a" expr1 ea offset expr ev
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

let binding () x =
  sprintf "%s" x

let vars () vars =
  annlist var (indent 2 (termlist seminl binding)) () vars
  (* TEMPORARY affichage pas tout-à-fait correct, le dernier seminl indente trop loin *)

let proc () (name, proc) =
  Settings.delimit name (
    sprintf "%s %s(%a);\n%a%a;"
      (if proc.result then "function" else "procedure")
      name
      (seplist semicolon binding) proc.formals
      vars (StringSet.elements proc.locals)
      block proc.body
  )

(* Programs. *)

let print_program () p =
  sprintf "program%tglobals %ld%t%a"
    nlnl
    p.globals
    nlnl
    (termlist nlnl proc) (StringMap.to_association_list p.defs)

