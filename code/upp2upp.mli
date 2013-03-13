(* This module provides functions for applying a [PP] unary or
   binary operator to [UPP] expressions.

   These functions build a single [UPP] abstract syntax tree
   node. Furthermore, they perform optimization by rewriting
   certain [UPP] expressions into equivalent, more efficient
   forms. *)

open UPP
val mkunop: PP.unop -> expression -> expression
val mkbinop: MIPSOps.binop -> expression -> expression -> expression

