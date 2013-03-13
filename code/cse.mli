(* This module performs common subexpression elimination (CSE).
   It transforms an [RTL] program into another [RTL] program. *)

val translate_program: RTL.program -> RTL.program

