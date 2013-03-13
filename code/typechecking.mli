(* [typecheck_program p] checks that the program [p] is well-typed,
   and aborts execution if it isn't. *)

val typecheck_program: LPP.program -> unit

