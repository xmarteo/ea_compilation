(* This module provides interpretations of the MIPS operators,
   for use in building interpreters or in the compiler itself. *)

val unop: MIPSOps.unop -> (int32 -> int32)
val binop: MIPSOps.binop -> (int32 -> int32 -> int32)

val uncon: MIPSOps.uncon -> (int32 -> bool)
val bincon: MIPSOps.bincon -> (int32 -> int32 -> bool)
