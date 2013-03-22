(* This module provides an interpreter for [LTL] programs. *)

val interpret: LTL.program -> unit

(* The following functionality is exported for use by interpreters
   of the forthcoming intermediate languages. *)

open InterpretUPP
open InterpretERTL

type 'value stack

val empty_stack: 'value stack
val sread: 'address value stack -> LTL.slot -> 'address value
val swrite: 'address value stack -> LTL.slot -> 'address value -> unit
val newframe: 'address value stack -> int32 -> 'address value stack
val deleteframe: 'value stack -> 'value stack
