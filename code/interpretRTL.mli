(* This module provides an interpreter for [RTL] programs. *)

val interpret: RTL.program -> unit

(* The following functionality is exported for use by interpreters
   of the forthcoming intermediate languages. *)

val read: 'value ref Register.Map.t -> Register.t -> 'value
val write: 'value ref Register.Map.t -> Register.t -> 'value -> unit
