(* This module offers functions that count how many times each
   pseudo-register is used within a piece of [ERTL] code. This
   is used in [Coloring] to drive the spilling heuristics. *)

(* [examine_procedure proc] counts how many times each pseudo-register
   is used within procedure [proc]. It returns a function that maps
   pseudo-registers to integer use counts. *)

val examine_procedure: ERTL.procedure -> (Register.t -> int)

