(* This module provides an interpreter for [PP] programs. *)

exception RuntimeError

val interpret: PP.program -> unit

