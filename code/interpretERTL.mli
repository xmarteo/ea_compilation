(* This module provides an interpreter for [ERTL] programs. *)

val interpret: ERTL.program -> unit

(* The following functionality is exported for use by interpreters
   of the forthcoming intermediate languages. *)

type ('procedure, 'label) address =
  | AddrCode of 'procedure * 'label
  | AddrInit

type 'value henv =
    'value ref MIPS.RegisterMap.t

val hread: 'address InterpretUPP.value henv -> MIPS.register -> 'address InterpretUPP.value
val hwrite: 'value henv -> MIPS.register -> 'value -> unit
val interpret_primitive: 'address InterpretUPP.value henv -> Primitive.primitive -> unit
val destruct: 'a list -> 'a * 'a list

