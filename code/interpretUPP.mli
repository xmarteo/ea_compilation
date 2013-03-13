(* This module provides an interpreter for [UPP] programs. *)

val interpret: UPP.program -> unit

(* The following functionality is exported for use by interpreters
   of the forthcoming intermediate languages. *)

type 'address value =
  | VUndefined
  | VInt of int32
  | VArray of 'address value array * int32
  | VCode of 'address

val asInt: 'address value -> int32
val asAddress: 'address value -> 'address
val load: 'address value -> int32 -> 'address value
val store: 'address value -> int32 -> 'address value -> unit
val unop: MIPSOps.unop -> 'address value -> 'address value
val binop: MIPSOps.binop -> 'address value -> 'address value -> 'address value
val default: 'address value
val allocate: 'a -> 'address value ref
val init: 'a -> 'address value ref
val interpret_alloc: 'address value -> 'address value
val interpret_primitive: Primitive.primitive -> 'address value list -> 'address value option
val gread: 'value array -> MIPSOps.offset -> 'value
val gwrite: 'value array -> MIPSOps.offset -> 'value -> unit

