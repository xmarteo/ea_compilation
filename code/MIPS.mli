(* This is the size of a machine word in bytes. *)

val word: int32

(* This is the type of hardware registers. *)

type register

val equal: register -> register -> bool
val print: register -> string

(* A list of the registers used for passing function parameters. *)

val parameters: register list

(* The register used for returning function results. *)

val result: register

(* The return address register. It is best thought of as a register
   that is used to pass a parameter (namely, the return address). *)

val ra: register

(* The zero register always holds the value 0. Although it is a
   special register, it is considered allocatable; see module [Zero]
   for an explanation. *)

val zero: register

(* Sets of hardware registers. *)

module RegisterSet : sig
  include Set.S with type elt = register
  val disjoint: t -> t -> bool
  val of_list: elt list -> t
end

(* Maps over hardware registers. *)

module RegisterMap : sig

  include Map.S with type key = register

  (* [lift f s] turns the set [s] into a map where every element [x]
     is mapped to [f x]. *)

  val lift: (key -> 'a) -> RegisterSet.t -> 'a t

end

(* A set of all allocatable hardware registers, that is, of all
   registers that are available for use by the register allocator --
   as opposed to reserved for some fixed use. *)

val allocatable: RegisterSet.t

(* A set of all allocatable ``caller-saved'' hardware registers, that
   is, of all allocatable registers that might be overwritten during a
   function call. This includes the so-called ``caller-saved temporary
   registers'' [$t0-$t9] as well as the registers used to implement
   the calling convention, namely [$a0-$a3], [$v0], and [$ra]. *)

val caller_saved: RegisterSet.t

(* A set of all allocatable ``callee-saved'' hardware registers, that
   is, of all allocatable registers that must be preserved by function
   calls. *)

val callee_saved: RegisterSet.t

(* Two non-allocatable registers, reserved for transferring spilled
   pseudo-registers to and from the stack. *)

val st0: register
val st1: register

(* The stack pointer register. *)

val sp: register

(* The global pointer register. *)

val gp: register

(* A set of all registers that are used in the code that we generate.
   This includes all allocatable registers, plus the four special
   registers mentioned above. *)

val registers: RegisterSet.t

