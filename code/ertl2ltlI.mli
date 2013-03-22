(* This module translates [ERTL] instructions into [LTL]
   instructions. It is parameterized over a module [Env], whose
   signature appears below, which provides support for mapping
   pseudo-registers to stack slots or hardware registers and for
   generating instructions (which requires allocating fresh control
   flow graph labels). *)

type decision =
  | Spill of MIPSOps.offset
  | Color of MIPS.register

module Make (Env : sig

  val lookup: Register.t -> decision

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: LTL.instruction -> Label.t

end) : sig

  (* [translate_instruction] turns an [ERTL] instruction into an [LTL]
     instruction, or sequence of instructions, that transfers control
     to the same label(s).

     Existing instruction labels are preserved, that is, the labels in
     the new control flow graph form a superset of the labels in the
     existing control flow graph. *)

  val translate_instruction: ERTL.instruction -> LTL.instruction

end

