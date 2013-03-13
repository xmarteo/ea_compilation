(* This module translates [UPP] instructions into [RTL] control flow
   graphs. It is parameterized over a module [Env], whose signature
   appears below, which provides support for mapping local variables
   to pseudo-registers, allocating fresh pseudo-registers, and
   generating instructions (which requires allocating fresh control
   flow graph labels). *)

module Make (Env : sig

  (* [lookup x] returns the pseudo-register that holds the local
     variable [x]. *)

  val lookup: string -> Register.t

  (* [allocate()] returns a fresh pseudo-register. *)

  val allocate: unit -> Register.t

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: RTL.instruction -> Label.t

  (* [loop target] returns a fresh instruction label [label], which it
     associates with an unconditional branch instruction whose target
     is [target label]. *)

  val loop: (Label.t -> Label.t) -> Label.t

  (* [is_exit label] tells whether the label [label] is the exit label
     of the current procedure or function. This can be used to determine
     which calls are tail calls. *)

  val is_exit: Label.t -> bool

  (* [result] is [None] if this is a procedure, and [Some f] if this
     a function named [f]. *)

  val result: string option

end) : sig

  (* [translate_instruction i destl] generates new [RTL] instructions
     whose effect is to execute the [UPP] instruction [i] and to
     transfer control to the destination label [destl]. It returns the
     entry label of the newly generated instructions. *)

  val translate_instruction: UPP.instruction -> Label.t -> Label.t

end

