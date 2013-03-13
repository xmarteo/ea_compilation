(* The functor [Visit] implements the core of the translation of
   [LTL] to [LIN]. *)

module Visit (S : sig

  (* [fetch l] is the instruction found at label [l] in the source
     program. *)

  val fetch: Label.t -> LTL.instruction

  (* [translate_instruction i] translates the [LTL] instruction [i] to
     a [LIN] instruction. [LTL] instructions that have one explicit
     successor are turned into [LIN] instructions with an implicit
     successor. [LTL] instructions that have two explicit successors
     are turned into [LIN] instructions where the first successor is
     explicit and the second successor is implicit. *)

  val translate_instruction: LTL.instruction -> LIN.instruction

  (* [generate i] generates instruction [i]. Instructions are
     generated sequentially. *)

  val generate: LIN.instruction -> unit

  (* [require l] records the fact that the label [l] should explicitly
     exist in the [LIN] program. It must be used whenever a [LIN]
     branch instruction is issued. *)

  val require: Label.t -> unit

  (* [mark l] marks the label [l]. [marked l] tells whether [l] is
     marked. *)

  val mark: Label.t -> unit
  val marked: Label.t -> bool

end) : sig

  (* [visit] implements a depth-first traversal of the control flow graph,
     generating instructions as new nodes are being discovered.

     If label [l] has already been discovered, then [visit l] issues
     an [IGoto] instruction towards [l]. If [l] has not been
     discovered yet, [visit l] marks [l] as discovered, issues an
     [ILabel] instruction, translates the instruction found at [l] in
     the source program, and visits its successors. *)

  val visit: Label.t -> unit

end

