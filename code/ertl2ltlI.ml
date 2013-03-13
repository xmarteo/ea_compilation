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

  (* [lookup r] returns the decision that has been made about
     pseudo-register [r]. *)

  val lookup: Register.t -> decision

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: LTL.instruction -> Label.t

end) = struct

open Env
open MIPSOps
open Integer

(* ------------------------------------------------------------------------- *)

(* [ERTL] stack slots form a sublanguage of [LTL] stack slots. *)

let translate_slot = function
  | ERTL.SlotIncoming o ->
      LTL.SlotIncoming o
  | ERTL.SlotOutgoing o ->
      LTL.SlotOutgoing o

(* ------------------------------------------------------------------------- *)

(* Accesses to pseudo-registers are translated to accesses to either
   hardware registers or stack slots. Note that an access to a stack
   slot requires a temporary hardware register, to be used in the
   [ISetStack] or [IGetStack] instruction. In fact, because some
   instructions are binary, we need two such temporary hardware
   registers. We use [$st0] and [$st1] for this purpose. These
   hardware registers are not made available to the register
   allocator, so they are always available. *)

(* If you want to write into pseudo-register [r] and transfer control
   to label [l], then invoke [write r l] -- it will return a pair of a
   hardware register [hwr] and a label [l'] -- these are the register
   that you should really write and the label that you should really
   transfer control to. *)

let write r l =
  match lookup r with
  | Color hwr ->

      (* Pseudo-register [r] has been mapped to hardware register
	 [hwr]. Just write into [hwr] and branch to [l]. *)

      hwr, l

  | Spill slot ->

      (* Pseudo-register [r] has been mapped to stack slot [slot].
	 Then, write into [$st0] and transfer control to an
	 instruction that copies [$st0] into [slot] before branching
	 to [l]. *)

      MIPS.st0, generate (LTL.ISetStack (LTL.SlotLocal slot, MIPS.st0, l))

(* If you want to read pseudo-register [r] as part of instruction [i]
   -- which really reads a hardware register -- then you should really
   generate instruction [read1 r i].

   If you want to read pseudo-registers [r1] and [r2] as part of
   instruction [i] -- which really reads two hardware registers --
   then you should really generate instruction [read2 r1 r2 i].

   [read1] and [read2] are defined in terms of [read], whose parameter
   [temphwr] stands for either [$st0] or [$st1]. *)

let read temphwr r (i : MIPS.register -> LTL.instruction) =
  match lookup r with
  | Color hwr ->

      (* Pseudo-register [r] has been mapped to hardware register
	 [hwr]. Just generate instruction [i] with a reference to
	 register [hwr]. *)

      i hwr

  | Spill slot ->

      (* Pseudo-register [r] has been mapped to stack slot [slot].
	 Issue an instruction that copies [slot] into the temporary
	 hardware register [temphwr], then generate instruction [i]
	 with a reference to register [temphwr]. *)

      LTL.IGetStack (temphwr, LTL.SlotLocal slot, generate (i temphwr))

let read1 r (i : MIPS.register -> LTL.instruction) =
  read MIPS.st0 r i

let read2 r1 r2 (i : MIPS.register -> MIPS.register -> LTL.instruction) =
  read MIPS.st0 r1 (fun hwr1 ->
    read MIPS.st1 r2 (fun hwr2 ->
      i hwr1 hwr2
    )
  )

(* ------------------------------------------------------------------------- *)

(* Moves between pseudo-registers can be translated without using
   temporary hardware registers (except when both pseudo-registers are
   spilled). *)

let move (dest : decision) (source : decision) l =
  match dest, source with

    (* Both pseudo-registers are translated to hardware registers.
       Issue a single [move] instruction, or no instruction at all if
       both pseudo-registers reside in the same hardware register. *)

  | Color desthwr, Color sourcehwr ->
      if MIPS.equal desthwr sourcehwr then
	LTL.IGoto l
      else
	LTL.IUnOp (UOpAddi 0l, desthwr, sourcehwr, l)

    (* One pseudo-register is translated to a hardware register, while
       the other is spilled. Issue a single stack access
       instruction. *)

  | Color desthwr, Spill sourceslot ->
      LTL.IGetStack (desthwr, LTL.SlotLocal sourceslot, l)
  | Spill destslot, Color sourcehwr ->
      LTL.ISetStack (LTL.SlotLocal destslot, sourcehwr, l)

    (* Both pseudo-registers are spilled. Combine the previous two
       cases, using a temporary hardware register. Of course, if the
       two pseudo-registers have been spilled into the same stack
       slot, there is nothing to do. *)

  | Spill destslot, Spill sourceslot ->
      if destslot = sourceslot then
	LTL.IGoto l
      else
	LTL.IGetStack (MIPS.st0, LTL.SlotLocal sourceslot,
		       generate (LTL.ISetStack (LTL.SlotLocal destslot, MIPS.st0, l)))

(* ------------------------------------------------------------------------- *)

(* [translate_instruction] turns an [ERTL] instruction into an [LTL]
   instruction, or sequence of instructions, that transfers control
   to the same label(s).

   Existing instruction labels are preserved, that is, the labels in
   the new control flow graph form a superset of the labels in the
   existing control flow graph. *)

let translate_instruction (instruction : ERTL.instruction) : LTL.instruction =
  match instruction with

    (* Allocating a stack frame consists in decrementing [$sp] by the
       size of the stack frame. Releasing a stack frame consists in
       incrementing [$sp] again. This is because the stack grows
       towards lower addresses. *)

  | ERTL.INewFrame l ->
      LTL.INewFrame l

  | ERTL.IDeleteFrame l ->
      LTL.IDeleteFrame l

  | ERTL.IGetHwReg (destr, sourcehwr, l) ->
      move (lookup destr) (Color sourcehwr) l

  | ERTL.ISetHwReg (desthwr, sourcer, l) ->
      move (Color desthwr) (lookup sourcer) l

  | ERTL.IGetStack (destr, slot, l) ->
      let desthwr, l = write destr l in
      LTL.IGetStack (desthwr, translate_slot slot, l)

  | ERTL.ISetStack (slot, sourcer, l) ->
      read1 sourcer (fun sourcehwr ->
	LTL.ISetStack (translate_slot slot, sourcehwr, l)
      )

    (* [IConst] instructions are translated in a straightforware way,
       except for [li $zero, 0], which is eliminated altogether. *)

  | ERTL.IConst (r, i, l) ->
      let hwr, l = write r l in
      if MIPS.equal hwr MIPS.zero then begin
	assert (i = 0l);
	LTL.IGoto l
      end
      else
	LTL.IConst (hwr, i, l)

    (* Special case for move instructions -- fewer temporary hardware
       registers are needed than in the general case. *)

  | ERTL.IUnOp (UOpAddi 0l, destr, sourcer, l) ->
      move (lookup destr) (lookup sourcer) l

  | ERTL.IUnOp (op, destr, sourcer, l) ->
      read1 sourcer (fun sourcehwr ->
	let desthwr, l = write destr l in
	LTL.IUnOp (op, desthwr, sourcehwr, l)
      )

  | ERTL.IBinOp (op, destr, sourcer1, sourcer2, l) ->
      read2 sourcer1 sourcer2 (fun sourcehwr1 sourcehwr2 ->
	let desthwr, l = write destr l in
	LTL.IBinOp (op, desthwr, sourcehwr1, sourcehwr2, l)
      )

  | ERTL.ICall (callee, _, l) ->
      LTL.ICall (callee, l)

  | ERTL.ITailCall (callee, _) ->
      LTL.ITailCall callee

  | ERTL.ILoad (destr, addressr, offset, l) ->
      read1 addressr (fun addresshwr ->
	let desthwr, l = write destr l in
	LTL.ILoad (desthwr, addresshwr, offset, l)
      )

  | ERTL.IStore (addressr, offset, valuer, l) ->
      read2 addressr valuer (fun addresshwr valuehwr ->
	LTL.IStore (addresshwr, offset, valuehwr, l)
      )

  | ERTL.IGetGlobal (destr, offset, l) ->
      let desthwr, l = write destr l in
      LTL.ILoad (desthwr, MIPS.gp, offset, l)

  | ERTL.ISetGlobal (offset, valuer, l) ->
      read1 valuer (fun valuehwr ->
	LTL.IStore (MIPS.gp, offset, valuehwr, l)
      )

  | ERTL.IGoto l ->
      LTL.IGoto l

  | ERTL.IUnBranch (cond, sourcer, truel, falsel) ->
      read1 sourcer (fun sourcehwr ->
	LTL.IUnBranch (cond, sourcehwr, truel, falsel)
      )

  | ERTL.IBinBranch (cond, sourcer1, sourcer2, truel, falsel) ->
      read2 sourcer1 sourcer2 (fun sourcehwr1 sourcehwr2 ->
	LTL.IBinBranch (cond, sourcehwr1, sourcehwr2, truel, falsel)
      )

  | ERTL.IReturn _ ->
      LTL.IReturn

(* ------------------------------------------------------------------------- *)

end

