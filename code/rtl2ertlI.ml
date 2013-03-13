(* This functor implements the central part of the translation of
   [RTL] to [ERTL]. It is called once for each procedure or
   function. It defines the translation of instructions as well as the
   prologue and epilogue that should be added to the procedure. *)

module Make (Env : sig

  (* [allocate()] returns a fresh pseudo-register. *)

  val allocate: unit -> Register.t

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: ERTL.instruction -> Label.t

  (* [formals] is a list of the procedure's formal arguments. *)

  val formals: Register.t list

  (* [entry] is the procedure's original entry point. *)

  val entry: Label.t

  (* [result] tells whether this is a procedure or a function and, in
     the latter case, which pseudo-register holds the function's
     result when the exit label is reached. *)

  val result: Register.t option

  (* [is_self callee] determines whether [callee] refers to the
     current procedure or function. This can be used to recognize
     tail calls to self and turn them into jumps. *)

  val is_self: Primitive.callee -> bool

end) = struct

  open Integer
  open MIPSOps
  open Env

  (* ----------------------------------------------------------------------- *)

  (* Define functions that generate instructions for moving data
     between hardware registers and pseudo-registers. *)

  let sethwreg (desthwr, sourcer) l =
    generate (ERTL.ISetHwReg (desthwr, sourcer, l))

  let sethwregs moves l =
    List.fold_right sethwreg moves l

  let osethwreg (desthwr, osourcer) l =
    Option.fold (fun sourcer l -> sethwreg (desthwr, sourcer) l) osourcer l

  let gethwreg (destr, sourcehwr) l =
    generate (ERTL.IGetHwReg (destr, sourcehwr, l))

  let gethwregs moves l =
    List.fold_right gethwreg moves l

  let ogethwreg (odestr, sourcehwr) l =
    Option.fold (fun destr l -> gethwreg (destr, sourcehwr) l) odestr l

  (* ----------------------------------------------------------------------- *)

  (* Define functions that generate instructions for moving data
     between stack slots and pseudo-registers. These are used in order
     to access the formal parameters stored in the stack's incoming
     area and, when calling a function, to write its actual parameters
     into the stack's outgoing area.

     [offsets rs] turns the list of pseudo-registers [rs] into a list
     of pairs of a pseudo-register and a stack offset. It is here that
     the offsets at which parameters are stored on the stack is
     decided. This function is used both in [setstackslots], for
     storing actual parameters, and in [getstackslots], for fetching
     formal parameters -- this ensures consistency between caller and
     callee. *)

  let offsets rs =
    let next : unit -> int32 = Misc.multiples MIPS.word in
    List.map (fun r -> r, next()) rs

  let setstackslots sourcers l =
    List.fold_right (fun (sourcer, offset) l ->
      generate (ERTL.ISetStack (ERTL.SlotOutgoing offset, sourcer, l))
    ) (offsets sourcers) l

  let getstackslots destrs l =
    List.fold_right (fun (destr, offset) l ->
      generate (ERTL.IGetStack (destr, ERTL.SlotIncoming offset, l))
    ) (offsets destrs) l

  (* ----------------------------------------------------------------------- *)

  (* [translate_call odestr callee actuals l] translates the [RTL]
     instruction [ICall (odestr, callee, actuals, l)] into an [ERTL]
     sequence of instructions that transfers control to [l]. *)

  (* Before the call, we copy the actual parameters into their
     designated position, as dictated by the calling convention. The
     first few parameters are passed in registers, the rest on the
     stack. For function calls only, after the call, we copy the
     result from its designated position to its desired location. *)

  let translate_call odestr callee actuals l =
    sethwregs (Misc.combine MIPS.parameters actuals) (
      setstackslots (Misc.subtract actuals MIPS.parameters) (
	generate (ERTL.ICall (
	  callee,
	  Misc.length actuals,
	  ogethwreg (odestr, MIPS.result) l
	))
      )
    )

  (* ----------------------------------------------------------------------- *)

  (* Each callee-save hardware register is saved into a distinct
     pseudo-register upon entry, and restored upon exit. Since
     pseudo-registers are preserved across function calls, this
     ensures that the value of the callee-save hardware registers is
     preserved.

     Although register [$ra] is not usually thought of as
     ``callee-save'', its value must also be preserved, because it is
     needed by the final [IReturn] instruction.

     During register allocation, the pseudo-register that is used to
     preserve a callee-save hardware register will be either assigned
     a hardware register or spilled. With luck, it will be assigned
     the very hardware register that it corresponds to, so that both
     [move] instructions will disappear. (Of course, this is not just
     luck. The register allocator will try to make this happen.) *)

  let preserved =
    MIPS.RegisterSet.add MIPS.ra MIPS.callee_saved

  let moves =
    MIPS.RegisterSet.fold (fun hwr moves ->
      (allocate(), hwr) :: moves
    ) preserved []

  (* ----------------------------------------------------------------------- *)

  (* Define the prologue that will be inserted in front of the
     existing code. The prologue allocates the procedure's frame,
     saves the callee-save hardware registers, and fetches the formal
     parameters from their initial locations in hardware registers or
     on the stack. *)

  let prologue =
    generate (ERTL.INewFrame (
      gethwregs moves (
	gethwregs (Misc.combine formals MIPS.parameters) (
	  getstackslots (Misc.subtract formals MIPS.parameters)
	    entry
	)
      )
    ))

  (* ----------------------------------------------------------------------- *)

  (* The cleanup sequence restores the callee-save hardware registers and
     deletes the stack frame. *)

  let cleanup l =
    sethwregs (Misc.mirror moves) (
      generate (ERTL.IDeleteFrame l)
    )

  (* This is the epilogue. If this is a function, as opposed to a procedure,
     the epilogue first copies the return value to the designated hardware
     register. Then, the epilogue performs the cleanup sequence. The epilogue
     ends with an [IReturn] instruction, so as to return control to the
     caller. This instruction carries the flag [true] if this is a function,
     [false] if this is a procedure. *)

  let epilogue =
    osethwreg (MIPS.result, result) (
      cleanup (
	generate (ERTL.IReturn (result <> None))
      )
    )

  (* ----------------------------------------------------------------------- *)

  (* An algorithm for simulating a parallel move. [destrs] and [sourcers] are
     lists of pseudo-registers, which must have the same length. [l] is a
     continuation label. *)

  let move (destr, sourcer) l =
    generate (ERTL.IUnOp (UOpAddi 0l, destr, sourcer, l))

  let rec parallel_move destrs sourcers l =
    match destrs, sourcers with
    | [], [] ->
	l
    | destr :: destrs, sourcer :: sourcers ->
	
	(* The destination register [destr] might occur in [sourcers]. Use
	   a temporary register, and perform two moves. *)

	(* This may seem naive, but is in fact not so bad, because the
	   register allocator will strive to make the first move, or the
	   second move, or both, disappear. For instance, if [destr] does not
	   occur in the list [sourcers], then [destr] is dead after the first
	   move, so [destr] and [temporary] do not interfere: they can be
	   coalesced. The last move disappears, and we end up with a
	   left-to-right sequential move. In a symmetric situation, the result
	   of register allocation could be a right-to-left sequential move.
	   In other situations, more complex results are possible. *)

	let temporary = allocate() in
	move (temporary, sourcer) (
	  parallel_move destrs sourcers (
	    move (destr, temporary) l
	  )
	)

    | _ ->
	assert false (* the two lists must have the same length *)

  (* ----------------------------------------------------------------------- *)

  (* [cleanup_and_jump callee nparams] generates a cleanup sequence that ends
     with a tail call to [callee]. *)

  let cleanup_and_jump (callee, nparams) =
    cleanup (
      generate (ERTL.ITailCall (callee, nparams))
    )

  (* We memoize this function, so that, if there are several tail calls to the
     same callee, the cleanup sequences are shared. This is not essential for
     correctness, but decreases the code size. *)

  let cleanup_and_jump =
    Misc.memoize cleanup_and_jump

  (* [translate_tail_call callee actuals] translates the [RTL] instruction
     [ITailCall (callee, actuals)] into an [ERTL] sequence of instructions. *)

  let translate_tail_call callee actuals =

    (* Determine whether this is a self-call. *)

    if is_self callee then begin

      (* A self tail call is translated by copying the actual parameters into
	 the formal parameters and jumping to the procedure's entry point
	 (that is, after the prologue). The stack frame is not destroyed, it
	 is re-used. *)

      (* Note that some pseudo-registers could appear both in [actuals] and in
	 [formals]. As a result, we must be careful: generating a sequence of
	 moves could be incorrect, as a pseudo-register could be clobbered
	 before it is read. Instead, we need to generate a sequence of moves
	 that simulates a ``parallel move''. In general, this can require
	 auxiliary pseudo-registers. *)
      
      parallel_move formals actuals entry

    end

    (* Determine whether all parameters are passed in hardware registers. *)

    else if Misc.length actuals <= Misc.length MIPS.parameters then begin

      (* A general tail call is translated by placing the actual parameters in
	 the designated hardware registers (as in an ordinary call), then
	 performing a cleanup sequence that ends in a jump to the callee
	 instead of a normal return. That is, it ends with [ITailCall] instead
	 of [IReturn]. *)

      sethwregs (Misc.combine MIPS.parameters actuals) (
	cleanup_and_jump (callee, Misc.length actuals)
      )

    end
    else

      (* If some parameters must be passed on the stack, then implementing a
	 tail call becomes difficult. On the one hand, we cannot destroy the
	 caller's frame until we have read the actual parameters. On the other
	 hand, we cannot write the actual parameters into the outgoing stack
	 slots until we have destroyed the stack frame. One could imagine a
	 solution based on the use of temporary hardware registers, but let's
	 just not do it. *)

      (* We turn the tail call back to a normal call. This means that we must
	 perform a call, followed with a jump to the epilogue. *)

      (* If the caller is a function ([result] is [Some _]), then the callee
	 must be a function as well. In that case, [translate_call] generates
	 a move from [$v0] into the [result] pseudo-register, and the epilogue
	 begins with a move from [result] back into [$v0]. This may seem
	 inefficient, and could be avoided if desired, but, in practice, the
	 register allocator will probably implement [result] as [$v0], so
	 these moves will disappear. *)

      translate_call result callee actuals epilogue

end
