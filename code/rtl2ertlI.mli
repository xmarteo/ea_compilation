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

end) : sig

  (* [translate_call odestr callee actuals l] translates the [RTL]
     instruction [ICall (odestr, callee, actuals, l)] into an [ERTL]
     sequence of instructions that transfers control to [l]. *)

  val translate_call: Register.t option -> Primitive.callee ->
                      Register.t list -> Label.t -> Label.t

  (* [prologue] will be inserted in front of the existing code. It
     should transfer control to the procedure's original entry point
     [entry]. *)

  val prologue: Label.t

  (* [epilogue] will be appended at the end of the existing code. It
     should end with an [IReturn] instruction. *)

  val epilogue: Label.t

  (* [translate_tail_call callee actuals] translates the [RTL]
     instruction [ITailCall (callee, actuals)] into an [ERTL]
     sequence of instructions. *)

  val translate_tail_call: Primitive.callee ->
                           Register.t list -> Label.t

end

