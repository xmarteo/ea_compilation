module Make (Env : sig

  (* [lookup x] returns the pseudo-register that holds the local
     variable [x]. *)

  val lookup: string -> Register.t

  (* [allocate()] returns a fresh pseudo-register. *)

  val allocate: unit -> Register.t

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: RTL.instruction -> Label.t

  (* [loop target] creates a fresh instruction label [label], which it
     associates with an unconditional branch instruction whose target
     is [target label]. It returns [target label]. *)

  val loop: (Label.t -> Label.t) -> Label.t

  (* [is_exit label] tells whether the label [label] is the exit label
     of the current procedure or function. This can be used to determine
     which calls are tail calls. *)

  val is_exit: Label.t -> bool

  (* [result] is [None] if this is a procedure, and [Some f] if this
     a function named [f]. *)

  val result: string option

end) = struct

open Integer
open MIPSOps
open Env

(* ------------------------------------------------------------------------- *)

(* Translating expressions. *)

(* [pick_destination e] decides which pseudo-register will hold the
   result of evaluating expression [e]. *)

(* It would be correct, in all cases, to allocate a fresh pseudo-register. We
   would then cross our fingers and hope that the register allocation phase
   will make a good choice. In practice, we seem to get slightly better code
   by making this decision here.

   However, when [e] is a local variable access, the result of the
   expression is already held within an existing pseudo-register [r],
   because local variables are held in pseudo-registers. In that case,
   we can save a move instruction by choosing [r] as the destination
   for this expression.

   It is nontrivial that this optimization is correct. One must check
   that the pseudo-register [r], which holds the local variable, will
   not be modified between the time when the variable is read and the
   time when the result of the expression is needed. This works here
   because Pseudo-Pascal expressions cannot modify local variables. *)

let pick_destination e =
  match e with
  | UPP.EGetVar x ->
      lookup x
  | _ ->
      allocate()

(* [translate_expression destr e destl] generates new instructions
   whose effect is to evaluate the expression [e], to place its value
   in the pseudo-register [destr], and to transfer control to the
   label [destl]. It returns the entry label of the newly generated
   instructions. *)

let rec translate_expression
   (destr : Register.t)
   (e : UPP.expression)
   (destl : Label.t)
    : Label.t =

  match e with

    (* Constants. Generate a [IConst] instruction directly into
       the destination register, and branch to the destination
       label. *)

  | UPP.EConst i ->
      generate (RTL.IConst (destr, i, destl))

    (* Local variable access. Copy the contents of the register that
       holds the variable into the destination register. On the MIPS,
       data movement is implemented using [addi 0]. *)

  | UPP.EGetVar x ->
      let sourcer = lookup x in
      if Register.equal sourcer destr then
	destl
      else
	generate (RTL.IUnOp (UOpAddi 0l, destr, sourcer, destl))

    (* Global variable access. *)

  | UPP.EGetGlobal offset ->
      generate (RTL.IGetGlobal (destr, offset, destl))

    (* Unary operator applications. First, evaluate the expression
       into a temporary register; then, generate a unary operator
       instruction into the destination register. *)

    (* One might note that it would be correct to use [destr] instead of a
       temporary pseudo-register. However, this would not be a good idea,
       because it would force the register allocation phase to use the same
       physical register as the source and destination of this [op]
       instruction. (The register allocator is naive: it never splits a
       pseudo-register.) Here, we let the register allocator map [temporary]
       and [destr] to two distinct physical registers, if it so desires: there
       is more freedom. *)

  | UPP.EUnOp (op, e) ->
      let temporary = pick_destination e in
      translate_expression temporary e (
	generate (RTL.IUnOp (op, destr, temporary, destl))
      )

  (* Binary operator applications. Analogous to the unary case.
     One must be careful to evaluate [e1] first and [e2] next. *)

  | UPP.EBinOp (op, e1, e2) ->
      let temporary1 = pick_destination e1
      and temporary2 = pick_destination e2 in
      translate_expression temporary1 e1 (
      translate_expression temporary2 e2 (
      generate (RTL.IBinOp (op, destr, temporary1, temporary2, destl))
      ))

    (* Function calls. *)

  | UPP.EFunCall (callee, actuals) ->
      translate_call (Some destr) callee actuals destl

    (* Memory reads. This is much like a unary operator. *)

  | UPP.ELoad (e, offset) ->
      let temporary = pick_destination e in
      translate_expression temporary e (
      generate (RTL.ILoad (destr, temporary, offset, destl))
      )

(* Translating function and procedure calls. This is analogous to the
   case of binary operator applications above, except the number of
   arguments is variable. The destination register is optional --
   present for functions, absent for procedures. *)

and translate_call 
   (odestr : Register.t option)
   (callee : Primitive.callee)
   (actuals : UPP.expression list)
   (destl : Label.t)
   : Label.t =

  let temporaries = List.map pick_destination actuals in
  List.fold_right2 translate_expression temporaries actuals (
  generate (RTL.ICall (odestr, callee, temporaries, destl))
  )

(* Translating tail calls. This is analogous to the case of ordinary calls
   above, except a tail call does not return, so there is no destination
   register and no successor label. *)

and translate_tail_call 
   (callee : Primitive.callee)
   (actuals : UPP.expression list)
   : Label.t =

  let temporaries = List.map pick_destination actuals in
  List.fold_right2 translate_expression temporaries actuals (
  generate (RTL.ITailCall (callee, temporaries))
  )

(* ------------------------------------------------------------------------- *)

(* [mkunbranch e uncon truel falsel] translates the expression [e],
   writing its value to a temporary register; then, it issues a unary
   branch instruction on that register, whose condition is [uncon],
   and whose target labels are [truel] and [falsel]. *)

let mkunbranch e uncon truel falsel =
  let temporary = pick_destination e in
  translate_expression temporary e (
  generate (RTL.IUnBranch (uncon, temporary, truel, falsel))
  )

(* [mkbinbranch e1 e2 bincon truel falsel] translates the expressions
   [e1] and [e2], writing their values to temporary registers; then,
   it issues a binary branch instruction on those registers, whose
   condition is [bincon], and whose target labels are [truel] and
   [falsel]. *)

let mkbinbranch e1 e2 bincon truel falsel =
  let temporary1 = pick_destination e1
  and temporary2 = pick_destination e2 in
  translate_expression temporary1 e1 (
  translate_expression temporary2 e2 (
  generate (RTL.IBinBranch (bincon, temporary1, temporary2, truel, falsel))
  ))

(* Translating conditions. *)

(* [translate_condition c truel falsel] generates new [RTL]
   instructions whose effect is to evaluate the condition [c] and to
   transfer control to one of the labels [truel] and [falsel],
   depending on the condition's value. It returns the entry label of
   the newly generated instructions. *)

let rec translate_condition
   (c : UPP.condition)
   (truel : Label.t)
   (falsel : Label.t)
    : Label.t =

  match c with

    (* The general compilation scheme for Boolean expressions, which
       follows, evaluates the expression into a temporary register,
       then performs a conditional branch, depending on whether the
       register is [0] or [1]. Yet, some special cases of Boolean
       expressions can be translated more efficiently. That is, if the
       expression is an application of a comparison operator, and if
       it can be mapped into a branch condition (consult the types
       [RTL.uncon] and [RTL.bincon]), then we do not need a temporary
       register: we can issue a conditional branch instruction that
       directly tests the desired condition. *)

    (* First, here are the cases where we can generate a unary
       conditional branch instruction. *)

  | UPP.CExpression (UPP.EBinOp (OpGe, e, UPP.EConst 0l))
  | UPP.CExpression (UPP.EBinOp (OpGt, e, UPP.EConst (-1l)))
  | UPP.CExpression (UPP.EBinOp (OpLe, UPP.EConst 0l, e))
  | UPP.CExpression (UPP.EBinOp (OpLt, UPP.EConst (-1l), e)) ->
      mkunbranch e UConGez truel falsel

  | UPP.CExpression (UPP.EBinOp (OpGt, e, UPP.EConst 0l))
  | UPP.CExpression (UPP.EBinOp (OpGe, e, UPP.EConst 1l))
  | UPP.CExpression (UPP.EBinOp (OpLt, UPP.EConst 0l, e))
  | UPP.CExpression (UPP.EBinOp (OpLe, UPP.EConst 1l, e)) ->
      mkunbranch e UConGtz truel falsel

  | UPP.CExpression (UPP.EBinOp (OpLe, e, UPP.EConst 0l))
  | UPP.CExpression (UPP.EBinOp (OpLt, e, UPP.EConst 1l))
  | UPP.CExpression (UPP.EBinOp (OpGe, UPP.EConst 0l, e))
  | UPP.CExpression (UPP.EBinOp (OpGt, UPP.EConst 1l, e)) ->
      mkunbranch e UConLez truel falsel

  | UPP.CExpression (UPP.EBinOp (OpLt, e, UPP.EConst 0l))
  | UPP.CExpression (UPP.EBinOp (OpLe, e, UPP.EConst (-1l)))
  | UPP.CExpression (UPP.EBinOp (OpGt, UPP.EConst 0l, e))
  | UPP.CExpression (UPP.EBinOp (OpGe, UPP.EConst (-1l), e)) ->
      mkunbranch e UConLtz truel falsel

    (* Next, here are the cases where we can generate a binary
       conditional branch instruction. *)

  | UPP.CExpression (UPP.EBinOp (OpEq, e1, e2)) ->
      mkbinbranch e1 e2 ConEq truel falsel

  | UPP.CExpression (UPP.EBinOp (OpNe, e1, e2)) ->
      mkbinbranch e1 e2 ConNe truel falsel

    (* Last, here is the general case for Boolean expressions. The
       expression [e] can evaluate only to [true] or [false], which we
       have represented as [1] and [0], respectively. We evaluate [e]
       into a register and test its value using a unary conditional
       branch. *)

  | UPP.CExpression e ->
      mkunbranch e UConGtz truel falsel

    (* Boolean negation. This is implemented, without generating any
       code, simply by exchanging the two destination labels. *)

  | UPP.CNot c ->
      translate_condition c falsel truel

    (* Boolean conjunction. The semantics of the conjunction operator
       is non-strict, that is, the second condition is not evaluated
       at all if the first condition evaluates to [false]. *)

  | UPP.CAnd (c1, c2) ->
      translate_condition c1
	(translate_condition c2 truel falsel)
	falsel

    (* Boolean disjunction. The semantics of the disjunction operator
       is non-strict, that is, the second condition is not evaluated
       at all if the first condition evaluates to [true]. *)

  | UPP.COr (c1, c2) ->
      translate_condition c1
	truel
	(translate_condition c2 truel falsel)

(* ------------------------------------------------------------------------- *)

(* Translating instructions. *)

(* [translate_instruction i destl] generates new [RTL] instructions
   whose effect is to execute the [UPP] instruction [i] and to
   transfer control to the destination label [destl]. It returns the
   entry label of the newly generated instructions. *)

let rec translate_instruction
   (i : UPP.instruction)
   (destl : Label.t)
    : Label.t =

  match i with

    (* Tail calls to procedures. *)

  | UPP.IProcCall (callee, actuals)
      when is_exit destl && result = None ->

      (* If [destl] is the exit label, and if this is a procedure, then
	 this is a tail call. Otherwise, it is an ordinary call. *)

      (* There cannot be a tail call from a function f to a procedure g,
	 because, after g is finished, f still needs to return a result
	 to its caller. *)
      
      translate_tail_call callee actuals

    (* Tail calls to functions. These take the form [f := g(...);], with
       the following condition: if this is a function, then [f] must be
       its result variable; if this is a procedure, [f] can be any local
       variable. *)

    (* That is, a tail call from a procedure to a function is permitted;
       the result of the function is just dropped. A tail call from a
       function f to a function g is permitted only if the result of g
       is transmitted, that is, if it becomes the result of f. *)

  | UPP.ISetVar (f, UPP.EFunCall (callee, actuals))
      when is_exit destl && (result = None || result = Some f) ->

      translate_tail_call callee actuals

    (* Procedure calls. *)

  | UPP.IProcCall (callee, actuals) ->
      translate_call None callee actuals destl

    (* Local variable update. *)

    (* We evaluate [e] directly into the register that holds the variable. *)

  | UPP.ISetVar (x, e) ->
      let destr = Env.lookup x in
      translate_expression destr e destl

    (* Global variable update. *)

  | UPP.ISetGlobal (offset, e) ->
      let temporary = pick_destination e in
      translate_expression temporary e (
        generate (RTL.ISetGlobal (offset, temporary, destl))
      )

    (* Memory write. This is translated to the corresponding [RTL]
       instruction. *)

  | UPP.IStore (eaddress, offset, evalue) ->
      let address = pick_destination eaddress
      and value = pick_destination evalue in
      translate_expression address eaddress (
      translate_expression value evalue (
      generate (RTL.IStore (address, offset, value, destl))
      ))

    (* Sequence. This is translated by chaining the destination
       labels. *)

  | UPP.ISeq instructions ->
      List.fold_right translate_instruction instructions destl

    (* Conditional. *)

    (* Observe how the destination label [destl] is duplicated, so that
       both branches of the [if] construct meet again after their execution
       is over. *)

  | UPP.IIf (c, i1, i2) ->
      translate_condition c
	(translate_instruction i1 destl)
	(translate_instruction i2 destl)

    (* Loop. *)

    (* We first transfer control to a fresh label, called [entry],
       which represents the loop's entry point. At that point, we test the
       condition [c]. If it holds, we execute the instruction [i] and
       transfer control back to [entry]. Otherwise, we exit the loop by
       transferring control to our destination label [destl]. *)

  | UPP.IWhile (c, i) ->
      loop (fun entry ->
	translate_condition c
	  (translate_instruction i entry)
	  destl
      )

(* ------------------------------------------------------------------------- *)

end

