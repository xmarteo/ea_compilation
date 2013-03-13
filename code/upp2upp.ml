open Integer
open MIPSOps
open UPP

(* ------------------------------------------------------------------------- *)

(* Determining whether an expression is pure (does not write to
   memory). This is a conservative approximation: we assume that every
   function call can write to memory. *)

let rec pure = function
  | EConst _
  | EGetVar _ 
  | EGetGlobal _ ->
      true
  | EUnOp (_, e)
  | ELoad (e, _) ->
      pure e
  | EBinOp (_, e1, e2) ->
      pure e1 && pure e2
  | EFunCall _ ->
      false

(* ------------------------------------------------------------------------- *)

(* Rules for integer addition and subtraction.

   Each of these rewrite rules causes the size of the term at hand to
   decrease strictly, except a number of rules that lift a unary
   addition out of a binary addition or subtraction. This ensures
   termination. (This argument is informal. A formal argument would
   have to be somewhat more detailed.)

   None of the rules interchanges the evaluation order of two
   arbitrary expressions, because, if these expressions have side
   effects, that might affect the semantics of the program. However,
   interchanging the evaluation order of [e1] and [e2] is fine if the
   side effects of [e1] cannot be observed by [e2], and vice versa.
   In order to simplify this criterion, we allow interchanging [e1]
   and [e2] when both are pure (do not write to memory).

   An expression is in canonical form when no rule is applicable to
   it. [mkadd] and [mksub] expect two expressions that are already in
   canonical form and return an expression in canonical form.

   There is no rule for rewriting [0 - e], because [MIPSOps] does not
   provide a unary negation operator. The [neg] instruction provided
   by the [MIPS] assembler is in fact a pseudo-instruction,
   implemented in terms of [sub], so there is no point in trying to
   exploit it.

   Here is a list of the rules. We write [e1 + e2] for a binary addition
   and [(i+) e] for a unary addition, that is, an instance of [UOpAddi].

   The order in which rules are listed matters: more specific rules are
   listed first, so as to ensure that they are preferred. For instance,
   the rule whose left-hand side is i1 + (i2+) e is listed before the
   rule whose left-hand side is i + e, because it is more specific and
   should be preferred.

     Left-hand side            Right-hand side
     -----------------------------------------

     i1 + i2                   (i1+i2)
     i1 + (i2+) e              (i1+i2) + e
     (i1+) e + i2              idem

     0 + e                     e
     e + 0                     idem

     i + e                     (i+) e
     e + i                     idem

     (i1+) e1 + (i2+) e2       ((i1+i2)+) (e1 + e2)
     e1 + (i+) e2              (i+) (e1 + e2)       (non-size-decreasing)
     (i+) e1 + e2              idem                 (non-size-decreasing)

     (0 - e1) + (0 - e2)       0 - (e1 + e2)
     e1 + (0 - e2)             e1 - e2
     (0 - e1) + e2             e2 - e1              if e1 and e2 are pure

     i1 - i2                   (i1-i2)
     i1 - (i2+) e              (i1-i2) - e
     (i1+) e - i2              (i1-i2) + e

     e - 0                     e

     i - e                     (i+) (0 - e)         if i <> 0 (non-size-decreasing) (actually size-increasing)
     e - i                     (-i+) e

     (i1+) e1 - (i2+) e2       (i1-i2+) (e1 - e2)
     e1 - (i+) e2              (-i+) (e1 - e2)      (non-size-decreasing)
     (i+) e1 - e2              (i+) (e1 - e2)       (non-size-decreasing)

     (0 - e1) - (0 - e2)       0 - (e1 - e2)
     e1 - (0 - e2)             e1 + e2
     (0 - e1) - e2             0 - (e1 + e2)
     0 - (e1 - e2)             e2 - e1              if e1 and e2 are pure

   One objective of these rules is to lift out and combine all of the
   integer constants that appear within an arbitrary addition and
   subtraction tree.

   One side-condition to several of the rules is that the unary
   operator (i+) can be used only if the constant i fits within
   16 bits. *)

let rec mkadd e1 e2 =
  match e1, e2 with

    (* Operator is applied to two constants. Evaluate. *)

  | EConst i1, EConst i2 ->
      EConst (i1 + i2)

    (* A constant combines with an [addi]. *)

  | EConst i1, EUnOp (UOpAddi i2, e)
  | EUnOp (UOpAddi i1, e), EConst i2 ->
      mkadd (EConst (i1 + i2)) e

    (* [0 + e] is [e]. [e + 0] is [e]. *)

  | EConst 0l, e
  | e, EConst 0l ->
      e

    (* Operator is applied to one constant that fits in 16
       bits. Introduce [addi]. *)

  | e, EConst i
  | EConst i, e
    when fits16 i ->
      EUnOp (UOpAddi i, e)

    (* Operator is applied to two [addi]'s. Lift them out and combine
       them into a single [addi]. Because the sum of [e1] and [e2]
       might again not be in canonical form, recursively invoke
       [mkadd] to rewrite it. *)

  | EUnOp (UOpAddi i1, e1), EUnOp (UOpAddi i2, e2)
    when fits16 (i1 + i2) ->
      EUnOp (UOpAddi (i1 + i2), mkadd e1 e2)

    (* Operator is applied to one [addi]. Lift it out. As noted above,
       this rule is size preserving, but cannot be applied ad
       infinitum. *)

  | e1, EUnOp (UOpAddi i, e2)
  | EUnOp (UOpAddi i, e1), e2 ->
      EUnOp (UOpAddi i, mkadd e1 e2)

    (* [(0 - e1) + (0 - e2)] is [0 - (e1 + e2)]. *)

  | EBinOp (OpSub, EConst 0l, e1), EBinOp (OpSub, EConst 0l, e2) ->
      EBinOp (OpSub, EConst 0l, mkadd e1 e2)

    (* [e1 + (0 - e2)] is [e1 - e2]. *)

  | e1, EBinOp (OpSub, EConst 0l, e2) ->
      mksub e1 e2

    (* [(0 - e1) + e2] is [e2 - e1] if [e1] and [e2] are pure. *)

  | EBinOp (OpSub, EConst 0l, e1), e2
    when pure e1 && pure e2 ->
      mksub e2 e1

    (* Default case. *)

  | e1, e2 ->
      EBinOp (OpAdd, e1, e2)

and mksub e1 e2 =
  match e1, e2 with

    (* Operator is applied to two constants. Evaluate. *)

  | EConst i1, EConst i2 ->
      EConst (i1 - i2)

    (* A constant combines with an [addi]. *)

  | EConst i1, EUnOp (UOpAddi i2, e) ->
      mksub (EConst (i1 - i2)) e

  | EUnOp (UOpAddi i1, e), EConst i2 ->
      mkadd (EConst (i1 - i2)) e

    (* [e - 0] is [e]. *)

  | e, EConst 0l ->
      e

    (* Operator is applied to one constant that fits in 16
       bits. Introduce [addi]. *)

  | EConst i, e
    when i <> 0l && fits16 i ->
      EUnOp (UOpAddi i, EBinOp (OpSub, EConst 0l, e))

  | e, EConst i
    when fits16 (-i) ->
      EUnOp (UOpAddi (-i), e)

    (* Operator is applied to two [addi]'s. Lift them out
       and combine them into a single [addi]. *)

  | EUnOp (UOpAddi i1, e1), EUnOp (UOpAddi i2, e2)
    when fits16 (i1 - i2) ->
      EUnOp (UOpAddi (i1 - i2), mksub e1 e2)

    (* Operator is applied to one [addi]. Lift it out. *)

  | e1, EUnOp (UOpAddi i, e2)
    when fits16 (-i) ->
      EUnOp (UOpAddi (-i), mksub e1 e2)

  | EUnOp (UOpAddi i, e1), e2 ->
      EUnOp (UOpAddi i, mksub e1 e2)

    (* [(0 - e1) - (0 - e2)] is [0 - (e1 - e2)]. *)

  | EBinOp (OpSub, EConst 0l, e1), EBinOp (OpSub, EConst 0l, e2) ->
      mkneg (mksub e1 e2)

    (* [e1 - (0 - e2)] is [e1 + e2]. *)

  | e1, EBinOp (OpSub, EConst 0l, e2) ->
      mkadd e1 e2

    (* [(0 - e1) - e2] is [0 - (e1 + e2)]. *)

  | EBinOp (OpSub, EConst 0l, e1), e2 ->
      mkneg (mkadd e1 e2)

    (* [0 - (e1 - e2)] is [e2 - e1] if [e1] and [e2] are pure. *)

  | EConst 0l, EBinOp (OpSub, e1, e2)
    when pure e1 && pure e2 ->
      mksub e2 e1

    (* Default case. *)

  | e1, e2 ->
      EBinOp (OpSub, e1, e2)

and mkneg e =
  mksub (EConst 0l) e

(* ------------------------------------------------------------------------- *)

(* Logical shift left. This operator is used below to implement
   multiplication by a power of two. *)

let mksll i1 = function

  (* [(e sll i1) sll i2] is [e sll (i1 + i2)]. *)

  | EUnOp (UOpSlli i2, e)
    when fits16 (i1 + i2) ->
      EUnOp (UOpSlli (i1 + i2), e)

    (* Default case. *)

  | e ->
      EUnOp (UOpSlli i1, e)

(* ------------------------------------------------------------------------- *)

(* Integer multiplication. *)

let rec mkmul e1 e2 =
  match e1, e2 with

    (* Operator is applied to two constants. Evaluate. *)

  | EConst i1, EConst i2 ->
      EConst (i1 * i2)

    (* [e * i] is [i * e]. This simplifies the statement of some of the rules
       below. *)

  | _, EConst _ ->
      mkmul e2 e1

    (* [0 * e] is [0] if [e] is pure. *)

  | EConst 0l, e
    when pure e ->
      EConst 0l

    (* [1 * e] is [e]. *)

  | EConst 1l, e ->
      e

    (* [-1 * e] is [0 - e]. *)

  | EConst (-1l), e ->
      mkneg e

    (* [i1 * ((i2+) e)] is [(i1*i2+) (i1 * e)]. This rule is not
       size-decreasing, but lifts an [addi] out of a
       multiplication. *)

  | EConst i1, EUnOp (UOpAddi i2, e)
    when fits16 (i1 * i2) ->
      EUnOp (UOpAddi (i1 * i2), mkmul (EConst i1) e)

    (* [(i1 * e1) * (i2 * e2)] is [(i1*i2) * (e1 * e2)]. *)

  | EBinOp (OpMul, EConst i1, e1), EBinOp (OpMul, EConst i2, e2) ->
      mkmul (EConst (i1 * i2)) (mkmul e1 e2)

    (* [i1 * (i2 * e)] is [(i1*i2) * e]. *)

  | EConst i1, EBinOp (OpMul, EConst i2, e) ->
      mkmul (EConst (i1 * i2)) e

    (* [2^i * e] is [e sll i]. *)

  | EConst i, e
    when is_power_of_two i ->
      mksll (log2 i) e

    (* [i1 * (e sll i2)] is [(i1*2^i2) * e]. *)

  | EConst i1, EUnOp (UOpSlli i2, e) ->
      mkmul (EConst (i1 * exp2 i2)) e

    (* Default case. *)

  | _, _ ->
      EBinOp (OpMul, e1, e2)

(* ------------------------------------------------------------------------- *)

(* Integer division. *)

let mkdiv e1 e2 =
  match e1, e2 with

    (* Operator is applied to two constants. Evaluate, unless [i2] is zero,
       because that would cause the compiler to fail. *)

  | EConst i1, EConst i2 when i2 <> 0l ->
      EConst (i1 / i2)

    (* One could invent more rules. For instance, one could exploit the
       properties of [0], [1], and [-1] with respect to division. One could
       notice that dividing by a power of [2] is equivalent to an arithmetic
       shift towards the right (that is, a shift operation that preserves the
       sign bit). One could also attempt to exploit the fact that [x / (y * z)]
       is equal to [x / y / z]. I did not try to write down every possible
       law. *)

  (* Default case. *)

  | _, _ ->
      EBinOp (OpDiv, e1, e2)

(* ------------------------------------------------------------------------- *)

(* Integer comparison. *)

let rec mkcmp op e1 e2 =
  match op, e1, e2 with

    (* Operator is applied to two constants. Evaluate. *)

  | _, EConst i1, EConst i2 ->
      EConst (InterpretMIPS.binop op i1 i2)

    (* Operator is [<] and its right-hand operand is a constant. Introduce
       [slti]. Don't do this if the constant is [0], though, as this might
       later preclude the use of a more compact [bltz] instruction. *)

  | OpLt, _, EConst i2
    when i2 <> 0l && fits16 i2 ->
      EUnOp (UOpSlti i2, e1)

    (* Operator is [<=] and its right-hand operand is a constant. Rewrite it
       into a use of [<], so as to be able to apply above optimization. Don't
       do this if the constant is [0], though, as this might later preclude
       the use of a more compact [blez] instruction. *)

  | OpLe, _, EConst i2
    when i2 <> 0l && i2 <> max_int (* overflow check *) ->
      mkcmp OpLt e1 (EConst (i2 + 1l))

    (* Operator is [>] or [>=] and its left-hand operand is a
       constant.  Reverse operator and operands so as to possibly
       benefit from the above optimizations. *)

  | OpGt, EConst _, _ ->
      mkcmp OpLt e2 e1

  | OpGe, EConst _, _ ->
      mkcmp OpLe e2 e1

    (* The optimizations above have symmetric cases that are not covered,
       because the MIPS does not offer an [sgti] instruction. The right thing
       to do, in order to produce efficient code, would probably be to reverse
       the comparison and introduce a Boolean negation: for instance, one
       would translate [3 < x] into [not (x >= 3)]. Unfortunately, our
       separation between expressions and conditions makes this transformation
       somewhat awkward, so it is not done for the moment. *)

    (* Default case. *)

  | _, _, _ ->
      EBinOp (op, e1, e2)

(* ------------------------------------------------------------------------- *)

(* Rewriting an arbitrary operator application. This code simply
   dispatches control to one of the functions defined above. *)

let mkunop op e =
  match op with
  | PP.UOpNeg ->
      mkneg e

let mkbinop op e1 e2 =
  match op with
  | OpAdd ->
      mkadd e1 e2
  | OpSub ->
      mksub e1 e2
  | OpMul ->
      mkmul e1 e2
  | OpDiv ->
      mkdiv e1 e2
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpEq
  | OpNe ->
      mkcmp op e1 e2

