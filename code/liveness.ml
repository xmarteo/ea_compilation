open ERTL

(* In the following, a ``variable'' means a pseudo-register or an
   allocatable hardware register. *)

(* These functions allow turning an [ERTL] control flow graph into an
   explicit graph, that is, making successor edges explicit. This is
   useful in itself and facilitates the computation of predecessor
   edges. *)

let instruction_successors (i : instruction) =
  match i with
  | IReturn _
  | ITailCall _ ->
      Label.Set.empty
  | INewFrame l
  | IDeleteFrame l
  | IGetHwReg (_, _, l)
  | ISetHwReg (_, _, l)
  | IGetStack (_, _, l)
  | ISetStack (_, _, l)
  | IGetGlobal (_, _, l)
  | ISetGlobal (_, _, l)
  | IConst (_, _, l)
  | IUnOp (_, _, _, l)
  | IBinOp (_, _, _, _, l)
  | ICall (_, _, l)
  | ILoad (_, _, _, l)
  | IStore (_, _, _, l)
  | INewArray l
  | IGoto l ->
      Label.Set.singleton l
  | IUnBranch (_, _, l1, l2)
  | IBinBranch (_, _, _, l1, l2) ->
      Label.Set.couple l1 l2

(* The analysis uses the lattice of sets of variables. The lattice's
   join operation is pointwise set union, which reflects the fact that
   a variable is deemed live at a program point if and only if it is
   live at any of the successors of that program point. *)

module L = struct

  (* A set of variable is represented as a pair of a set of
     pseudo-registers and a set of hardware registers. *)

  type t =
      Register.Set.t * MIPS.RegisterSet.t

  type property =
      t

  let bottom =
    Register.Set.empty, MIPS.RegisterSet.empty

  let psingleton r =
    Register.Set.singleton r, MIPS.RegisterSet.empty

  let hsingleton hwr =
    Register.Set.empty, MIPS.RegisterSet.singleton hwr

  let join (rset1, hwrset1) (rset2, hwrset2) =
    (Register.Set.union rset1 rset2, MIPS.RegisterSet.union hwrset1 hwrset2)

  let diff (rset1, hwrset1) (rset2, hwrset2) =
    (Register.Set.diff rset1 rset2, MIPS.RegisterSet.diff hwrset1 hwrset2)

  let equal (rset1, hwrset1) (rset2, hwrset2) =
    Register.Set.equal rset1 rset2 && MIPS.RegisterSet.equal hwrset1 hwrset2

  let is_maximal _ =
    false

end

module F = Fix.Make (Label.ImperativeMap) (L)

(* These are the sets of variables defined at (written by) an instruction. *)

let defined (i : instruction) : L.t =
  match i with
  | IGetHwReg (r, _, _)
  | IGetStack (r, _, _)
  | IConst (r, _, _)
  | IUnOp (_, r, _, _)
  | IBinOp (_, r, _, _, _)
  | ILoad (r, _, _, _)
  | IGetGlobal (r, _, _) ->
      L.psingleton r
  | INewArray _
  | ICall _ ->
      (* [ICall] potentially destroys all caller-save hardware registers. *)
      Register.Set.empty, MIPS.caller_saved
  | ISetHwReg (hwr, _, _) ->
      L.hsingleton hwr
  | INewFrame _
  | IDeleteFrame _
  | ISetStack _
  | IStore _
  | ISetGlobal _
  | IGoto _
  | IUnBranch _
  | IBinBranch _
  | IReturn _
  | ITailCall _ ->
      L.bottom

(* This is the set of variables used at (read by) an instruction. *)

let saved =
  MIPS.RegisterSet.add MIPS.ra MIPS.callee_saved

let used (i : instruction) : L.t =
  match i with
  | IGetHwReg (_, hwr, _) ->
      L.hsingleton hwr
  | INewFrame _
  | IDeleteFrame _
  | IGetStack _
  | IConst _
  | IGoto _
  | IGetGlobal _ ->
      L.bottom
  | ISetHwReg (_, r, _)
  | ISetStack (_, r, _)
  | IUnOp (_, _, r, _)
  | ILoad (_, r, _, _)
  | ISetGlobal (_, r, _)
  | IUnBranch (_, r, _, _) ->
      L.psingleton r
  | IBinOp (_, _, r1, r2, _)
  | IStore (r1, _, r2, _)
  | IBinBranch (_, r1, r2, _, _) ->
      Register.Set.couple r1 r2, MIPS.RegisterSet.empty
  | INewArray _ ->
      Register.Set.empty,
      MIPS.RegisterSet.of_list (Misc.prefix 1l MIPS.parameters)
  | ICall (_, nparams, _) ->
      (* [ICall] reads the hardware registers that are used to pass
	 parameters. So does [ITailCall]. *)
      Register.Set.empty,
      MIPS.RegisterSet.of_list (Misc.prefix nparams MIPS.parameters)
  | IReturn false ->
      (* [IReturn true] reads the result-value register [$v0], while [IReturn
	 false] does not. *)
      (* Both [IReturn] and [ITailCall] read [$ra] -- indeed, [IReturn] uses
	 it directly, while [ITailCall] transmits it to the callee. Both
	 [IReturn] and [ITailCall] read the callee-save registers -- indeed,
	 the caller expects to find meaningful values in these registers, so
	 they must be considered live. *)
      Register.Set.empty,
      saved
  | IReturn true ->
      Register.Set.empty,
      MIPS.RegisterSet.add MIPS.result saved
  | ITailCall (_, nparams) ->
      Register.Set.empty,
      MIPS.RegisterSet.union
	saved
        (MIPS.RegisterSet.of_list (Misc.prefix nparams MIPS.parameters))

(* An instruction is considered pure if it has no side effect, that is, if
   its only effect is to write a value to its destination variable.

   A pure instruction whose destination is dead after the instruction will
   be eliminated during the translation of [ERTL] to [LTL]. This is done by
   replacing the instruction with an [IGoto] instruction.

   [eliminable liveafter i] returns [Some l], where [l] is [i]'s single
   successor, if instruction [i] is eliminable. Otherwise, it returns
   [None]. The parameter [liveafter] is the set of variables that are live
   after the instruction. *)

let eliminable ((pliveafter, hliveafter) : L.t) (i : instruction) =
  match i with
  | IGetHwReg (r, _, l)
  | IGetStack (r, _, l)
  | IGetGlobal (r, _, l)
  | IConst (r, _, l)
  | IUnOp (_, r, _, l)
  | IBinOp (_, r, _, _, l)
  | ILoad (r, _, _, l) ->
      if Register.Set.mem r pliveafter then None else Some l
  | ISetHwReg (hwr, _, l) ->
      if MIPS.RegisterSet.mem hwr hliveafter then None else Some l
  | IReturn _
  | ITailCall _
  | INewFrame _
  | IDeleteFrame _
  | ISetStack _
  | ISetGlobal _
  | INewArray _ (* marteo has to check this *)
  | ICall _
  | IStore _
  | IGoto _
  | IUnBranch _
  | IBinBranch _ ->
      None

(* This is the abstract semantics of instructions. It defines the
   variables that are live before the instruction in terms of
   those that are live after the instruction. *)

(* The standard definition is: a variable is considered live
   before the instruction if either (1) it is used by the instruction,
   or (2) it is live after the instruction and not defined by the
   instruction.

   As an exception to this rule, if the instruction is eliminable,
   then a variable is considered live before the instruction
   if and only if it is live after the instruction. This anticipates
   on the instruction's elimination.

   This exception means that the source variables of a pure
   instruction need not be considered live if the instruction's result
   is unused. This allows a sequence of pure instructions whose end
   result is dead to be considered entirely dead.

   It is a simple, but not entirely trivial, exercise to check that
   this transfer function is monotone. *)

let instruction_semantics (i : instruction) (liveafter : L.t) : L.t =
  match eliminable liveafter i with
  | None ->
      L.join (L.diff liveafter (defined i)) (used i)
  | Some _ ->
      liveafter

(* A valuation is a function that maps a program point (a control flow
   graph label) to the set of variables that are live after that
   point. *)

type valuation =
    Label.t -> L.t

(* This is how we turn an [ERTL] procedure into a liveness analysis
   problem and solve it. *)

let analyze (proc : procedure) : valuation =

  (* Formulate the problem. Construct a system (recursive) equations
     that describe the live variables before and after each
     instruction. *)

  (* The following two functions, [livebefore] and [liveafter],
     define these equations. Both use an oracle, a valuation --
     also called [liveafter] -- which is supposed to tell us
     which variables are live after each label. *)

  (* The live variables before an instruction are computed, using the
     instruction's semantics, in terms of the live variables after the
     instruction -- which are given by the oracle. *)

  let livebefore label (liveafter : valuation) : L.t =
    let i : instruction = Label.Map.find label proc.graph in
    instruction_semantics i (liveafter label)
  in

  (* The live variables after an instruction are the union of the live
     variables before each of the instruction's successors. *)

  let liveafter label (liveafter : valuation) : L.t =
    let i : instruction = Label.Map.find label proc.graph in
    Label.Set.fold (fun successor accu ->
      L.join (livebefore successor liveafter) accu
    ) (instruction_successors i) L.bottom
  in

  (* Compute the least fixed point of these recursive equations. *)

  F.lfp liveafter

