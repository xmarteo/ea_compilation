(* This module performs common subexpression elimination (CSE).
   It transforms an [RTL] program into another [RTL] program. *)

open MIPSOps
open RTL

(* ---------------------------------------------------------------------------- *)

(* Join points. *)

(* A control flow graph label is a join point if it has more than one
   predecessor or if it is the control flow graph's entry label.

   The control flow graph's exit label may or may not be a join point
   according to this definition; this is irrelevant. *)

let instruction_successors (i : instruction) =
  match i with
  | ITailCall _ ->
      Label.Set.empty
  | IGetGlobal (_, _, l)
  | ISetGlobal (_, _, l)
  | IConst (_, _, l)
  | IUnOp (_, _, _, l)
  | IBinOp (_, _, _, _, l)
  | ICall (_, _, _, l)
  | ILoad (_, _, _, l)
  | IStore (_, _, _, l)
  | INewArray (_, _, l)
  | IGoto l ->
      Label.Set.singleton l
  | IUnBranch (_, _, l1, l2)
  | IBinBranch (_, _, _, l1, l2) ->
      Label.Set.couple l1 l2

let successors graph : Label.SetMap.t =
  Label.Map.map instruction_successors graph

let predecessors graph =
  Label.SetMap.reverse (successors graph)

let join_points entry graph =
  Label.Map.fold (fun label predecessors points ->
    if Label.Set.cardinal predecessors > 1 then
      Label.Set.add label points
    else
      points
  ) (predecessors graph) (Label.Set.singleton entry)

(* ---------------------------------------------------------------------------- *)

(* Symbolic expressions. *)

(* Symbolic expressions are built on top of symbolic variables that stand for
   unknown values. These symbolic variables are just atoms of a new kind. *)

module SymVar : AtomSig.S = Atom

(* Symbolic expressions are built on top of symbolic variables using
   the target processor's operators. We also have two constructors,
   [ELoad] and [EGlobal], that reflect the contents of a heap location
   and of a global variable, respectively. *)

type expression =
  | EVar of SymVar.t
  | EConst of int32
  | EUnOp of unop * expression
  | EBinOp of binop * expression * expression
  | ELoad of expression * offset
  | EGetGlobal of offset

(* [prune symvar p e] replaces every sub-expression of [e] that
   satisfies predicate [p] with a fresh symbolic variable. It is used
   to suppress [ELoad] and [EGetGlobal] expressions when the memory
   location or global variable that they reflect is modified. The
   function [symvar] is used to generate fresh symbolic variables. *)

let rec prune symvar p e =
  if p e then
    EVar (symvar())
  else
    match e with
    | EVar _
    | EConst _
    | EGetGlobal _ ->
	e
    | EUnOp (op, e1) ->
	EUnOp (op, prune symvar p e1)
    | EBinOp (op, e1, e2) ->
	EBinOp (op, prune symvar p e1, prune symvar p e2)
    | ELoad (e, ofs) ->
	ELoad (prune symvar p e, ofs)

(* Comparison of symbolic expressions is syntactic. One could
   incorporate axioms such as the commutativity and associativity of
   certain operators, etc. However, since the translation of [PP] to
   [UPP] has already normalized expressions with respect to these
   rules, doing so would probably not be useful. *)

let rec compare_expressions e1 e2 =
  match e1, e2 with
  | EVar v1, EVar v2 ->
      SymVar.compare v1 v2
  | EConst i1, EConst i2 ->
      Int32.compare i1 i2
  | EUnOp (op1, e1), EUnOp (op2, e2) ->
      let r = compare op1 op2 in
      if r = 0 then
	compare_expressions e1 e2
      else
	r
  | EBinOp (op1, e1, f1), EBinOp (op2, e2, f2) ->
      let r = compare op1 op2 in
      if r = 0 then
	let s = compare_expressions e1 e2 in
	if s = 0 then
	  compare_expressions f1 f2
	else
	  s
      else
	r
  | ELoad (e1, o1), ELoad (e2, o2) ->
      let r = compare o1 o2 in
      if r = 0 then
	compare_expressions e1 e2
      else
	r
  | EGetGlobal o1, EGetGlobal o2 ->
      Int32.compare o1 o2
  | _, _ ->
      (* head constructors are distinct, standard [compare] will
	 return [1] or [-1]. *)
      compare e1 e2

(* ---------------------------------------------------------------------------- *)

(* Environments map pseudo-registers to symbolic expressions and,
   conversely, symbolic expressions to sets of pseudo-registers. *)

module Env : sig

  type t

  (* [init f rs] is an environment that maps every pseudo-register [r]
     in the set [rs] to the symbolic expression [f r]. *)

  val init: (Register.t -> expression) -> Register.Set.t -> t

  (* [map f env] is an environment that maps [r] to [f e] if [env]
     maps [r] to [e]. *)

  val map: (expression -> expression) -> t -> t

  (* [assign r e env] updates the environment [env] with a mapping
     of [r] to [e]. *)

  val assign: Register.t -> expression -> t -> t

  (* [forward r env] looks up the expression associated with [r]
     in [env]. [backward e env] looks up the pseudo-registers
     associated with [e] in [env]. *)

  val forward: Register.t -> t -> expression
  val backward: expression -> t -> Register.Set.t

end = struct

  module ExpMap = Map.Make (struct
    type t = expression
    let compare = compare_expressions
  end)

  module RegSetExpMap = SetMap.MakeHetero(Register.Set)(ExpMap)

  type t = {
      forward: expression Register.Map.t;
      backward: RegSetExpMap.t
    }

  let forward r env =
    try
      Register.Map.find r env.forward
    with Not_found ->
      assert false

  let backward e env =
    RegSetExpMap.find e env.backward

  let assign r e env =
    let e0 = forward r env in
    {
      forward = Register.Map.add r e env.forward; (* overriding previous binding *)
      backward = RegSetExpMap.mkedge e r (RegSetExpMap.rmedge e0 r env.backward)
    }

  let init f rs =
    Register.Set.fold (fun r env ->
      let e = f r in
      {
        forward = Register.Map.add r e env.forward;
        backward = RegSetExpMap.mkedge e r env.backward
      }
    ) rs { forward = Register.Map.empty; backward = ExpMap.empty }

  let map f env =
    init (fun r -> f (forward r env)) (Register.Map.domain env.forward)
       
end

(* ---------------------------------------------------------------------------- *)

(* Symbolic execution. *)

module Exec (X : sig

  (* [symvar()] produces a fresh symbolic variable. *)

  val symvar: unit -> SymVar.t

  (* [proc] is the source procedure. *)

  val proc: procedure

  (* [join_points] is the set of join points of the control
     flow graph, as defined above. *)

  val join_points: Label.Set.t

  (* [generate label i] generates instruction [i] at label [l]
     in the control flow graph of the translated procedure. *)

   val generate: Label.t -> instruction -> unit

end) = struct

  open X

  (* [init] produces a fresh initial environment that maps every
     pseudo-register to a distinct symbolic variable. *)

  let init () =
    Env.init (fun _ -> EVar (symvar())) proc.locals

  (* [execute label env] performs symbolic execution at label [label] under
     environment [env]. At least one step of symbolic execution is
     performed, even if [label] is a join point, unless [label] is the
     control flow graph's exit label.

     Symbolic execution carries on sequentially along all control flow paths
     until it reaches join points or the exit label, where it stops. Its
     side effect is to generate instructions in the translated procedure's
     control flow graph. *)

  let rec execute label env =
    if not (Label.equal label proc.exit) then
      let i = Label.Map.find label proc.graph in
      match i with

	(* Assignments. Compute the symbolic result of the operation
	   and simulate the assignment using [assign]. As a special
	   case, we never replace an [IConst] instruction with a
	   [move] instruction, because this seldom saves any code and
	   makes register allocation more difficult.

	   By default, we assume that reading a single global variable
	   (or heap location) twice yields the same value twice. This
	   assumption is reflected in the existence of [ELoad] and
	   [EGetGlobal] symbolic expressions. This is safe because we
	   forget about all such expressions when we pass an [IStore]
	   or [ISetGlobal] instruction; see below. *)

      | IConst (r, k, next) ->
	  generate label i;
	  jump next (Env.assign r (EConst k) env)
      | IUnOp (op, r, r1, next) ->
	  assign label i r (EUnOp (op, Env.forward r1 env)) next env
      | IBinOp (op, r, r1, r2, next) ->
	  assign label i r (EBinOp (op, Env.forward r1 env, Env.forward r2 env)) next env
      | ILoad (r, s, o, next) ->
	  assign label i r (ELoad (Env.forward s env, o)) next env
      | IGetGlobal (r, o, next) ->
	  assign label i r (EGetGlobal o) next env

	(* As announced above, instructions that write a heap location
	   or global variable invalidate the meaning of some [ELoad]
	   and [EGetGlobal] symbolic expressions. Thus, we prune all
	   such expressions.

	   More precisely, a store invalidates all [ELoad]
	   expressions. A write to a global variable invalidates
	   [EGetGlobal] expressions for that particular variable
	   only. *)

      | IStore (_, _, _, next) ->
	  let invalidated = function
	    | ELoad _ ->
		true
	    | _ ->
		false
	  in
	  let env = Env.map (prune symvar invalidated) env in
	  generate label i;
	  jump next env
	  
      (* I have no idea what I'm doing *)
      
      | INewArray (_, _, next) ->
	  jump next env (* This probably is the local equivalent of "do nothing" *)

      | ISetGlobal (o1, _, next) ->
	  let invalidated = function
	    | EGetGlobal o2 ->
		o1 = o2
	    | _ ->
		false
	  in
	  let env = Env.map (prune symvar invalidated) env in
	  generate label i;
	  jump next env

	(* Instructions that have no effect on pseudo-registers. *)

      | IGoto next ->
	  generate label i;
	  jump next env
      | IUnBranch (_, _, l1, l2)
      | IBinBranch (_, _, _, l1, l2) ->
	  generate label i;
	  jump l1 env;
	  jump l2 env

	(* Function or procedure call. It would be correct to view
	   this as an operation that invalidates all heap locations
	   and global variables and (for function calls) assigns an
	   to the destination register.

	   However, if we did so, then we would produce code where the
	   value of a common sub-expression can be recorded across a
	   call. That would put more pressure on the register
	   allocator, typically leading it to use more callee-save
	   registers and a larger stack frame. If the computation
	   whose value is being recorded is not expensive, that might
	   be counter-productive.

	   Instead, we start afresh at [next] by forgetting all we
	   know. That is, we drop [env] and continue with a fresh
	   environment. This means that fewer common sub-expressions
	   will be found, but register allocation will be easier. *)

      | ICall (_, _, _, next) ->
	  generate label i;
	  jump next (init())

	(* Tail call. Translate the instruction and stop, since tail
	   calls do not return. *)

      | ITailCall _ ->
	  generate label i

  (* [assign label i r e next env] simulates the execution of
     instruction [i] at label [label] under environment [env], assuming
     that its effect is to assign the symbolic expression [e] to
     pseudo-register [r] and then to transfer control to label
     [next]. *)

  and assign label i r e next env =

    (* Check if some pseudo-register [s] already holds the symbolic
       expression [e] at this point. If so, replace instruction
       [i] with a [move] instruction from [s] to [r]. Otherwise,
       keep [i]. *)

      generate label (
	try
	  let s = Register.Set.choose (Env.backward e env) in
	  IUnOp (UOpAddi 0l, r, s, next)
	with Not_found ->
	  i
      );

      (* Update the environment to reflect the new value of [r]
	 and proceed with execution at [next]. *)

      jump next (Env.assign r e env)

  (* [jump label env] continues symbolic execution at label [label] under
     environment [env]. If [label] is a join point, symbolic execution
     stops. Otherwise, it continues at [label]. *)

  and jump label env =
    if not (Label.Set.mem label join_points) then
      execute label env

  (* [start label] initiates symbolic execution at [label] with a
     fresh initial environment. *)

  let start label =
    execute label (init())

end

(* ---------------------------------------------------------------------------- *)

(* Translating procedures. *)

let translate_procedure proc =

  let module P = struct

    let proc =
      proc

    (* Define a universe of symbolic variables and a function
       that draws fresh variables out of it. *)

    let universe =
      SymVar.new_universe "?"

    let symvar () =
      SymVar.fresh universe

    (* Compute the procedure's join points. *)

    let join_points =
      join_points proc.entry proc.graph

    (* Define a function that generates instructions in the
       new control flow graph. *)

    let graph =
      ref Label.Map.empty

    let generate label i =
      assert (not (Label.Map.mem label !graph));
      graph := Label.Map.add label i !graph

  end in

  let module E = Exec(P) in

  (* Perform symbolic execution, starting at every join point. *)

  Label.Set.iter E.start P.join_points;

  (* Construct a new procedure out of the new control flow graph. *)

  { proc with
    graph = !P.graph
  }

(* ---------------------------------------------------------------------------- *)

(* Translating programs. *)

let translate_program prog =
  { prog with
    defs = StringMap.map translate_procedure prog.defs
  }

