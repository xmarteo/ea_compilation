open Primitive

(* ------------------------------------------------------------------------- *)

(* [translate_instruction] translates an [LTL] instruction into a
   [LIN] instruction. *)

let translate_instruction = function

    (* Sequential instructions. *)

  | LTL.INewFrame _ ->
      LIN.INewFrame
  | LTL.IDeleteFrame _ ->
      LIN.IDeleteFrame
  | LTL.IGetStack (r, slot, _) ->
      LIN.IGetStack (r, slot)
  | LTL.ISetStack (slot, r, _) ->
      LIN.ISetStack (slot, r)
  | LTL.IConst (r, i, _) ->
      LIN.IConst (r, i)
  | LTL.IUnOp (op, r1, r2, _) ->
      LIN.IUnOp (op, r1, r2)
  | LTL.IBinOp (op, r, r1, r2, _) ->
      LIN.IBinOp (op, r, r1, r2)
  | LTL.ICall (callee, _) ->
      LIN.ICall callee
  | LTL.ILoad (r1, r2, o, _) ->
      LIN.ILoad (r1, r2, o)
  | LTL.IStore (r1, o, r2, _) ->
      LIN.IStore (r1, o, r2)

    (* Because [Branch.compress] has been called before, no [IGoto]
       instruction can be reached. *)

  | LTL.IGoto _ ->
      assert false

    (* Conditional branch instructions. In [LIN], control implicitly
       falls through to the second successor, so only the first
       successor is explicitly mentioned in the instruction. *)

  | LTL.IUnBranch (cond, r, l1, _) ->
      LIN.IUnBranch (cond, r, l1)
  | LTL.IBinBranch (cond, r1, r2, l1, _) ->
      LIN.IBinBranch (cond, r1, r2, l1)

    (* Instructions without a successor. *)

  | LTL.IReturn ->
      LIN.IReturn
  | LTL.ITailCall callee ->
      LIN.ITailCall callee
      
    (* Array allocation takes place now *)
    
  | LTL.INewArray _ ->
      LIN.ICall (CPrimitiveFunction Alloc) (* this obviously does not work,
      not taking the arguments into account *)

(* ------------------------------------------------------------------------- *)

(* [translate entry graph] turns an [LTL] control flow graph into
   a [LIN] sequence of instructions. *)

let translate entry graph =

  (* Keep track of the labels that have been visited (initially none), of the
     labels that must exist within the [LIN] code (initially only the graph's
     entry point) and of the list of [LIN] instructions that are being
     generated (initially empty). Instructions are held in the list in reverse
     order, for efficiency. The list is reversed once generation is over. *)

  let visited, required, instructions =
    ref Label.Set.empty, ref (Label.Set.singleton entry), ref []
  in

  (* Instantiate the functor. *)

  let module V = Ltl2linI.Visit (struct
    let fetch label =
      Label.Map.find label graph
    let translate_instruction =
      translate_instruction
    let generate instruction =
      instructions := instruction :: !instructions
    let require label =
      required := Label.Set.add label !required
    let mark label =
      visited := Label.Set.add label !visited
    let marked label =
      Label.Set.mem label !visited
  end) in

  (* Traverse the control flow graph. *)

  V.visit entry;

  (* Now, get rid of the labels that do not need to exist. Also,
     reverse the list to re-establish the correct order. *)

  List.filter (function
    | LIN.ILabel l ->
	Label.Set.mem l !required
    | _ ->
	true
  ) (List.rev !instructions)

(* ------------------------------------------------------------------------- *)

(* Extend the translation to procedures and programs. *)

let translate_procedure proc =
  let entry, graph = Branch.compress proc.LTL.entry proc.LTL.graph in
  {
    LIN.formals = proc.LTL.formals;
    LIN.locals = proc.LTL.locals;
    LIN.code = translate entry graph
  }

let translate_program (p : LTL.program) : LIN.program =
  {
    LIN.globals = p.LTL.globals;
    LIN.defs = StringMap.map translate_procedure p.LTL.defs
  }

