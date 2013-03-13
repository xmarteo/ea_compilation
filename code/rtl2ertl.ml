(* This module translates [RTL] into [ERTL]. *)

open Primitive

let translate_procedure f proc =

  (* ----------------------------------------------------------------------- *)

  (* Define a function that allocates a fresh pseudo-register, adds it
     to the set of pseudo-registers in use, and returns it. *)

  let locals =
    ref proc.RTL.locals
  in

  let allocate () =
    let register = Register.fresh proc.RTL.runiverse in
    locals := Register.Set.add register !locals;
    register
  in

  (* ----------------------------------------------------------------------- *)

  (* Define a function that generates an instruction at a fresh label. *)

  let graph, generate =
    Label.Map.generator proc.RTL.luniverse
  in

  (* ----------------------------------------------------------------------- *)

  (* Define a function that recognizes self-calls. *)

  let is_self = function
    | CPrimitiveFunction _ ->
	false
    | CUserFunction g ->
	f = g
  in

  (* ----------------------------------------------------------------------- *)

  (* Instantiate the functor that does the most interesting part of the work. *)

  let module I = Rtl2ertlI.Make (struct
    let allocate = allocate
    let generate = generate
    let formals = proc.RTL.formals
    let entry = proc.RTL.entry
    let result = proc.RTL.result
    let is_self = is_self
  end) in

  (* ----------------------------------------------------------------------- *)

  (* Define the translation of instructions. *)

  (* [translate_instruction] turns an [RTL] instruction into an [ERTL]
     instruction, or sequence of instructions, that transfers control to the
     same label(s). Existing instruction labels are preserved, that is, the
     labels in the new control flow graph form a superset of the labels in
     the existing control flow graph.

     The translation is trivial, except at procedure or function calls,
     where the calling convention is made explicit. *)

  let translate_instruction (i : RTL.instruction) : ERTL.instruction =
    match i with

    | RTL.ICall (odestr, callee, actuals, l) ->
	ERTL.IGoto (I.translate_call odestr callee actuals l)

    | RTL.ITailCall (callee, actuals) ->
	ERTL.IGoto (I.translate_tail_call callee actuals)

    | RTL.IConst (destr, i, l) ->
	ERTL.IConst (destr, i, l)

    | RTL.IUnOp (op, destr, sourcer, l) ->
	ERTL.IUnOp (op, destr, sourcer, l)

    | RTL.IBinOp (op, destr, sourcer1, sourcer2, l) ->
	ERTL.IBinOp (op, destr, sourcer1, sourcer2, l)

    | RTL.ILoad (destr, addressr, offset, l) ->
	ERTL.ILoad (destr, addressr, offset, l)

    | RTL.IStore (addressr, offset, valuer, l) ->
	ERTL.IStore (addressr, offset, valuer, l)

    | RTL.IGetGlobal (destr, offset, l) ->
	ERTL.IGetGlobal (destr, offset, l)

    | RTL.ISetGlobal (offset, valuer, l) ->
	ERTL.ISetGlobal (offset, valuer, l)

    | RTL.IGoto l ->
	ERTL.IGoto l

    | RTL.IUnBranch (cond, sourcer, truel, falsel) ->
	ERTL.IUnBranch (cond, sourcer, truel, falsel)

    | RTL.IBinBranch (cond, sourcer1, sourcer2, truel, falsel) ->
	ERTL.IBinBranch (cond, sourcer1, sourcer2, truel, falsel)

  in

  (* ----------------------------------------------------------------------- *)

  (* Translate each of the instructions in the existing control flow graph. *)

  let () =
    Label.Map.iter (fun label i ->
      let i = translate_instruction i in
      graph := Label.Map.add label i !graph
    ) proc.RTL.graph
  in

  (* ----------------------------------------------------------------------- *)

  (* Plug in the prologue and epilogue and build an [ERTL] procedure. *)

  {
    ERTL.formals = Misc.length proc.RTL.formals;
    ERTL.runiverse = proc.RTL.runiverse;
    ERTL.locals = !locals;
    ERTL.luniverse = proc.RTL.luniverse;
    ERTL.entry = I.prologue;
    ERTL.graph = Label.Map.add proc.RTL.exit (ERTL.IGoto I.epilogue) !graph
  }

(* ------------------------------------------------------------------------- *)

(* There only remains to translate programs. *)

let translate_program p = {
  ERTL.globals = p.RTL.globals;
  ERTL.defs = StringMap.mapi translate_procedure p.RTL.defs
}

