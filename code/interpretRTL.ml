open Printf
open MIPSOps
open Primitive
open InterpretUPP
open RTL
open Integer

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError = InterpretPP.RuntimeError

(* ------------------------------------------------------------------------- *)

(* Interpreting pseudo-register accesses. The reference cell that
   holds the pseudo-register is found by looking up the
   environment. *)

let lookup (env : 'value ref Register.Map.t) r =
  try
    Register.Map.find r env
  with Not_found ->
    fprintf stderr "Runtime error -- reference to undefined pseudo-register (%s).\n" (Register.print r);
    raise RuntimeError

let read env r =
  !(lookup env r)

let write env r v =
  (lookup env r) := v

(* ------------------------------------------------------------------------- *)

(* This exception is raised by [ITailCall] instructions in order to exit
   the current procedure or function. *)

type void

exception Transmit of void value option

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret p =

  (* Create an environment [genv] that holds the global variables. *)

  let genv = Array.make (p.globals / MIPS.word) (VInt 0l) in

  (* The code that follows refers to [p.defs] and to [genv] where
     necessary. *)

  (* ----------------------------------------------------------------------- *)

  (* Interpreting function and procedure calls. This returns [None] for
     procedures and [Some result] for functions. *)

  let rec interpret_call env callee actuals : 'address value option =
    match callee with

    | CPrimitiveFunction p ->

	interpret_primitive p actuals

    | CUserFunction f ->

	(* Lookup the definition of the procedure [f]. *)

	let proc =
	  try
	    StringMap.find f p.defs
	  with Not_found ->
	    assert false
	in

	(* Create a new local environment with (default) bindings for
	   the local pseudo-registers. Initialize the pseudo-registers
	   that hold the formal parameters with the actual
	   parameters. *)

	let env =
	  Register.Map.addm (Register.Map.lift allocate proc.locals) Register.Map.empty
	in

	List.iter2 (write env) proc.formals actuals;

	(* Execute the procedure body. *)

	try
	  interpret_graph env proc.graph proc.exit proc.entry;
	  (* Fetch the result in the result variable. *)
	  Option.map (read env) proc.result
	with Transmit result ->
	  (* If the body ends in a tail call, transmit its result. *)
	  (* This is not a true implementation of tail calls! but it will do. *)
	  match proc.result, result with
	  | None, _ ->
	      None (* tail call from procedure to procedure or function *)
	  | Some _, None ->
	      assert false (* no tail call from function to procedure *)
	  | Some _, Some _ ->
	      result (* tail call from function to function *)

  (* ----------------------------------------------------------------------- *)

  (* Interpreting control flow graphs. *)

  and interpret_graph env graph exitl l =
    try
      let i = Label.Map.find l graph in
      if Label.equal l exitl then begin
	fprintf stderr "Runtime error -- an instruction is associated with the exit label (%s).\n" (Label.print l);
	raise RuntimeError
      end;
      interpret_graph env graph exitl (interpret_instruction_at env l i)
    with Not_found ->
      if not (Label.equal l exitl) then begin
	fprintf stderr "Runtime error -- no instruction is associated with the current label (%s).\n" (Label.print l);
	raise RuntimeError
      end

  (* ----------------------------------------------------------------------- *)

  (* Interpreting instructions. This returns a continuation label. *)

  and interpret_instruction_at env l i =
    try
      interpret_instruction env i
    with RuntimeError as e ->
      fprintf stderr "Runtime error -- at label: %s\n" (Label.print l);
      raise e

  and interpret_instruction env = function

    | IConst (destr, i, l) ->
	write env destr (VInt i);
	l

    | IUnOp (op, destr, sourcer, l) ->
	write env destr (unop op (read env sourcer));
	l

    | IBinOp (op, destr, sourcer1, sourcer2, l) ->
	write env destr (
	  binop op (read env sourcer1) (read env sourcer2)
	);
	l

    | ICall (destro, callee, rs, l) ->
	begin match destro, interpret_call env callee (List.map (read env) rs) with
	| Some destr, Some result ->
	    write env destr result
	| None, None ->
	    ()
	| _ ->
	    assert false
	end;
	l

    | ITailCall (callee, rs) ->
	raise (Transmit (interpret_call env callee (List.map (read env) rs)))

    | ILoad (destr, addressr, offset, l) ->
	write env destr (load (read env addressr) offset);
	l

    | IStore (addressr, offset, valuer, l) ->
	store (read env addressr) offset (read env valuer);
	l

    | IGetGlobal (destr, offset, l) ->
	write env destr (gread genv offset);
	l

    | ISetGlobal (offset, sourcer, l) ->
	gwrite genv offset (read env sourcer);
	l
    
    | INewArray (destr, len, l) ->
	failwith "Unimplented, too hard ! fix me !" (* marteo should fix it *)

    | IGoto l ->
	l

    | IUnBranch (cond, sourcer, l1, l2) ->
	if InterpretMIPS.uncon cond (asInt (read env sourcer)) then l1 else l2

    | IBinBranch (cond, sourcer1, sourcer2, l1, l2) ->
	if InterpretMIPS.bincon cond (asInt (read env sourcer1)) (asInt (read env sourcer2)) then l1 else l2

  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting programs (end). *)

  (* Execute the program body. *)

  let (_ : 'address value option) =
    interpret_call Register.Map.empty (CUserFunction "_main") []
  in
  ()

