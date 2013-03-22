open Printf
open MIPSOps
open Primitive
open InterpretUPP
open InterpretERTL
open InterpretLTL
open LIN
open Integer

(* TEMPORARY if some instruction (say, a binop) raises RuntimeError, the error
   message will not display a code label (like it would in the RTL and ERTL interpreters).
   Doing so is made difficult by the fact that not every instruction carries a label.
   Fix? *)

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError = InterpretPP.RuntimeError

(* ------------------------------------------------------------------------- *)

(* The labels that appear in addresses are now either labels, as in
   [LTL], or sequences of instructions. This is useful because the
   successor of an [ICall] instruction does not necessarily carry an
   explicit label, even though it is a jump target. *)

type lin_label =
  | LL of Label.t
  | LI of instruction list

type lin_procedure =
    instruction list Label.Map.t (* control flow graph *) * int32 (* locals *)

type address =
    (lin_procedure, lin_label) InterpretERTL.address

type value =
    address InterpretUPP.value

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret p =

  (* Create an environment [henv] that holds the hardware registers. *)

  let henv : value henv = MIPS.RegisterMap.lift allocate MIPS.registers in

  (* Create an array that holds the global variables and store its
     address in [$gp]. *)

  hwrite henv MIPS.gp (VArray (Array.make (p.globals / MIPS.word) (VInt 0l), 0l));

  (* The code that follows refers to [p.defs] and [henv] where
     necessary. *)

  (* ----------------------------------------------------------------------- *)

  (* Interpreting function and procedure calls. *)

  let rec interpret_call (stack : value stack) callee (next : address) =
    match callee with

    | CPrimitiveFunction p ->

	interpret_primitive henv p;
	interpret_jump stack next

    | CUserFunction f ->

	(* Lookup the definition of the procedure [f]. *)

	let proc =
	  try
	    StringMap.find f p.defs
	  with Not_found ->
	    assert false
	in

	(* Build a mapping of labels to sequences of instructions. *)

	let rec build = function
	  | ILabel l :: instructions ->
	      Label.Map.add l instructions (build instructions)
	  | _ :: instructions ->
	      build instructions
	  | [] ->
	      Label.Map.empty
	in

	(* Write the address of the next instruction into [$ra] and
	   jump to the procedure's entry point. The procedure itself
	   is responsible for pushing a new stack frame. *)

	hwrite henv MIPS.ra (VCode next);
	interpret_instructions stack (build proc.code, proc.locals) proc.code

  (* ----------------------------------------------------------------------- *)

  (* Interpreting addresses. *)

  and interpret_instructions stack proc instructions =

    (* Fetch instruction. *)

    match instructions with
    | [] ->
	fprintf stderr "Runtime error -- fell off the instruction stream.\n";
	raise RuntimeError
    | i :: instructions ->

	(* Execute. *)

	interpret_instruction stack proc i instructions

  and interpret_jump stack address =
    match address with
    | AddrInit ->

	(* Halt. *)

	()

    | AddrCode (proc, LI instructions) ->

	(* Execute. *)

	interpret_instructions stack proc instructions

    | AddrCode (proc, LL l) ->

	(* Decode label. *)

	let instructions =
	  try
	    let graph, _ = proc in
	    Label.Map.find l graph
	  with Not_found ->
	    fprintf stderr
	      "Runtime error -- no instruction is associated with the current label (%s).\n" (Label.print l);
	    raise RuntimeError
	in

	(* Execute. *)

	interpret_instructions stack proc instructions

  (* ----------------------------------------------------------------------- *)

  (* Interpreting instructions. *)

  and interpret_instruction (stack : value stack) (proc : lin_procedure) i instructions =
    match i with

    | INewFrame ->
	let _, locals = proc in
	interpret_instructions (newframe stack locals) proc instructions

    | IDeleteFrame ->
	interpret_instructions (deleteframe stack) proc instructions

    | IGetStack (destr, slot) ->
	hwrite henv destr (sread stack slot);
	interpret_instructions stack proc instructions

    | ISetStack (slot, sourcer) ->
	swrite stack slot (hread henv sourcer);
	interpret_instructions stack proc instructions

    | IConst (destr, i) ->
	hwrite henv destr (VInt i);
	interpret_instructions stack proc instructions

    | IUnOp (op, destr, sourcer) ->
	hwrite henv destr (unop op (hread henv sourcer));
	interpret_instructions stack proc instructions

    | IBinOp (op, destr, sourcer1, sourcer2) ->
	hwrite henv destr (binop op (hread henv sourcer1) (hread henv sourcer2));
	interpret_instructions stack proc instructions

      (* Pass the address of the next instruction as the return address. *)

    | ICall (callee) ->
	interpret_call stack callee (AddrCode (proc, LI instructions))

    | ILoad (destr, addressr, offset) ->
	hwrite henv destr (load (hread henv addressr) offset);
	interpret_instructions stack proc instructions

    | IStore (addressr, offset, valuer) ->
	store (hread henv addressr) offset (hread henv valuer);
	interpret_instructions stack proc instructions

    | IGoto l ->
	interpret_jump stack (AddrCode (proc, LL l))

      (* If condition holds, then jump, otherwise continue in sequence. *)

    | IUnBranch (cond, sourcer, l) ->
	if InterpretMIPS.uncon cond (asInt (hread henv sourcer)) then
	  interpret_jump stack (AddrCode (proc, LL l))
	else
	  interpret_instructions stack proc instructions

    | IBinBranch (cond, sourcer1, sourcer2, l) ->
	if InterpretMIPS.bincon cond (asInt (hread henv sourcer1)) (asInt (hread henv sourcer2)) then
	  interpret_jump stack (AddrCode (proc, LL l))
	else
	  interpret_instructions stack proc instructions

    | IReturn ->
	interpret_jump stack (asAddress (hread henv MIPS.ra))

    | ITailCall callee ->
	interpret_call stack callee (asAddress (hread henv MIPS.ra))

      (* Continue in sequence. *)

    | ILabel l ->
	interpret_instructions stack proc instructions

  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting programs (end). *)

  (* Execute the program body. *)

  interpret_call empty_stack (CUserFunction "_main") AddrInit

