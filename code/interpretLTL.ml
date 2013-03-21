open Printf
open MIPSOps
open Primitive
open InterpretUPP
open InterpretERTL
open LTL
open Integer

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError = InterpretPP.RuntimeError

(* ------------------------------------------------------------------------- *)

(* Accessing stack slots. The incoming, outgoing, and local stack
   areas are represented as mappings of offsets to value
   references. Slots in the outgoing area are allocated on demand,
   since the stack can grow indefinitely. The sizes of the incoming
   and local areas are fixed. *)

module Int32Map =
  Map.Make(Int32)

type 'value frame = {
    incoming: 'value ref Int32Map.t;
    mutable outgoing: 'value ref Int32Map.t;
    local: 'value ref Int32Map.t
  }

type 'value stack =
    'value frame list

let slookup stack slot =
  let frame, _ = destruct stack in
  match slot with
  | SlotLocal offset ->
      begin try
	Int32Map.find offset frame.local
      with Not_found ->
	fprintf stderr "Runtime error -- invalid local slot (%ld).\n" offset;
	raise RuntimeError
      end
  | SlotIncoming offset ->
      begin try
	Int32Map.find offset frame.incoming
      with Not_found ->
	fprintf stderr "Runtime error -- invalid incoming slot (%ld).\n" offset;
	raise RuntimeError
      end
  | SlotOutgoing offset ->
      begin try
	Int32Map.find offset frame.outgoing
      with Not_found ->
	let cell = ref default in
	frame.outgoing <- Int32Map.add offset cell frame.outgoing;
	cell
      end

let sread stack slot =
  !(slookup stack slot)

let swrite stack slot v =
  (slookup stack slot) := v

(* An empty stack. *)

let empty_stack =
  []

(* Creating a stack frame. The callee's incoming stack area is the
   caller's outgoing stack area.  The callee's outgoing stack area is
   empty. The callee's local area is initialized at a fixed size. The
   callee's frame is pushed on top of the stack. *)

let allocate_locals locals =
  let rec loop offset m =
    if offset = locals then
      m
    else
      loop (offset + MIPS.word) (Int32Map.add offset (ref default) m)
  in
  loop 0l Int32Map.empty

let newframe stack locals =
  match stack with
  | [] ->
      [{
         incoming = Int32Map.empty;
         outgoing = Int32Map.empty;
         local = allocate_locals locals
      }]
  | frame :: _ ->
      {
        incoming = frame.outgoing;
        outgoing = Int32Map.empty;
         local = allocate_locals locals
      } :: stack

(* Deleting a stack frame. We assume that incoming parameters are never
   written and outgoing parameters are never read, so that the
   caller's stack is unaffected by the call. *)

let deleteframe stack =
  let _, stack = destruct stack in
  stack

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret p =

  (* Create an environment [henv] that holds the hardware registers. *)

  let henv = MIPS.RegisterMap.lift allocate MIPS.registers in

  (* Create an array that holds the global variables and store its
     address in [$gp]. *)

  hwrite henv MIPS.gp (VArray (Array.make (p.globals / MIPS.word) (VInt 0l), 0l));

  (* The code that follows refers to [p.defs] and [henv] where
     necessary. *)

  (* ----------------------------------------------------------------------- *)

  (* Interpreting function and procedure calls. *)

  let interpret_call stack callee next =
    match callee with

    | CPrimitiveFunction p ->

	interpret_primitive henv p;
	stack,
	next

    | CUserFunction f ->

	(* Lookup the definition of the procedure [f]. *)

	let proc =
	  try
	    StringMap.find f p.defs
	  with Not_found ->
	    assert false
	in

	(* Write the address of the next instruction into [$ra] and
	   jump to the procedure's entry point. The procedure itself
	   is responsible for pushing a new stack frame. *)

	hwrite henv MIPS.ra (VCode next);
	stack,
	AddrCode (proc, proc.entry)

  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting instructions. *)

  let interpret_instruction stack proc i =
    match i with

    | INewFrame l ->
	newframe stack proc.locals,
	AddrCode (proc, l)

    | IDeleteFrame l ->
	deleteframe stack,
	AddrCode (proc, l)

    | IGetStack (destr, slot, l) ->
	hwrite henv destr (sread stack slot);
	stack, AddrCode (proc, l)

    | ISetStack (slot, sourcer, l) ->
	swrite stack slot (hread henv sourcer);
	stack, AddrCode (proc, l)

    | IConst (destr, i, l) ->
	hwrite henv destr (VInt i);
	stack, AddrCode (proc, l)

    | IUnOp (op, destr, sourcer, l) ->
	hwrite henv destr (unop op (hread henv sourcer));
	stack, AddrCode (proc, l)

    | IBinOp (op, destr, sourcer1, sourcer2, l) ->
	hwrite henv destr (binop op (hread henv sourcer1) (hread henv sourcer2));
	stack, AddrCode (proc, l)

      (* Pass the address of the next instruction as the return address. *)

    | ICall (callee, l) ->
	interpret_call stack callee (AddrCode (proc, l))

    | ILoad (destr, addressr, offset, l) ->
	hwrite henv destr (load (hread henv addressr) offset);
	stack, AddrCode (proc, l)

    | IStore (addressr, offset, valuer, l) ->
	store (hread henv addressr) offset (hread henv valuer);
	stack, AddrCode (proc, l)
	
    | INewArray _ -> failwith "Unimplemented !" (* marteo should fix this *)

    | IGoto l ->
	stack, AddrCode (proc, l)

    | IUnBranch (cond, sourcer, l1, l2) ->
	stack,
	AddrCode (proc,
	  if InterpretMIPS.uncon cond (asInt (hread henv sourcer)) then l1 else l2
	)

    | IBinBranch (cond, sourcer1, sourcer2, l1, l2) ->
	stack,
	AddrCode (proc,
	  if InterpretMIPS.bincon cond (asInt (hread henv sourcer1)) (asInt (hread henv sourcer2)) then l1 else l2
	)

      (* Jump to [$ra]. *)

    | IReturn ->
	stack,
	asAddress (hread henv MIPS.ra)

      (* Tail call. Interpret the call, passing [$ra] as the return address. *)

    | ITailCall callee ->
	interpret_call stack callee (asAddress (hread henv MIPS.ra))

  in

  let interpret_instruction_at stack proc l i =
    try
      interpret_instruction stack proc i
    with RuntimeError as e ->
      fprintf stderr "Runtime error -- at label: %s\n" (Label.print l);
      raise e

  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting addresses. *)

  let rec fetch_and_execute stack proc l : unit =

    (* Fetch instruction. *)

    let i =
      try
	Label.Map.find l proc.graph
      with Not_found ->
	fprintf stderr
	  "Runtime error -- no instruction is associated with the current label (%s).\n" (Label.print l);
	raise RuntimeError
    in

    (* Execute this instruction and continue at the next address. *)

    continue (interpret_instruction_at stack proc l i)

  and continue (stack, next) : unit =
    match next with
    | AddrInit ->

	(* Halt. *)

	()

    | AddrCode (proc, l) ->

	fetch_and_execute stack proc l

  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting programs (end). *)

  (* Execute the program body. *)

  continue (interpret_call empty_stack (CUserFunction "_main") AddrInit)

