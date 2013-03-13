open Printf
open MIPSOps
open Primitive
open InterpretUPP
open InterpretRTL
open ERTL
open Integer

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError = InterpretPP.RuntimeError

(* ------------------------------------------------------------------------- *)

(* In [ERTL], an address is either a pair of a procedure and a label
   or the initial address (used to start the interpreter). *)

type ('procedure, 'label) address =
  | AddrCode of 'procedure * 'label
  | AddrInit

(* ------------------------------------------------------------------------- *)

(* Accessing hardware registers. *)

type 'value henv =
    'value ref MIPS.RegisterMap.t

let hlookup henv hwr =
  try
    MIPS.RegisterMap.find hwr henv
  with Not_found ->
    assert false

let hread henv hwr =
  if MIPS.equal hwr MIPS.zero then
    VInt 0l (* reading register [$zero] always reads 0 *)
  else
    !(hlookup henv hwr)

let hwrite henv hwr v =
  (hlookup henv hwr) := v

(* ------------------------------------------------------------------------- *)

(* Interpreting primitive operations. Parameters and results are exchanged
   in hardware registers. *)

let interpret_primitive henv = function
  | Write ->
      let v = hread henv (List.hd MIPS.parameters) in
      fprintf stdout "%ld%!" (asInt v)
  | Writeln ->
      let v = hread henv (List.hd MIPS.parameters) in
      fprintf stdout "%ld\n%!" (asInt v)
  | Readln ->
      let line = input_line stdin in
      let v = try
	VInt (Int32.of_string line)
      with Failure "int of string" ->
	fprintf stderr "readln: error: \"%s\" is not the representation of an integer.\n%!" line;
	raise RuntimeError
      in
      hwrite henv MIPS.result v
  | Alloc ->
      let v = hread henv (List.hd MIPS.parameters) in
      hwrite henv MIPS.result (interpret_alloc v)

(* ------------------------------------------------------------------------- *)

(* Accessing stack slots. The incoming and outgoing stack areas are
   represented as mappings of offsets to value references. Slots in
   the outgoing area are allocated on demand, since the stack can grow
   indefinitely. The size of the incoming area is fixed. The local
   area is represented as a mapping of pseudo-registers to value
   references.

   The semantics of [ERTL] specifies that [INewFrame] pushes a stack
   frame while [IDeleteFrame] pops one, thus restoring the caller's
   frame.

   Note that [IReturn] jumps to the address specified by [$ra], which
   is not necessarily the caller's address if [$ra] was not correctly
   set.

   Each stack frame records the procedure with which it is associated.
   This is in principle unnecessary, and does not influence the semantics.
   It is used only at [IReturn] to detect that [$ra] is incorrectly set
   and abort cleanly. *)

module Int32Map =
  Map.Make(Int32)

type frame = {
    incoming: ertl_address value ref Int32Map.t;
    mutable outgoing: ertl_address value ref Int32Map.t;
    local: ertl_address value ref Register.Map.t;
    owner: procedure;
  }

and ertl_address =
    (ERTL.procedure, Label.t) address

type stack =
    frame list

let destruct stack =
  match stack with
  | [] ->
      fprintf stderr "Runtime error -- the call stack is empty (mismatched newframe/deleteframe).\n";
      raise RuntimeError
  | frame :: stack ->
      frame, stack

let slookup stack slot =
  let frame, _ = destruct stack in
  match slot with
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

let read stack r =
  let frame, _ = destruct stack in
  read frame.local r

let write stack r v =
  let frame, _ = destruct stack in
  write frame.local r v

let sread stack slot =
  !(slookup stack slot)

let swrite stack slot v =
  (slookup stack slot) := v

(* An empty stack. *)

let empty_stack =
  []

(* Creating a stack frame. The callee's incoming stack area is the
   caller's outgoing stack area. The callee's outgoing stack area is
   empty. The callee's frame is pushed on top of the stack. *)

let newframe stack proc =
  match stack with
  | [] ->
      [{
         incoming = Int32Map.empty;
         outgoing = Int32Map.empty;
         local = Register.Map.lift allocate proc.locals;
	 owner = proc;
      }]
  | frame :: _ ->
      {
        incoming = frame.outgoing;
        outgoing = Int32Map.empty;
        local = Register.Map.lift allocate proc.locals;
	owner = proc;
      } :: stack

(* Deleting a stack frame. We assume that incoming parameters are
   never written and outgoing parameters are never read, so that the
   caller's stack is unaffected by the call. *)

let deleteframe stack =
  let _, stack = destruct stack in
  stack

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret p =

  (* Create an environment [genv] that holds the global variables. *)

  let genv = Array.make (p.globals / MIPS.word) (VInt 0l) in

  (* Create an environment [henv] that holds the hardware registers. *)

  let henv = MIPS.RegisterMap.lift allocate MIPS.allocatable in

  (* The code that follows refers to [p.defs], [genv], and [henv]
     where necessary. *)

  (* A counter of the number of pending calls, for sanity checking. *)

  let calls = ref 0 in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting function and procedure calls. *)

  let interpret_call stack callee next : stack * ertl_address =
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

	incr calls;
	hwrite henv MIPS.ra (VCode next);
	stack,
	AddrCode (proc, proc.entry)

  in

  (* ----------------------------------------------------------------------- *)

  (* Sanity checking. *)

  (* Make sure that the address of the current instruction belongs to
     a procedure [proc] that is the owner of the current stack
     frame. This check can be made whenever the stack frame is assumed
     to be valid (i.e. between newframe and deleteframe, i.e.,
     whenever the stack or a pseudo-register is accessed). *)

  (* Make sure that the number of pending calls equals the number of
     frames on the stack. (This assumes no tail calls.) *)

  let valid_frame stack proc =
    begin match stack with
    | frame :: _ when proc == frame.owner ->
	()
    | _ ->
	fprintf stderr "Runtime error -- current pc does not match current stack frame.\n";
	fprintf stderr "              -- maybe $ra was incorrectly set,\n";
	fprintf stderr "              -- or newframe/deleteframe were incorrectly used?\n";
	raise RuntimeError
    end;
    if !calls <> List.length stack then begin
	fprintf stderr "Runtime error -- current frames on stack do not match current pending calls.\n";
	fprintf stderr "              -- maybe newframe/deleteframe were incorrectly used?\n";
	raise RuntimeError
    end;
  in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting instructions. *)

  let interpret_instruction stack proc i : stack * ertl_address =
    match i with

    | INewFrame l ->
	newframe stack proc,
	AddrCode(proc, l)

    | IDeleteFrame l ->
	valid_frame stack proc;
	deleteframe stack,
	AddrCode (proc, l)

    | IGetHwReg (destr, sourcehwr, l) ->
	valid_frame stack proc;
	write stack destr (hread henv sourcehwr);
	stack,
	AddrCode (proc, l)

    | ISetHwReg (desthwr, sourcer, l) ->
	valid_frame stack proc;
	hwrite henv desthwr (read stack sourcer);
	stack,
	AddrCode (proc, l)

    | IGetStack (destr, slot, l) ->
	valid_frame stack proc;
	write stack destr (sread stack slot);
	stack,
	AddrCode (proc, l)

    | ISetStack (slot, sourcer, l) ->
	valid_frame stack proc;
	swrite stack slot (read stack sourcer);
	stack,
	AddrCode (proc, l)

    | IConst (destr, i, l) ->
	valid_frame stack proc;
	write stack destr (VInt i);
	stack,
	AddrCode (proc, l)

    | IUnOp (op, destr, sourcer, l) ->
	valid_frame stack proc;
	write stack destr (unop op (read stack sourcer));
	stack,
	AddrCode (proc, l)

    | IBinOp (op, destr, sourcer1, sourcer2, l) ->
	valid_frame stack proc;
	write stack destr (binop op (read stack sourcer1) (read stack sourcer2));
	stack,
	AddrCode (proc, l)

      (* Pass the address of the next instruction as the return address. *)

    | ICall (callee, _, l) ->
	interpret_call stack callee (AddrCode (proc, l))

    | ILoad (destr, addressr, offset, l) ->
	valid_frame stack proc;
	write stack destr (load (read stack addressr) offset);
	stack,
	AddrCode (proc, l)

    | IStore (addressr, offset, valuer, l) ->
	valid_frame stack proc;
	store (read stack addressr) offset (read stack valuer);
	stack,
	AddrCode (proc, l)

    | IGetGlobal (destr, offset, l) ->
	valid_frame stack proc;
	write stack destr (gread genv offset);
	stack,
	AddrCode (proc, l)

    | ISetGlobal (offset, sourcer, l) ->
	valid_frame stack proc;
	gwrite genv offset (read stack sourcer);
	stack,
	AddrCode (proc, l)

    | IGoto l ->
	stack,
	AddrCode (proc, l)

    | IUnBranch (cond, sourcer, l1, l2) ->
	valid_frame stack proc;
	stack,
	AddrCode (proc,
	  if InterpretMIPS.uncon cond (asInt (read stack sourcer)) then l1 else l2
	)

    | IBinBranch (cond, sourcer1, sourcer2, l1, l2) ->
	valid_frame stack proc;
	stack,
	AddrCode (proc,
	  if InterpretMIPS.bincon cond (asInt (read stack sourcer1)) (asInt (read stack sourcer2)) then l1 else l2
	)

      (* Jump to [$ra]. *)

    | IReturn _ ->
	decr calls;
	stack,
	asAddress (hread henv MIPS.ra)

      (* Tail call. Interpret the call, passing [$ra] as the return address. *)

    | ITailCall (callee, _) ->
	decr calls;
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

