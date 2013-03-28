open Printf
open MIPSOps
open Primitive
open UPP
open Integer

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError = InterpretPP.RuntimeError

(* ------------------------------------------------------------------------- *)

(* In [UPP] and [RTL], values can be undefined, 32 bit integers, or
   arrays of values. Address arithmetic is allowed -- simulating it in
   Objective Caml is somewhat painful. In [ERTL], [LTL], and [LIN],
   values also include return addresses. We make room for return
   addresses right now, even though they are unused in [UPP] and
   [RTL], so as to avoid changing the type of values later. *)

type 'address value =
  | VUndefined
  | VInt of int32
  | VArray of 'address value array * int32
  | VCode of 'address

(* These accessors allow projecting out of the sum [value]. *)

let asInt = function
  | VInt i ->
      i
  | _ ->
      fprintf stderr "Runtime error -- expected an integer.\n";
      raise RuntimeError

let asArray = function
  | VArray (a, offset) ->
      a, offset
  | _ ->
      fprintf stderr "Runtime error -- expected an array.\n";
      raise RuntimeError

let asAddress = function
  | VCode address ->
      address
  | _ ->
      fprintf stderr "Runtime error -- expected an address.\n";
      raise RuntimeError

(* [default] is the default values for local variables of all types.
   The default value is [VUndefined], which means that using an
   uninitialized variable is an error. A default value of 0 is used
   for global variables and heap-allocated arrays. *)

let default =
  VUndefined

let allocate _ =
  ref default

let init _ =
  ref (VInt 0l)

(* ------------------------------------------------------------------------- *)

(* Interpreting primitive operations. *)

let interpret_alloc v =
  let n = (asInt v) / MIPS.word in
  begin try
    VArray (Array.make n (VInt 0l), 0l)
  with Invalid_argument _ ->
    fprintf stderr "Runtime error -- negative array length (%ld).\n" n;
    raise RuntimeError
  end

let interpret_primitive p actuals =
  match p, actuals with
  | Write, [ v ] ->
      fprintf stdout "%ld%!" (asInt v);
      None
  | Writeln, [ v ] ->
      fprintf stdout "%ld\n%!" (asInt v);
      None
  | Readln, [] ->
      let line = input_line stdin in
      begin try
	Some (VInt (Int32.of_string line))
      with Failure "int of string" ->
	fprintf stderr "readln: error: \"%s\" is not the representation of an integer.\n%!" line;
	raise RuntimeError
      end
  | Alloc, [ v ] ->
      Some (interpret_alloc v)
  | _ ->
      assert false

(* ------------------------------------------------------------------------- *)

(* Interpreting local variable accesses. The reference cell that holds
   the variable is found by looking up the environment. *)

let lookup (env : 'address value ref StringMap.t) x =
  try
    StringMap.find x env
  with Not_found ->
    fprintf stderr "Runtime error -- reference to undefined local variable (%s).\n" x;
    raise RuntimeError

(* ------------------------------------------------------------------------- *)

(* Interpreting global variable accesses. *)

let gread genv offset =
  try
    Array.get genv (offset / MIPS.word)
  with Invalid_argument _ ->
    fprintf stderr "Runtime error -- global variable offset out of bounds (%ld / %ld).\n"
      offset (MIPS.word * Array.length genv);
    raise RuntimeError

let gwrite genv offset v =
  try
    Array.set genv (offset / MIPS.word) v
  with Invalid_argument _ ->
    fprintf stderr "Runtime error -- global variable offset out of bounds (%ld / %ld).\n"
      offset (MIPS.word * Array.length genv);
    raise RuntimeError

(* ------------------------------------------------------------------------- *)

(* Interpreting arithmetic operators. This includes a simulation of
   array arithmetic. *)

let unop op v =
  match op, v with
  | UOpAddi 0l, _ ->
      v
  | _, VInt i ->
      VInt (InterpretMIPS.unop op i)
  | UOpAddi i, VArray (a, offset) ->
      VArray (a, offset + i)
  | _, _ ->
      fprintf stderr "Runtime error -- unary arithmetic operation has illegal operand.\n";
      raise RuntimeError

let binop op v1 v2 =
  match op, v1, v2 with
  | _, VInt i1, VInt i2 ->
      VInt (InterpretMIPS.binop op i1 i2)
  | OpAdd, VArray (a, offset), VInt i
  | OpAdd, VInt i, VArray (a, offset) ->
      VArray (a, offset + i)
  | OpSub, VArray (a, offset), VInt i ->
      VArray (a, offset - i)
  | _, _, _ ->
      fprintf stderr "Runtime error -- binary arithmetic operation has illegal operand.\n";
      raise RuntimeError

(* ------------------------------------------------------------------------- *)

(* Interpreting array loads and stores. *)

let load va offset1 =
  let a, offset0 = asArray va in
  let offset = offset0 + offset1 in
  try
    Array.get a (offset / MIPS.word)
  with Invalid_argument _ ->
    fprintf stderr "Runtime error -- load out of bounds (%ld / %ld).\n"
      offset (MIPS.word * Array.length a);
    raise RuntimeError

let store va offset1 vv =
  let a, offset0 = asArray va in
  let offset = offset0 + offset1 in
  try
    Array.set a (offset / MIPS.word) vv
  with Invalid_argument _ ->
    fprintf stderr "Runtime error -- store out of bounds (%ld / %ld).\n"
      offset (MIPS.word * Array.length a);
    raise RuntimeError

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret p =

  (* Create an environment [genv] that holds the global variables.
     The global variables are initialized to 0. *)

  let genv = Array.make (p.globals / MIPS.word) (VInt 0l) in

  (* The code that follows refers to [p.defs] and to [genv] where
     necessary. *)

  (* ----------------------------------------------------------------------- *)

  (* Interpreting function and procedure calls. This returns [None] for
     procedures and [Some result] for functions. *)

  let rec interpret_call env callee actuals : 'address value option =

    (* Evaluate the actual arguments. [List.map] implements left to
       right evaluation order, and [PP] has left to right evaluation
       order, so this is consistent. *)

    let actuals = List.map (interpret_expression env) actuals in

    (* User-defined functions and primitive functions are treated
       differently. *)

    match callee with

    | CPrimitiveFunction p ->

	interpret_primitive p actuals

    | CUserFunction f ->

	failwith "unimplemented, fix me ! marteo !"

  (* ----------------------------------------------------------------------- *)

  (* Interpreting expressions. *)

  and interpret_expression env = function

    | EConst i ->
	VInt i

    | EGetVar x ->
	!(lookup env x)

    | EGetGlobal offset ->
	gread genv offset

    | EUnOp (op, e) ->
	unop op (interpret_expression env e)

    | EBinOp (op, e1, e2) ->
	let v1 = interpret_expression env e1 in
	let v2 = interpret_expression env e2 in
	binop op v1 v2

    | EFunCall (callee, es) ->
	begin match interpret_call env callee es with
	| Some result ->
	    result
	| None ->
	    assert false
	end

    | ELoad (e, offset) ->
	load (interpret_expression env e) offset
	
    | ENewArray e ->
      let n = asInt (interpret_expression env e) in
      begin try
	VArray ((Array.make n default), n)
      with Invalid_argument _ ->
	fprintf stderr "Runtime error -- negative array length (%ld).\n" n;
	raise RuntimeError
      end

  (* ----------------------------------------------------------------------- *)

  (* Interpreting conditions. This yields a Boolean result. *)

  and interpret_condition env = function

    | CExpression e ->
	begin match asInt (interpret_expression env e) with
	| 0l ->
	    false
	| 1l ->
	    true
	| x ->
	    fprintf stderr "Runtime error -- undefined Boolean condition (%ld).\n" x;
	    raise RuntimeError
	end

    | CNot c ->
	not (interpret_condition env c)

      (* Objective Caml implements shortcut evaluation of [&&] and [||],
	 which makes them suitable for interpreting PP's [and] and [or]
	 connectives. *)

    | CAnd (c1, c2) ->
	interpret_condition env c1 && interpret_condition env c2

    | COr (c1, c2) ->
	interpret_condition env c1 || interpret_condition env c2

  (* ----------------------------------------------------------------------- *)

  (* Interpreting instructions. This yields no result, but execution can
     have side effects, that is, modify the value of some variables, or
     invoke primitive operations that perform input/output. *)

  and interpret_instruction env = function

    | IProcCall (callee, es) ->
	begin match interpret_call env callee es with
	| None ->
	    ()
	| Some _ ->
	    assert false
	end

    | ISetVar (x, e) ->
	(lookup env x) := (interpret_expression env e)

    | ISetGlobal (offset, e) ->
	gwrite genv offset (interpret_expression env e)

    | IStore (ea, offset, ev) ->
	let va = interpret_expression env ea in
	let vv = interpret_expression env ev in
	store va offset vv

    | ISeq [] ->
	()

    | ISeq (i :: is) ->
	interpret_instruction env i;
	interpret_instruction env (ISeq is)

    | IIf (cond, i1, i2) ->
	interpret_instruction env
	  (if interpret_condition env cond then i1 else i2)

    | IWhile (cond, body) ->
        while interpret_condition env cond do
	  interpret_instruction env body
        done

   in

  (* ----------------------------------------------------------------------- *)

  (* Interpreting programs (end). *)

  (* Execute the program body. *)

  let (_ : 'address value option) =
    interpret_call StringMap.empty (CUserFunction "_main") []
  in
  ()

