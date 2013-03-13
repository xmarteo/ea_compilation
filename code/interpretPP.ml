open Printf
open MIPSOps
open Primitive
open PP
open Integer

(* ------------------------------------------------------------------------- *)

(* This exception is intentionally never caught. This allows us to obtain
   stack backtraces when debugging. *)

exception RuntimeError

(* ------------------------------------------------------------------------- *)

(* Values are tagged with their types. Booleans and integers are still
   distinguished. Arrays are represented as Objective Caml arrays,
   whose indices have type [int], whereas PP array indices are 32 bit
   integers -- this will require some conversions between [int32] and
   [int]. The [VNil] value is the default value for variables of array
   type -- it is an array that cannot be dereferenced. *)

type value =
  | VBool of bool
  | VInt of int32
  | VArray of value array
  | VNil

(* These accessors allow projecting out of the sum [value]. They
   implement dynamic checks, which cannot fail if typechecking has
   been performed and if the typechecker is correct. However, the type
   system does not prevent accesses to an uninitialized variable of
   array type. *)

let asBool = function
  | VBool b ->
      b
  | _ ->
      assert false

let asInt = function
  | VInt i ->
      i
  | _ ->
      assert false

let asArray = function
  | VArray a ->
      a
  | VNil ->
      fprintf stderr "Runtime error -- access to nil array.\n";
      raise RuntimeError
  | _ ->
      assert false

(* ------------------------------------------------------------------------- *)

(* [default typ] returns a default value of type [typ]. It is used to
   assign initial values to newly created global or local
   variables. *)

let default = function
  | TypBool ->
      VBool false
  | TypInt ->
      VInt 0l
  | TypArray _ ->
      VNil

(* [allocate typ] returns a fresh reference cell that holds a default
   value of type [typ]. *)

let allocate typ =
  ref (default typ)

(* ------------------------------------------------------------------------- *)

(* Interpreting primitive operations. We expect the number and nature
   of the parameters to be consistent; this is guaranteed by the
   typechecker. *)

let interpret_primitive (p : primitive) (actuals : value list) =
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
  | _ ->
      assert false

(* ------------------------------------------------------------------------- *)

(* An environment maps an identifier (a name for a variable) to a
   modifiable value (a reference cell that holds a value). There is
   one environment for global variables and one for local
   variables. *)

type environment =
  value ref StringMap.t

(* Interpreting variable accesses. The reference cell that holds the
   variable is found by looking up the global and local
   environments. The lookup cannot fail if the variable is properly
   bound, a condition that the typechecker enforces. If a global and a
   local have the same name, the latter takes precedence. *)

let lookup (genv : environment) (env : environment) (x : string) : value ref =
  try
    StringMap.find x env
  with Not_found ->
    try
      StringMap.find x genv
    with Not_found ->
      assert false

(* ------------------------------------------------------------------------- *)

(* Another environment maps an identifier (a name for a procedure or
   function) to a definition. Procedure and function definitions are
   fixed, so no reference cells are involved here. *)

type definitions =
  PP.procedure StringMap.t

(* Interpreting function and procedure calls. This returns [None] for
   procedures and [Some result] for functions. *)

let rec interpret_call
  (defs : definitions)
  (genv : environment)
  (env : environment)
  (callee : callee)
  (actuals : expression list)
  : value option =

  (* Evaluate the actual arguments. [List.map] implements left to
     right evaluation order, and [PP] has left to right evaluation
     order, so this is consistent. *)

  let actuals = List.map (interpret_expression defs genv env) actuals in

  (* User-defined functions and primitive functions are treated
     differently. *)

  match callee with

  | CPrimitiveFunction p ->

      interpret_primitive p actuals

  | CUserFunction f ->

      (* Lookup the definition of the procedure [f]. *)

      let proc =
	try
	  StringMap.find f defs
	with Not_found ->
	  assert false
      in

      (* Create a new local environment with bindings for the actual
	 parameters, local variables, and return value. The latter two
	 are initialized with default values. Notice how [ref] and
	 [allocate] create fresh memory cells to hold the new
	 variables. *)

      let env =
	List.fold_right2 (fun (formal, _) actual env ->
	  StringMap.add formal (ref actual) env
        ) proc.formals actuals StringMap.empty
      in

      let env =
	Option.fold (fun typ env -> StringMap.add f (allocate typ) env) proc.result env
      in

      let env =
	StringMap.addm (StringMap.map allocate proc.locals) env
      in
	  
      (* Execute the procedure body. *)

      interpret_instruction defs genv env proc.body;

      (* Fetch the result in the result variable. *)

      Option.map (fun _ -> !(StringMap.find f env)) proc.result

(* ------------------------------------------------------------------------- *)

(* Interpreting expressions. *)

and interpret_expression defs genv env = function

  | EConst (ConstBool b) ->
      VBool b

  | EConst (ConstInt i) ->
      VInt i

  | EGetVar x ->
      !(lookup genv env x)

  | EUnOp (UOpNeg, e) ->
      VInt (- (asInt (interpret_expression defs genv env e)))

  | EBinOp (op, e1, e2) ->
      begin
	let i1 = asInt (interpret_expression defs genv env e1) in
	let i2 = asInt (interpret_expression defs genv env e2) in
	match op with
	| OpAdd ->
	    VInt (i1 + i2)
	| OpSub ->
	    VInt (i1 - i2)
	| OpMul ->
	    VInt (i1 * i2)
	| OpDiv ->
	    VInt (i1 / i2)
	| OpLt ->
	    VBool (i1 < i2)
	| OpLe ->
	    VBool (i1 <= i2)
	| OpGt ->
	    VBool (i1 > i2)
	| OpGe ->
	    VBool (i1 >= i2)
	| OpEq ->
	    VBool (i1 = i2)
	| OpNe ->
	    VBool (i1 <> i2)
      end

  | EFunCall (callee, es) ->
      begin match interpret_call defs genv env callee es with
      | Some result ->
	  result
      | None ->
	  assert false
      end

  | EArrayGet (ea, ei) ->
      let a = asArray (interpret_expression defs genv env ea) in
      let i = asInt (interpret_expression defs genv env ei) in
      begin try
	Array.get a i
      with Invalid_argument _ ->
	fprintf stderr "Runtime error -- array index out of bounds (%ld / %ld).\n"
	  i (Array.length a);
	raise RuntimeError
      end

  | EArrayAlloc (typ, en) ->
      let n = asInt (interpret_expression defs genv env en) in
      begin try
	VArray (Array.make n (default typ))
      with Invalid_argument _ ->
	fprintf stderr "Runtime error -- negative array length (%ld).\n" n;
	raise RuntimeError
      end
      
  | EArrayLength tab ->
      let t = asArray (interpret_expression defs genv env tab) in
      VInt (Array.length t)

(* ------------------------------------------------------------------------- *)

(* Interpreting conditions. This yields a Boolean result. *)

and interpret_condition defs genv env = function

  | CExpression e ->
      asBool (interpret_expression defs genv env e)

  | CNot c ->
      not (interpret_condition defs genv env c)

    (* Objective Caml implements shortcut evaluation of [&&] and [||],
       which makes them suitable for interpreting PP's [and] and [or]
       connectives. *)

  | CAnd (c1, c2) ->
      interpret_condition defs genv env c1 && interpret_condition defs genv env c2

  | COr (c1, c2) ->
      interpret_condition defs genv env c1 || interpret_condition defs genv env c2

(* ------------------------------------------------------------------------- *)

(* Interpreting instructions. This yields no result, but execution can
   have side effects, that is, modify the value of some variables, or
   invoke primitive operations that perform input/output. *)

and interpret_instruction defs genv env = function

  | IProcCall (callee, es) ->
      begin match interpret_call defs genv env callee es with
      | None ->
	  ()
      | Some _ ->
	  assert false
      end

  | ISetVar (x, e) ->
      (lookup genv env x) := (interpret_expression defs genv env e)

  | IArraySet (ea, ei, ev) ->
      let a = asArray (interpret_expression defs genv env ea) in
      let i = asInt (interpret_expression defs genv env ei) in
      let v = interpret_expression defs genv env ev in
      begin try
	Array.set a i v
      with Invalid_argument _ ->
	fprintf stderr "Runtime error -- array index out of bounds (%ld / %ld).\n"
	  i (Array.length a);
	raise RuntimeError
      end

  | ISeq is ->
      List.iter (interpret_instruction defs genv env) is

  | IIf (cond, i1, i2) ->
      interpret_instruction defs genv env
	(if interpret_condition defs genv env cond then i1 else i2)

  | IWhile (cond, body) ->
      while interpret_condition defs genv env cond do
	interpret_instruction defs genv env body
      done

(* ------------------------------------------------------------------------- *)

(* Interpreting programs. *)

let interpret (p : program) =

  (* Create an environment that holds the global variables. *)

  let genv =
    StringMap.map allocate p.globals
  in

  (* Execute the program body. *)

  interpret_instruction p.defs genv StringMap.empty p.main

