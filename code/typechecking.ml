(* This is a typechecker for [LPP] (or, equivalently, [PP]) programs.
   Locations are used when printing error messages. *)

open Primitive
open LPP

(* ------------------------------------------------------------------------- *)

(* There are two name spaces in [PP]: one for procedures and
   functions, one for (global and local) variables. Hence, the
   typechecker maintains two environments. One maps procedure and
   function names to function types; the other maps variable names to
   types. A function type consists of a list of the types of the
   formal arguments and an optional result type. *)

type function_type =
    typ list * typ option
      
type function_env =
    function_type StringMap.t

type variable_env =
    typ StringMap.t

(* ------------------------------------------------------------------------- *)

(* Environment lookup. An error message is displayed if the name
   that we are looking for isn't found in the environment. *)

let lookup (thing : string) (i : identifier) env =
  try
    StringMap.find (Location.content i) env
  with Not_found ->
    Error.error i (Printf.sprintf "Error: %s %s is undefined.\n" thing (Location.content i))

let vlookup : identifier -> variable_env -> typ =
  lookup "variable"

let flookup : identifier -> function_env -> function_type =
  lookup "function"

(* ------------------------------------------------------------------------- *)

(* Comparing and printing types. *)

let rec equal typ1 typ2 =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool ->
      true
  | TypArray typ1, TypArray typ2 ->
      equal typ1 typ2
  | _, _ ->
      false

let equalarray typ1 typ2 =
  equal typ1 (TypArray typ2)

let rec print_type = function
  | TypInt ->
      "integer"
  | TypBool ->
      "boolean"
  | TypArray typ ->
      Printf.sprintf "array of %s" (print_type typ)

(* ------------------------------------------------------------------------- *)

(* Typechecking function and procedure calls. *)

let rec typecheck_call (fenv : function_env) (venv : variable_env) callee expressions =
  let (formals : typ list), (result : typ option) =
    match Location.content callee with
    | CUserFunction f ->
	flookup (Location.make (Location.startpos callee) (Location.endpos callee) f) fenv
    | CPrimitiveFunction (Write | Writeln) ->
	[ TypInt ], None
    | CPrimitiveFunction Readln ->
	[], Some TypInt
    | CPrimitiveFunction Alloc ->
	(* This primitive operation is not available in [PP]. *)
	assert false
  in
  try
    List.iter2 (typecheck_expression_expecting fenv venv) formals expressions;
    result
  with Invalid_argument _ ->
    Error.error callee
      (Printf.sprintf "Invalid function call (expected %d arguments, got %d).\n"
	 (List.length formals)
	 (List.length expressions))

(* Typechecking expressions. *)

and typecheck_expression_expecting fenv venv typ1 e : unit =
  let typ2 = typecheck_expression fenv venv e in
  if not (equal typ1 typ2) then
    Error.error e
      (Printf.sprintf "Type mismatch (expected %s, got %s).\n" (print_type typ1) (print_type typ2))

and typecheck_expression_expecting_array fenv venv e : typ =
  match typecheck_expression fenv venv e with
  | TypArray component ->
      component
  | typ ->
      Error.error e (Printf.sprintf "Type mismatch (expected an array, got %s).\n" (print_type typ))

and typecheck_expression (fenv : function_env) (venv : variable_env) (e : LPP.expression) : typ =
  match Location.content e with
  | EConst (ConstBool _) ->
      TypBool
  | EConst (ConstInt _) ->
      TypInt
  | EGetVar v ->
      vlookup v venv
  | EUnOp (op, e) ->
      let expected, returned =
	match op with
	| UOpNeg ->
	    TypInt, TypInt
      in
      typecheck_expression_expecting fenv venv expected e;
      returned
  | EBinOp (op, e1, e2) ->
      let expected, returned =
	match op with
	| MIPSOps.OpAdd
	| MIPSOps.OpSub
	| MIPSOps.OpMul
	| MIPSOps.OpDiv ->
	    TypInt, TypInt
	| MIPSOps.OpLt                                       
	| MIPSOps.OpLe
	| MIPSOps.OpGt
	| MIPSOps.OpGe
	| MIPSOps.OpEq
	| MIPSOps.OpNe ->
	    TypInt, TypBool
      in
      typecheck_expression_expecting fenv venv expected e1;
      typecheck_expression_expecting fenv venv expected e2;
      returned
  | EFunCall (callee, expressions) ->
      begin match typecheck_call fenv venv callee expressions with
      | None ->
	  Error.error callee "This is a procedure, not a function.\n"
      | Some typ ->
	  typ
      end
  | EArrayGet (earray, eindex) ->
      typecheck_expression_expecting fenv venv TypInt eindex;
      typecheck_expression_expecting_array fenv venv earray
  | EArrayAlloc (typ, elength) ->
      typecheck_expression_expecting fenv venv TypInt elength;
      TypArray typ
  | EArrayLength earray ->
      begin
      match typecheck_expression_expecting_array fenv venv earray with
      | _ -> TypInt end
  | ECastVar (evar, typ) ->
      begin
      match typecheck_expression fenv venv evar with
      | _ -> typ end

(* ------------------------------------------------------------------------- *)

(* Typechecking conditions. *)

let rec typecheck_condition (fenv : function_env) (venv : variable_env) = function
  | CExpression e ->
      typecheck_expression_expecting fenv venv TypBool e
  | CNot c ->
      typecheck_condition fenv venv c
  | CAnd (c1, c2)
  | COr (c1, c2) ->
      typecheck_condition fenv venv c1;
      typecheck_condition fenv venv c2

(* ------------------------------------------------------------------------- *)

(* Typechecking instructions. *)

let rec typecheck_instruction (fenv : function_env) (venv : variable_env) = function
  | IProcCall (callee, expressions) ->
      begin match typecheck_call fenv venv callee expressions with
      | None ->
	  ()
      | Some _ ->
	  Error.error callee "This is a function, not a procedure.\n"
      end
  | ISetVar (v, e) ->
      typecheck_expression_expecting fenv venv (vlookup v venv) e
  | IArraySet (earray, eindex, evalue) ->
      let typ = typecheck_expression_expecting_array fenv venv earray in
      typecheck_expression_expecting fenv venv TypInt eindex;
      typecheck_expression_expecting fenv venv typ evalue
  | ISeq instructions ->
      List.iter (typecheck_instruction fenv venv) instructions
  | IIf (cond, ithen, ielse) ->
      typecheck_condition fenv venv cond;
      typecheck_instruction fenv venv ithen;
      typecheck_instruction fenv venv ielse
  | IWhile (cond, ibody) ->
      typecheck_condition fenv venv cond;
      typecheck_instruction fenv venv ibody

(* ------------------------------------------------------------------------- *)

(* This turns a list of variable bindings [bindings] and a specific
   name [x] into a list of occurrences of the identifier [x]. *)

let occurrences (x : string) (bindings : (identifier * 'a) list) : identifier list =
  List.map fst (List.filter (fun (id, _) -> x = Location.content id) bindings)

(* These turn a list of bindings into a map. *)

let map_of_association_list entity (bindings : (identifier * 'a) list) : 'a StringMap.t =
  try
    StringMap.of_association_list (List.map (fun (id, data) -> (Location.content id, data)) bindings)
  with StringMap.Duplicate x ->
    Error.errors (occurrences x bindings) (Printf.sprintf "%s %s is declared more than once.\n" entity x)

let variable_map bindings : variable_env =
  map_of_association_list "Variable" bindings

let procedure_map defs : procedure StringMap.t =
  map_of_association_list "Procedure" defs

(* ------------------------------------------------------------------------- *)

(* Typechecking a function requires first locally extending the
   variable environment with bindings for the function's formal
   parameters, result variable, and local variables, then typechecking
   the function's body. *)

let typecheck_function
    (fenv : function_env)
    (venv : variable_env)
    ((f, proc) : identifier * procedure)
    =

  (* Check that the formal parameters, actual parameters, and result
     variable have pairwise distinct names, so that no confusion
     arises. *)

  let formals = proc.LPP.formals
  and locals = proc.LPP.locals in

  let mf = Location.content f
  and mformals = variable_map formals
  and mlocals = variable_map locals in

  if StringMap.mem mf mformals then
    Error.errors (f :: occurrences mf formals)
      "A formal parameter cannot carry the same name as\n\
       the procedure or function that is being defined.\n";

  if StringMap.mem mf mlocals then
    Error.errors (f :: occurrences mf locals)
      "A local variable cannot carry the same name as\n\
       the procedure or function that is being defined.\n";

  begin
    try
      let x =
	StringSet.choose
	  (StringSet.inter
	     (StringMap.domain mformals)
	     (StringMap.domain mlocals))
      in
      Error.errors (occurrences x formals @ occurrences x locals)
	"A formal parameter and a local variable cannot\n\
	 carry the same name.\n"
    with Not_found ->
      ()
  end;
  
  (* Extend the environment with the formal parameters. *)

  let venv =
    StringMap.addm mformals venv
  in

  (* Extend the environment with the result variable, if this function
     returns a result. *)

  let venv =
    Option.fold (StringMap.add mf) proc.result venv
  in

  (* Extend the environment with the local variables. *)

  let venv =
    StringMap.addm mlocals venv
  in

  (* Typecheck the body. *)

  typecheck_instruction fenv venv proc.body

(* ------------------------------------------------------------------------- *)

(* This extracts a function type out of a function definition. *)

let extract_function_type (proc : procedure) : function_type =
  (List.map snd proc.formals, proc.result)

(* ------------------------------------------------------------------------- *)

(* The program body, as well as each function, is typechecked under a
   variable environment where only global variables are available.

   In [PP], all procedures and functions are implicitly mutually
   recursive. Thus, the entire program is typechecked under a single,
   fixed, function environment, which contains an entry for every
   function. *)

let typecheck_program (p : program) =
  let fenv = StringMap.map extract_function_type (procedure_map p.defs)
  and venv = variable_map p.globals in
  List.iter (typecheck_function fenv venv) p.defs;
  typecheck_instruction fenv venv p.main

