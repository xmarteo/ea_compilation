(* This transformation maps [PP] programs to [UPP] programs.
   The key changes are as follows.

   First, types are dropped, so, for instance, Booleans, integers, and
   pointers are no longer distinguished.

   Second, accesses to local and global variables are
   distinguished.

   Third, operations on arrays are replaced with more elementary
   memory load and store operations.

   Fourth, instructions are inserted at the beginning of every
   procedure to initialize all local variables to zero.

   Last, the set of operators changes to better reflect that of the
   MIPS. Some instruction selection is performed to recognize and
   adequately translate certain patterns. Instruction selection takes
   the form of a set of rewriting rules, implemented inside
   [Upp2upp]. *)

open Integer
open Primitive
open MIPSOps
open LPP

(* ------------------------------------------------------------------------- *)

(* A global environment maps global variables to the offset that they
   have been assigned in the global storage area. *)

type genv =
    (offset * PP.typ) StringMap.t

(* A local environment is a set of local variables. A variable is
   considered local if its name appears in the local environment,
   and global otherwise. *)

type env =
    PP.typ StringMap.t
    
(* The environment of functions defined in the PP program, with the list
   of their formal parameters types *)
    
type fenv =
    ((PP.typ list) * (PP.typ option)) StringMap.t

(* ------------------------------------------------------------------------- *)

(* Translating a word count into a byte count. This involves
   multiplying by the word size. *)

let w2b (e : UPP.expression) : UPP.expression =
  Upp2upp.mkbinop OpMul (UPP.EConst MIPS.word) e

let convert (e : UPP.expression) : UPP.expression =
  let mul = Upp2upp.mkbinop OpMul (UPP.EConst 2l) e in
  Upp2upp.mkbinop OpAdd mul (UPP.EConst 1l)
  
let unconvert (e : UPP.expression) : UPP.expression =
  let temp = Upp2upp.mkbinop OpSub e (UPP.EConst 1l) in
  Upp2upp.mkbinop OpDiv temp (UPP.EConst 2l)
  

(* Convert a pair of an array base address and element index into an
   element address. This involves adding the base address to the
   index, converted to a byte offset. *)

let element_address (base : UPP.expression) (index : UPP.expression) : UPP.expression =
  Upp2upp.mkbinop OpAdd base (w2b index)

(* Translating memory loads and stores. *)

let mkload (e : UPP.expression) : UPP.expression =
  match e with

    (* An explicit offset is available. Make it part of the
       load instruction. *)

  | UPP.EUnOp (UOpAddi i, e) ->
      UPP.ELoad (e, i)

    (* Default case. *)

  | e ->
      UPP.ELoad (e, 0l)

let mkstore (e1 : UPP.expression) (e2 : UPP.expression) : UPP.instruction =
  match e1 with

    (* An explicit offset is available. Make it part of the
       store instruction. *)

  | UPP.EUnOp (UOpAddi i1, e1) ->
      UPP.IStore (e1, i1, e2)

    (* Default case. *)

  | _ ->
      UPP.IStore (e1, 0l, e2)

(* ------------------------------------------------------------------------- *)

(* Translating expressions. *)

let rec translate_expression (fenv : fenv) (genv : genv) (env : env) = function

    (* Constants. Drop type distinctions. *)

  | PP.EConst (PP.ConstBool false) ->
      UPP.EConst 0l
  | PP.EConst (PP.ConstBool true) ->
      UPP.EConst 1l
  | PP.EConst (PP.ConstInt i) ->
      UPP.EConst i

    (* Variable access. Distinguish globals and locals. *)

  | PP.EGetVar v ->
      if StringMap.mem v env then

        (* This is a local variable. *)
        
        begin
        match (StringMap.find v env) with
        | TypArray _ ->
	  UPP.EGetVar v
	| _ ->
	  unconvert (UPP.EGetVar v)
	end
      else

	(* This is a global variable. Translate to a global variable
	   access instruction. *)
	   
	let (offset, typ) = StringMap.find v genv in
	let ex = UPP.EGetGlobal offset in
	begin
	match typ with
	| TypArray _ ->
	  ex
	| _ ->
	  unconvert ex
	end

    (* Operator applications. Their sub-expressions are easily translated
       using recursive calls to [translate_expression]. Then, the translation
       of the operator application itself is delegated to [Upp2upp]. *)

  | PP.EUnOp (op, e) ->
      Upp2upp.mkunop op (translate_expression fenv genv env e)
  | PP.EBinOp (op, e1, e2) ->
      Upp2upp.mkbinop op (translate_expression fenv genv env e1) (translate_expression fenv genv env e2)

    (* Function call. *)

  | PP.EFunCall (callee, es) ->
      begin
      match callee with
      | CPrimitiveFunction _ -> UPP.EFunCall (callee, List.map (fun x -> translate_expression fenv genv env x) es)
      | CUserFunction funcString ->
          let typList, retour = StringMap.find funcString fenv in
	  let fullList = List.combine es typList in
	  let trans (expr, typ) =
	    begin
	    match typ with
	    | TypArray _ -> translate_expression fenv genv env expr
	    | _ -> convert (translate_expression fenv genv env expr)
	    end
	  in
	  let newExpr = UPP.EFunCall (callee, List.map trans fullList) in
	  begin
	  match retour with
	  | Some (TypArray _) -> newExpr
	  | _ -> unconvert newExpr
	  end
      end
      
    (* Array read. We compute the element's address and access it. *)

  | PP.EArrayGet (earray, eindex) ->

      mkload (
        element_address
          (translate_expression fenv genv env earray)
          (translate_expression fenv genv env eindex)
      )

    (* Array allocation. Forget about the type and convert the
       desired size, which is expressed as a number of elements. 
    *)

  | PP.EArrayAlloc (t, e) ->
      let len = translate_expression fenv genv env e in
      UPP.ENewArray(w2b len)
      
  (* the length of an array t is stored in t[-1] *)
  
  | PP.EArrayLength earray ->
      mkload (
        element_address
          (translate_expression fenv genv env earray)
          (UPP.EConst Int32.minus_one)
      )
      
  (* casting a variable to some type does actually completely nothing
     except mess with the typechecking *)
     
  | PP.ECastVar (e, _) ->
      translate_expression fenv genv env e

(* ------------------------------------------------------------------------- *)

(* Translating conditions. *)

let rec translate_condition (fenv : fenv) (genv : genv) (env : env) = function
  | PP.CExpression e ->
      UPP.CExpression (translate_expression fenv genv env e)
  | PP.CNot c ->
      UPP.CNot (translate_condition fenv genv env c)
  | PP.CAnd (c1, c2) ->
      UPP.CAnd (translate_condition fenv genv env c1, translate_condition fenv genv env c2)
  | PP.COr (c1, c2) ->
      UPP.COr (translate_condition fenv genv env c1, translate_condition fenv genv env c2)

(* ------------------------------------------------------------------------- *)

(* Translating instructions. *)

let rec translate_instruction (fenv : fenv) (genv : genv) (env : env) = function

    (* Procedure call. *)

  | PP.IProcCall (callee, es) ->
      begin
      match callee with
      | CPrimitiveFunction _ -> UPP.IProcCall (callee, List.map (fun x -> translate_expression fenv genv env x) es)
      | CUserFunction funcString ->
	  let typList, _ = StringMap.find funcString fenv in
	  let fullList = List.combine es typList in
	  let trans (expr, typ) =
	    begin
	    match typ with
	    | TypArray _ -> translate_expression fenv genv env expr
	    | _ -> convert (translate_expression fenv genv env expr)
	    end
	  in
	  UPP.IProcCall (callee, List.map trans fullList)
      end

    (* Variable update. Distinguish globals and locals. *)

  | PP.ISetVar (v, e) ->
      if StringMap.mem v env then

        (* This is a local variable. *)
        begin
	  match (StringMap.find v env) with
	  | PP.TypArray _ ->
	    UPP.ISetVar (v, translate_expression fenv genv env e)
	  | _  ->
             UPP.ISetVar (v, convert (translate_expression fenv genv env e))
	end
	
      else

	(* This is a global variable. Translate to a global variable
	   update instruction. *)
        let (offset, typ) = (StringMap.find v genv) in
	begin
	  match typ with
	  | PP.TypArray _ -> 
	    UPP.ISetGlobal (offset, translate_expression fenv genv env e)
	  | _ ->
	    UPP.ISetGlobal (offset, convert (translate_expression fenv genv env e))
	end

    (* Array write. We compute the element's address and access it. *)

  | PP.IArraySet (earray, eindex, evalue) ->

      mkstore (
          element_address
            (translate_expression fenv genv env earray)
            (translate_expression fenv genv env eindex)
        )
	(translate_expression fenv genv env evalue)

    (* Sequence. *)

  | PP.ISeq is ->
      UPP.ISeq (List.map (translate_instruction fenv genv env) is)

    (* Conditional. *)

  | PP.IIf (c, i1, i2) ->
      UPP.IIf (
        translate_condition fenv genv env c,
        translate_instruction fenv genv env i1,
        translate_instruction fenv genv env i2
      )

    (* While. *)

  | PP.IWhile (c, i) ->
      UPP.IWhile (
        translate_condition fenv genv env c,
        translate_instruction fenv genv env i
      )

(* ------------------------------------------------------------------------- *)

(* Translating procedures. *)

let translate_procedure fenv genv f proc =

  (* Obtain a list of the formal parameters' names (with their
     types). *)

  let formals = proc.PP.formals in

  (* Build the procedure's local environment -- here, simply a set of
    local variable names that allows distinguishing global and local
    variables. (Local variables hide global variables, so [genv] alone
    isn't enough to distinguish.) *)

  let rec make_env env = function
    | [] -> env
    | (s, t)::tail ->
      make_env (StringMap.add s t env) tail
  in
  let env = make_env StringMap.empty formals
  in
  let result, env =
    match proc.PP.result with
    | None ->
	false, env
    | Some typ ->
	true, StringMap.add f typ env
  in
  let locals = proc.PP.locals in
  let env = StringMap.fold StringMap.add locals env in

  (* Translate the procedure's body. *)

  let body =
    [ translate_instruction fenv genv env proc.PP.body ]
  in

  (* Insert instructions to initialize every local variable (including
     the result variable, if there is one) to zero. This is required in
     order to preserve the semantics of [PP]. Indeed, the semantics of
     [PP] states that every local variable receives a default value when
     it is allocated, whereas the semantics of [UPP] states that every
     local variable is initially undefined.

     Many of these initialization instructions are usually superfluous,
     because programmers often explicitly initialize variables before
     using them. In that case, the newly added instructions are in fact
     dead. This fact will be detected by the liveness analysis, and the
     dead instructions will be removed in the translation of [ERTL] to
     [LTL]. *)

  let initialize local =
   UPP.ISetVar (local, UPP.EConst 0l)
   in

  let body =
    if result then initialize f :: body else body
  in

  let body =
    StringMap.fold (fun local (_ : PP.typ) body ->
      initialize local :: body
    ) proc.PP.locals body
  in

  (* This is it. *)

  {
    UPP.formals = formals;
    UPP.result = result;
    UPP.locals = locals;
    UPP.body = UPP.ISeq body
  }

(* Building the global environment. This involves choosing the offsets
   at which global variables are stored. *)

let allocate_globals (p : PP.program) : fenv * genv * int32 =

  (* [next] holds the next available location. All data is word-sized,
     so [next] is incremented by 4 bytes every time we allocate a new
     global variable, regardless of the type of the data that is being
     allocated. The final value of [next] is the overall space required
     by the global variables. *)

  let (u,v) = StringMap.fold (fun global (t : PP.typ) (genv, next) ->
    StringMap.add global (next, t) genv,
    next + MIPS.word
  ) p.PP.globals (StringMap.empty, 0l)
  in
  let w = StringMap.fold (fun name def newMap ->
    let l = def.PP.formals in
    let result = def.PP.result in
    let typs = List.map snd l in
    StringMap.add name (typs, result) newMap)
    p.PP.defs StringMap.empty
  in (w, u, v)

(* Translating programs. *)

let translate_program (p : PP.program) : UPP.program =

  (* Build the global environment and compute how much space
     is needed to hold the global variables. *)

  let fenv, genv, globals = allocate_globals p in

  (* Translate the program body to a procedure, for greater
     uniformity. *)

  let main = {
    UPP.formals = [];
    UPP.result = false;
    UPP.locals = StringMap.empty;
    UPP.body = translate_instruction fenv genv StringMap.empty p.PP.main
  } in

  (* Build a transformed program. *)

  {
    UPP.globals = globals;
    UPP.defs = StringMap.add "_main" main (StringMap.mapi (translate_procedure fenv genv) p.PP.defs)
  }

