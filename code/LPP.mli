(* This is the definition of the abstract syntax for Located
   Pseudo-Pascal (LPP). This language is identical to Pseudo-Pascal,
   except abstract syntax trees carry source code locations. These
   locations are useful for printing accurate error messages when
   typechecking. *)

(* All identifiers carry locations. *)

type identifier =
    string Location.t

(* Expressions carry locations at every node. *)

type expression =
    raw_expression Location.t

(* Expressions. *)

and raw_expression =

    (* Constants. *)

  | EConst of constant                         

    (* Global or local variable access. *)

  | EGetVar of identifier

    (* Unary operator applications. *)

  | EUnOp of unop * expression

    (* Binary operator applications. *)

  | EBinOp of MIPSOps.binop * expression * expression

    (* Function call. *)

  | EFunCall of Primitive.callee Location.t * expression list

    (* Array read. Parameters are an array and an integer index. *)

  | EArrayGet of expression * expression

    (* Array allocation. Parameters are the desired element type
       and the desired number of elements. *)

  | EArrayAlloc of typ * expression
  
    (* Array length. Parameter is an array *)
   
  | EArrayLength of expression
  
    (* Tricking the typechecker into thinking
       e is of type t *)
       
  | ECastVar of expression * typ

(* Constants. *)

and constant =

    (* Boolean constants. *)

  | ConstBool of bool                          

    (* Integer constants. *)

  | ConstInt of int32

(* Unary integer arithmetic operators. *)

and unop =

  | UOpNeg

(* Conditions are expressions that produce a Boolean result. Their
   syntax allows the use of the Boolean connectives [not], [and], and
   [or], which are not available as part of the syntax of
   expressions. This somewhat nonstandard design is intended to
   eliminate some redundancy in the compiler. Conditions are used in
   the [if] and [while] instructions.

   The [and] or [or] connectives are special in that their right-hand
   side is not evaluated if the value of the left-hand side determines
   the value of the whole. *)

and condition =

    (* Boolean expression. *)

  | CExpression of expression

    (* Boolean connectives. *)

  | CNot of condition
  | CAnd of condition * condition
  | COr of condition * condition

(* Instructions. *)

and instruction = 

    (* Procedure call. *)

  | IProcCall of Primitive.callee Location.t * expression list          

    (* Local or global variable update. *)

  | ISetVar of identifier * expression

    (* Array write. Parameters are an array, an integer index, and a
       new element value. *)

  | IArraySet of expression * expression * expression

    (* Sequence. *)

  | ISeq of instruction list                   

    (* Conditional. *)

  | IIf of condition * instruction * instruction 

    (* Loop. *)

  | IWhile of condition * instruction

(* Types. *)

and typ =
  | TypInt
  | TypBool
  | TypArray of typ

(* Function definitions. *)

and procedure = {

  (* A list of typed formal arguments. *)

  formals: (identifier * typ) list;

  (* A typed result, present for functions and absent for
     procedures. By convention, functions return a result by writing
     into a local variable whose name is that of the function. *)

  result: typ option;

  (* A list of typed local variables. *)

  locals: (identifier * typ) list;

  (* The body of the function. *)

  body: instruction

}

(* Programs. *)

and program = {

  (* A set of typed global variables. *)

  globals: (identifier * typ) list;

  (* A set of named function or procedure definitions. *)

  defs: (identifier * procedure) list;

  (* The program body. *)

  main: instruction

}

