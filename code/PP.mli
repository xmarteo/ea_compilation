(* This is the definition of the abstract syntax for Pseudo-Pascal
   (PP). This language is typed, with integers, Booleans, and arrays.
   All data is word-sized. *)

(* Expressions. *)

type expression =

    (* Constants. *)

  | EConst of constant                         

    (* Global or local variable access. *)

  | EGetVar of string                               

    (* Unary operator applications. *)

  | EUnOp of unop * expression                 

    (* Binary operator applications. *)

  | EBinOp of MIPSOps.binop * expression * expression  

    (* Function call. *)

  | EFunCall of Primitive.callee * expression list          

    (* Array read. Parameters are an array and an integer index. *)

  | EArrayGet of expression * expression       

    (* Array allocation. Parameters are the desired element type
       and the desired number of elements. *)

  | EArrayAlloc of typ * expression
  
    (* Array length *)
  
  | EArrayLength of expression

(* Constants. *)

and constant = LPP.constant =

    (* Boolean constants. *)

  | ConstBool of bool                          

    (* Integer constants. *)

  | ConstInt of int32

(* Unary integer arithmetic operators. *)

and unop = LPP.unop =

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

  | IProcCall of Primitive.callee * expression list          

    (* Local or global variable update. *)

  | ISetVar of string * expression

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

and typ = LPP.typ =
  | TypInt
  | TypBool
  | TypArray of typ

(* Function definitions. *)

and procedure = {

  (* A list of typed formal arguments. *)

  formals: (string * typ) list;

  (* A typed result, present for functions and absent for
     procedures. By convention, functions return a result by writing
     into a local variable whose name is that of the function. *)

  result: typ option;

  (* A set of typed local variables. *)

  locals: typ StringMap.t;

  (* The body of the function. *)

  body: instruction

}

(* Programs. *)

and program = {

  (* A set of typed global variables. *)

  globals: typ StringMap.t;

  (* A set of named function or procedure definitions. *)

  defs: procedure StringMap.t;

  (* The program body. *)

  main: instruction

}

