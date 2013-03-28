(* This is the definition of the abstract syntax for Untyped
   Pseudo-Pascal (UPP). This language is untyped -- everything is a
   machine word: integers, Booleans, data pointers, code pointers.
   Global variables are no longer named and are represented by
   absolute memory addresses. The unary operators are now those of the
   target processor, as opposed to those of the source language. The
   binary operators happen to be the same in both languages. *)

open MIPSOps

(* Expressions. *)

type expression =

    (* Constants. *)

  | EConst of int32

    (* Local variable access. *)

  | EGetVar of string

    (* Global variable access. *)

  | EGetGlobal of offset

    (* Unary operator applications. *)

  | EUnOp of unop * expression               

    (* Binary operator applications. *)

  | EBinOp of binop * expression * expression  

    (* Function call. *)

  | EFunCall of Primitive.callee * expression list          

    (* Memory read. Parameters are a pointer and a constant offset. *)

  | ELoad of expression * offset
  
    (* Array allocation *)
    
  | ENewArray of expression

(* Conditions. *)

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

    (* Local variable update. *)

  | ISetVar of string * expression

    (* Global variable update. *)

  | ISetGlobal of offset * expression

    (* Memory write. Parameters are a pointer, a constant offset, and
       a new value. *)

  | IStore of expression * offset * expression

    (* Sequence. *)

  | ISeq of instruction list                   

    (* Conditional. *)

  | IIf of condition * instruction * instruction

    (* Loop. *)

  | IWhile of condition * instruction

(* Function definitions. *)

and procedure = {

  (* A list of formal arguments. *)

  formals: (string * PP.typ) list;

  (* By convention, functions return a result by writing into a local
     variable whose name is that of the function. Procedures return no
     result. This flag distinguishes the two. *)

  result: bool;

  (* A set of local variables. *)

  locals: PP.typ StringMap.t ;

  (* The body of the function. *)

  body : instruction

}

(* Programs. *)

and program = {

  (* The space required by global variables, in bytes. *)

  globals: int32;

  (* A set of named function or procedure definitions. By convention,
     the program body is viewed as a procedure with no formal
     parameters, called "_main". *)

  defs: procedure StringMap.t;

}

