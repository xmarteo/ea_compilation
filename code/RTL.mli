(* This is the definition of the abstract syntax for the Register
   Transfer Language (RTL). Like UPP, this language is untyped --
   everything is a machine word. Local variables are no longer named
   and are represented by pseudo-registers. Expressions and conditions
   disappear: they are decomposed into sequences of intructions, again
   using pseudo-registers to hold intermediate results. Instructions
   are organized in a control flow graph. *)

open MIPSOps

(* Pseudo-registers are (a kind of) atoms. There is one universe of
   pseudo-registers for every function. Pseudo-registers are
   implemented by module [Register]. *)
	
(* Labels are (another kind of) atoms. There is one universe of labels
   for every function. Labels are implemented by module [Label]. *)

(* Instructions. Each instruction carries one or more continuation
   labels. *)

type instruction = 

    (* Load an immediate value into a register. *)

  | IConst of Register.t * int32 * Label.t

    (* Read a register, apply a unary operator, write to a register.
       Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * Register.t * Register.t * Label.t

    (* Read two registers, apply a binary operator, write to a register.
       Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * Register.t * Register.t * Register.t * Label.t

    (* Function or procedure call. Parameters are optional destination
       register (present for function calls, absent for procedure
       calls), callee, and actual parameters. *)

  | ICall of Register.t option * Primitive.callee * Register.t list * Label.t

    (* Tail (function or procedure) call. Parameters are callee and actual
       parameters. This instruction has no successor, because a tail call
       does not return. *)

  | ITailCall of Primitive.callee * Register.t list

    (* Memory read. Parameters are destination register, source
       (address) register, and a constant offset. *)

  | ILoad of Register.t * Register.t * offset * Label.t

    (* Memory write. Parameters are address register, a constant
       offset, and value register. *)

  | IStore of Register.t * offset * Register.t * Label.t

    (* Global variable access. Parameters are destination register
       and constant offset. *)

  | IGetGlobal of Register.t * offset * Label.t

    (* Global variable update. Parameters are a constant offset
       and value register. *)

  | ISetGlobal of offset * Register.t * Label.t

    (* No operation (just jump to the continuation label). *)

  | IGoto of Label.t

    (* Unary conditional branch. Parameters are a unary condition, the
       register that the condition is applied to, the label that is
       jumped to if the condition holds, and the label that is jumped
       to if the condition does not hold. *)

  | IUnBranch of uncon * Register.t * Label.t * Label.t

    (* Binary conditional branch. Parameters are a binary condition,
       the registers that the condition is applied to, the label that
       is jumped to if the condition holds, and the label that is
       jumped to if the condition does not hold. *)

  | IBinBranch of bincon * Register.t * Register.t * Label.t * Label.t
  
    (* Array allocation. Parameters are destination register and length *)
    
  | INewArray of Register.t * Register.t * Label.t

(* Function definitions. *)

and procedure = {

  (* A list of formal arguments. *)

  formals: Register.t list;

  (* By convention, functions return a result, by writing into a
     distinguished [result] pseudo-register. Procedures return no
     result. *)

  result: Register.t option;

  (* A set of all pseudo-registers used by the function, including
     those mentioned above. *)

  runiverse: Register.universe;
  locals: Register.Set.t;

  (* The control flow graph of the function is represented by an entry
     label, an exit label, and a mapping of labels to instructions.
     (No instruction is associated to the exit label.) *)

  luniverse: Label.universe;
  entry: Label.t;
  exit: Label.t;
  graph: graph

}

(* Control flow graphs. *)

and graph =
    instruction Label.Map.t

(* Programs. *)

and program = {

  (* The space required by global variables, in bytes. *)

  globals: int32;

  (* A set of named function or procedure definitions. By convention,
     the program body is viewed as a procedure with no formal
     parameters, called "_main". *)

  defs: procedure StringMap.t

}

