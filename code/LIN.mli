(* This is the definition of the abstract syntax for the Linearized
   Location Transfer Language (LIN). Like its predecessors [UPP],
   [RTL], [ERTL], and [LTL], this language is untyped -- everything is
   a machine word.

   Contrary to LTL, instructions are no longer organized in a control
   flow graph. Instructions are organized in a sequence. Each
   instruction implicitly transfers control to its immediate
   successor, unless an explicit branch appears in the instruction. A
   new pseudo-instruction, [ILabel], appears. It is used to designate
   branch targets in the code. *)

open MIPSOps

(* Stack slots are as in [LTL]. *)

type slot = LTL.slot =
  | SlotLocal of offset
  | SlotIncoming of offset
  | SlotOutgoing of offset

(* Instructions. *)

and instructions =
    instruction list

and instruction = 

    (* Allocate a new stack frame to hold the current procedure's
       formals and locals. *)

  | INewFrame

    (* Release the current stack frame. *)

  | IDeleteFrame

    (* Copy the contents of a stack slot into a register. *)

  | IGetStack of MIPS.register * slot

    (* Copy the contents of a register into a stack slot. *)

  | ISetStack of slot * MIPS.register

    (* Load an immediate value into a register. *)

  | IConst of MIPS.register * int32

    (* Read a register, apply a unary operator, write to a register.
       Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * MIPS.register * MIPS.register

    (* Read two registers, apply a binary operator, write to a register.
       Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * MIPS.register * MIPS.register * MIPS.register

    (* Function call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The return value is
       received in MIPS register [$v0]. *)

  | ICall of Primitive.callee

    (* Tail call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The register [$ra] is
       not affected. A tail call does not return, so there is no
       successor. *)

  | ITailCall of Primitive.callee

    (* Memory read. Parameters are destination register, source
       (address) register, and a constant offset. *)

  | ILoad of MIPS.register * MIPS.register * offset

    (* Memory write. Parameters are address register, a constant
       offset, and value register. *)

  | IStore of MIPS.register * offset * MIPS.register

    (* Unconditional branch. *)

  | IGoto of Label.t

    (* Unary conditional branch. Parameters are a unary condition, the
       register that the condition is applied to, and the label that
       is jumped to if the condition holds. *)

  | IUnBranch of uncon * MIPS.register * Label.t

    (* Binary conditional branch. Parameters are a binary condition,
       the registers that the condition is applied to, and the label
       that is jumped to if the condition holds. *)

  | IBinBranch of bincon * MIPS.register * MIPS.register * Label.t

    (* Transfer control back to the caller. This means jumping to
       the address held in register [ra]. *)

  | IReturn

    (* The pseudo-instruction [ILabel l] designates its immediate
       successor as carrying label [l]. *)

  | ILabel of Label.t

(* Function definitions. *)

and procedure = {

  (* The total number of formal parameters that this function expects. *)

  formals: int32;

  (* The size (in bytes) of the local stack area that this function
     requires. *)

  locals: int32;

  (* A sequence of instructions. The sequence must begin with an
     [ILabel] instruction. *)

  code: instructions

}

(* Programs. *)

and program = {

  (* The space required by global variables, in bytes. *)

  globals: int32;

  (* A set of named function or procedure definitions. By convention,
     the program body is viewed as a procedure with no formal
     parameters, called "_main". *)

  defs: procedure StringMap.t

}

