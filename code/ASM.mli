(* This is the definition of the abstract syntax for a subset of the
   MIPS assembly language (ASM).

   In comparison with LIN, procedure boundaries are dissolved. Code
   labels are now strings. [IGetStack] and [ISetStack] instructions
   are replaced with [ILoad] and [IStore] instructions off the stack
   pointer register [$sp]. The [INewFrame] and [IDeleteFrame]
   operations are replaced with decrements and increments of [$sp]. It
   can still be assumed that [$gp] points to the global storage
   area. *)

open MIPSOps

(* Code labels are now strings and should be globally unique. *)

type label =
    string

(* Instructions. *)

and instructions =
    instruction list

and instruction = 

    (* Issue a comment into the assembler code. This is a
       pseudo-instruction. The Boolean flag tells whether the comment
       should be preceded with a line break. *)

  | IComment of bool * string

    (* No operation. *)

  | INop

    (* Load an immediate value into a register. *)

  | IConst of MIPS.register * int32

    (* Read a register, apply a unary operator, write to a register.
       Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * MIPS.register * MIPS.register

    (* Read two registers, apply a binary operator, write to a register.
       Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * MIPS.register * MIPS.register * MIPS.register

    (* Function call. *)

  | ICall of label

    (* Memory read. Parameters are destination register, source
       (address) register, and a constant offset. *)

  | ILoad of MIPS.register * MIPS.register * offset

    (* Memory write. Parameters are address register, a constant
       offset, and value register. *)

  | IStore of MIPS.register * offset * MIPS.register

    (* Unconditional branch. *)

  | IGoto of label

    (* Unary conditional branch. Parameters are a unary condition, the
       register that the condition is applied to, and the label that
       is jumped to if the condition holds. *)

  | IUnBranch of uncon * MIPS.register * label

    (* Binary conditional branch. Parameters are a binary condition,
       the registers that the condition is applied to, and the label
       that is jumped to if the condition holds. *)

  | IBinBranch of bincon * MIPS.register * MIPS.register * label

    (* Transfer control back to the caller. This means jumping to
       the address held in register [ra]. *)

  | IReturn

    (* The pseudo-instruction [ILabel l] designates its immediate
       successor as carrying label [l]. *)

  | ILabel of label

(* Programs. *)

and program = {

  (* The space required by global variables, in bytes. *)

  globals: int32;

  (* The procedure label at which execution should begin. *)

  entry: label;

  (* A sequence of instructions. *)

  code: instructions

}

