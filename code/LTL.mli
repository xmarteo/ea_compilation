(* This is the definition of the abstract syntax for the Location
   Transfer Language (LTL). Like its predecessors [UPP], [RTL], and
   [ERTL], this language is untyped -- everything is a machine
   word. As in RTL, instructions are organized in a control flow
   graph.

   In LTL, pseudo-registers disappear and are replaced with
   locations. A location is either a hardware register or a stack
   slot.

   The [IGetGlobal] and [ISetGlobal] instructions disappear and can
   now be expressed in terms of [ILoad] and [IStore]. The hardware
   register [$gp] can be assumed to hold the base address of the
   global storage area. *)

open MIPSOps

(* Stack slots are references to addresses within a stack frame. The
   stack frame is divided into three areas: the local area, for use by
   the current function only; the incoming area, which the caller uses
   to transmit parameters to the current function; and the outgoing
   area, which the current function uses to transmit parameters to its
   callee(s). Within each area, a nonnegative byte offset is used to
   indicate which slot is desired. *)

type slot =
  | SlotLocal of offset
  | SlotIncoming of offset
  | SlotOutgoing of offset

(* Instructions. Each instruction carries one or more continuation
   labels. *)

and instruction = 

    (* Allocate a new stack frame to hold the current procedure's
       formals and locals. *)

  | INewFrame of Label.t

    (* Release the current stack frame. *)

  | IDeleteFrame of Label.t

    (* Copy the contents of a stack slot into a register. *)

  | IGetStack of MIPS.register * slot * Label.t

    (* Copy the contents of a register into a stack slot. *)

  | ISetStack of slot * MIPS.register * Label.t

    (* Load an immediate value into a register. *)

  | IConst of MIPS.register * int32 * Label.t

    (* Read a register, apply a unary operator, write to a register.
       Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * MIPS.register * MIPS.register * Label.t

    (* Read two registers, apply a binary operator, write to a register.
       Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * MIPS.register * MIPS.register * MIPS.register * Label.t

    (* Function call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The return value is
       received in MIPS register [$v0]. *)

  | ICall of Primitive.callee * Label.t

    (* Tail call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The register [$ra] is
       not affected. A tail call does not return, so there is no
       successor. *)

  | ITailCall of Primitive.callee

    (* Memory read. Parameters are destination register, source
       (address) register, and a constant offset. *)

  | ILoad of MIPS.register * MIPS.register * offset * Label.t

    (* Memory write. Parameters are address register, a constant
       offset, and value register. *)

  | IStore of MIPS.register * offset * MIPS.register * Label.t

    (* No operation (just jump to the continuation label). *)

  | IGoto of Label.t

    (* Unary conditional branch. Parameters are a unary condition, the
       register that the condition is applied to, the label that is
       jumped to if the condition holds, and the label that is jumped
       to if the condition does not hold. *)

  | IUnBranch of uncon * MIPS.register * Label.t * Label.t

    (* Binary conditional branch. Parameters are a binary condition,
       the registers that the condition is applied to, the label that
       is jumped to if the condition holds, and the label that is
       jumped to if the condition does not hold. *)

  | IBinBranch of bincon * MIPS.register * MIPS.register * Label.t * Label.t

    (* Transfer control back to the caller. This means popping a stack
       frame and jumping to the address held in register [ra]. *)

  | IReturn

(* Function definitions. *)

and procedure = {

  (* The total number of formal parameters that this function expects. *)

  formals: int32;

  (* The size (in bytes) of the local stack area that this function
     requires. *)

  locals: int32;

  (* The control flow graph of the function is represented by an entry
     label and a mapping of labels to instructions. *)

  luniverse: Label.universe;
  entry: Label.t;
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

