(* This is the definition of the abstract syntax for the Explicit
   Register Transfer Language (ERTL).

   This language is very much like RTL, except the calling convention
   has been made explicit. That is, functions and procedures no longer
   accept parameters and return results via a high-level mechanism;
   instead, they do so via either hardware registers or stack
   slots.

   Functions and procedures no longer magically return to their
   caller: instead, a new [IReturn] instruction appears, whose
   semantics is to transfer control to the address stored in [$ra].

   The [ICall] instruction implicitly writes [$ra] before transferring
   control to the caller.

   Functions and procedures are no longer explicitly distinguished:
   functions are simply procedures that happen to write the hardware
   register [$v0]. There only remains a distinction at [IReturn]
   instructions (see below).

   Two new instructions, [IGetHwReg] and [ISetHwReg], appear for
   reading and writing hardware registers. Two new instructions,
   [IGetStack] and [ISetStack], appear for reading and writing stack
   slots.

   Two new instructions, [INewFrame] and [IDeleteFrame], appear in
   order to allocate and release stack frames. They will be
   translated, in the final assembly code, to arithmetic on [$sp].

   Note that [IGetStack] and [ISetStack] only make sense relative to
   the current frame, that is, they must not be used outside the
   [INewFrame] / [IDeleteFrame] pair. In fact, the same can be said of
   all accesses to pseudo-registers, because a pseudo-register can be
   spilled and turned into a stack slot in a later phase. *)

open MIPSOps

(* Stack slots are references to addresses within a stack frame. The
   stack frame is divided into two areas: the incoming area, which the
   caller uses to transmit parameters to the current function; and the
   outgoing area, which the current function uses to transmit
   parameters to its callee(s). Within each area, a nonnegative byte
   offset is used to indicate which slot is desired. *)

type slot =
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

    (* Copy the contents of a hardware register into a pseudo-register. *)

  | IGetHwReg of Register.t * MIPS.register * Label.t

    (* Copy the contents of a pseudo-register into a hardware register. *)

  | ISetHwReg of MIPS.register * Register.t * Label.t

    (* Copy the contents of a stack slot into a register. *)

  | IGetStack of Register.t * slot * Label.t

    (* Copy the contents of a register into a stack slot. *)

  | ISetStack of slot * Register.t * Label.t

    (* Load an immediate value into a register. *)

  | IConst of Register.t * int32 * Label.t

    (* Read a register, apply a unary operator, write to a register.
       Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * Register.t * Register.t * Label.t

    (* Read two registers, apply a binary operator, write to a register.
       Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * Register.t * Register.t * Register.t * Label.t

    (* Function call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The return value is
       received in MIPS register [$v0]. The total number of actual
       arguments is recorded in the instruction. The address of the
       next instruction is written into [$ra] as a side effect. *)

  | ICall of Primitive.callee * int32 * Label.t

    (* Tail call. The first four actual arguments are passed in
       MIPS registers [$a0] to [$a3]. Remaining arguments are passed
       on the stack, in the outgoing area. The total number of actual
       arguments is recorded in the instruction. The register [$ra] is
       not affected. A tail call does not return, so there is no
       successor. *)

  | ITailCall of Primitive.callee * int32

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

    (* Transfer control to the address stored in [$ra].

       The Boolean flag carried by [IReturn] does not affect the
       semantics of the instruction, but tells whether the caller is
       expected to read the value of the hardware register
       [MIPS.result]. (In other words, the flag is [true] for
       functions, [false] for procedures.) This is exploited in the
       liveness analysis, where [MIPS.result] is considered live just
       before [IReturn true], and dead just before [IReturn false]. *)

  | IReturn of bool
  
    (* Array allocation. The calling convention has been explicited, so
       at this point length is already in a0 and result will be in v0. 
       No arguments. *)
    
  | INewArray of Label.t

(* Function definitions. *)

and procedure = {

  (* The explicit list of formal arguments that was present in [RTL]
     disappears. Instead, only the total number of formal parameters
     is recorded. The first four formal parameters are received in
     MIPS registers [$a0] to [$a3]. Remaining formal parameters are
     received on the stack, in the incoming area. The return value is
     passed in MIPS register [$v0]. *)

  formals: int32;

  (* A set of all pseudo-registers used by the function. *)

  runiverse: Register.universe;
  locals: Register.Set.t;

  (* The control flow graph of the function is represented by an entry
     label and a mapping of labels to instructions. The exit label
     disappears: the [IReturn] instruction makes it unnecessary. *)

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

