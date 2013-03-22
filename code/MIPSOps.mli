(* These type definitions document the target processor's operators
   and branch conditions. Here, the target processor is the MIPS. *)

(* The MIPS manual explains that some instructions are actual machine
   instructions, while others are pseudo-instructions, which are
   expanded away by the assembler. Do we need to be aware of the
   distinction?

   Ignoring the distinction is interesting. There are several
   processors in the MIPS family, and the distinction between actual
   instructions and pseudo-instructions might vary with the
   processor. For instance, a future member of the MIPS family might
   implement more instructions in hardware than its predecessors.

   On the other hand, ignoring the distinction means that we cannot
   use the hardware register [$at], which is reserved by the assembler
   for translating some pseudo-instructions into actual instructions.

   Our approach is to follow standard practice and to exploit
   pseudo-instructions when desired. This means that we cannot use
   register [$at]. *)

(* Immediate constants, used in the definition of some operators. They
   must fit in 16 bits. *)

type immediate16 =
   int32

(* Offsets, used as part of addressing modes. They are measured in
   bytes and must fit in 16 bits. *)

type offset =
   immediate16

(* Unary (integer arithmetic) operators. *)

type unop =
  | UOpAddi of immediate16
  | UOpSlli of immediate16 (* shift left logical *)
  | UOpSlti of immediate16 (* set on less than immediate *)

(* Binary (integer arithmetic or integer comparison) operators. Among
   the comparison operators, only [OpLt] corresponds to a MIPS binary
   comparison instruction, namely [slt]. All others correspond to
   pseudo-instructions. They are exploited because they are
   convenient. *)

type binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpLt                                       
  | OpLe
  | OpGt
  | OpGe
  | OpEq
  | OpNe 

(* Unary branch conditions. *)

type uncon =

    (* Greater than or equal to zero. *)

  | UConGez

    (* Greater than zero. *)

  | UConGtz

    (* Less than or equal to zero. *)

  | UConLez

    (* Less than zero. *)

  | UConLtz

(* Binary branch conditions. *)

and bincon =

    (* Equal. *)

  | ConEq

    (* Not equal. *)

  | ConNe

