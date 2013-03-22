open Integer
open Primitive

(* In order to translate [IGetStack] and [ISetStack], one must
   understand the actual layout of stack frames. 

   The MIPS stack grows towards low addresses. I will speak of the
   high limit address of the stack frame (that is, the address of the
   previous stack frame) as the ``top of the stack frame'' and of the
   low limit address of the stack frame as the ``bottom of the stack
   frame''.

   The top of the stack frame is the initial value of [$sp] when the
   procedure is entered. The bottom of the stack frame is the new
   value of [$sp] that the procedure installs. The difference between
   the two corresponds to the parameters that are passed on the stack
   and to the procedure's local stack storage area. We refer to this
   value as the size of the frame.

   Thus, the top of the stack frame is at [$sp + locals + formals],
   where [locals] is the size of the local storage area and [formals]
   is the size of the formal parameters area, both expressed in bytes.
   The parameters are between [$sp + locals] and [$sp + locals +
   formals]. Local storage lies between [$sp] and [$sp + locals].

   A [SlotIncoming] stack slot is translated into an offset into the
   frame's parameters area. A [SlotLocal] stack slot is translated
   into an offset into the frame's local storage area. A
   [SlotOutgoing] stack slot is translated into an offset into the
   callee's parameters area, which lies below [$sp]. We translate
   higher offsets to lower addresses: this is made necessary by the
   fact that the size of the outgoing area is not recorded in our
   instructions. *)

(* [locals proc] is the size (in bytes) of [proc]'s local stack area. *)

let locals (proc : LIN.procedure) : int32 =
  proc.LIN.locals

(* [formals proc] is the size (in bytes) of [proc]'s incoming stack area.  It
   consists of only the parameters that are not passed in hardware
   registers. *)

let formals (proc : LIN.procedure) : int32 =
  MIPS.word * (max 0l (proc.LIN.formals - Misc.length MIPS.parameters))

(* [translate_slot proc slot] translates [slot] into an offset off
   [$sp]. *)

let translate_slot (proc : LIN.procedure) (slot : LIN.slot) : int32 =
  match slot with
  | LIN.SlotLocal offset ->
      locals proc - (offset + MIPS.word)
  | LIN.SlotIncoming offset ->
      locals proc + formals proc - (offset + MIPS.word)
  | LIN.SlotOutgoing offset ->
      - (offset + MIPS.word)

(* [adjust offset] generates an [ASM] instruction that adjusts the
   stack pointer [sp] by [offset]. *)

let adjust (offset : int32) : ASM.instruction =
  if offset = 0l then
    ASM.INop
  else
    ASM.IUnOp (MIPSOps.UOpAddi offset, MIPS.sp, MIPS.sp)

(* [prim2label] maps primitive operations to the names under which
   they appear in the assembly code. These names must of course
   correspond to labels in the assembly program that is produced.
   This is ensured in [PrintASM] by appending standard definitions for
   these labels to the [ASM] program. *)

let prim2label = function
  | Write ->
      "write"
  | Writeln ->
      "writeln"
  | Readln ->
      "readln"
  | Alloc ->
      "alloc"

(* [proc2label p f] maps the procedure name [f] to a procedure entry
   label. *)

let proc2label (p : LIN.program) f =
  let proc = StringMap.find f p.LIN.defs in
  match proc.LIN.code with
  | LIN.ILabel entry :: _ ->
      entry
  | _ ->
      assert false

(* [translate_instruction p proc i] translates the instruction [i].
   The program [p] is used when translating procedure calls: it
   provides a mapping of procedure names to labels. The current
   procedure [proc] is used when translating stack accesses. Labels
   are mapped to globally unique strings using [Label.print]. *)

let translate_instruction
    (p : LIN.program)
    (proc : LIN.procedure)
    (instruction : LIN.instruction)
    : ASM.instruction
    =
  match instruction with

  | LIN.INewFrame ->
      adjust (-(locals proc + formals proc))

  | LIN.IDeleteFrame ->
      adjust (locals proc + formals proc)

  | LIN.IGetStack (r, slot) ->
      let offset = translate_slot proc slot in
      ASM.ILoad (r, MIPS.sp, offset)

  | LIN.ISetStack (slot, r) ->
      let offset = translate_slot proc slot in
      ASM.IStore (MIPS.sp, offset, r)

  | LIN.IConst (r, i) ->
      ASM.IConst (r, i)

  | LIN.IUnOp (op, r1, r2) ->
      ASM.IUnOp (op, r1, r2)

  | LIN.IBinOp (op, r, r1, r2) ->
      ASM.IBinOp (op, r, r1, r2)

  | LIN.ICall (CUserFunction f) ->
      ASM.ICall (Label.print (proc2label p f))

  | LIN.ICall (CPrimitiveFunction p) ->
      ASM.ICall (prim2label p)

  | LIN.ITailCall (CUserFunction f) ->
      ASM.IGoto (Label.print (proc2label p f))

  | LIN.ITailCall (CPrimitiveFunction p) ->
      ASM.IGoto (prim2label p)

  | LIN.ILoad (r1, r2, o) ->
      ASM.ILoad (r1, r2, o)

  | LIN.IStore (r1, o, r2) ->
      ASM.IStore (r1, o, r2)

  | LIN.IGoto l ->
      ASM.IGoto (Label.print l)

  | LIN.IUnBranch (cond, r, l) ->
      ASM.IUnBranch (cond, r, Label.print l)

  | LIN.IBinBranch (cond, r1, r2, l) ->
      ASM.IBinBranch (cond, r1, r2, Label.print l)

  | LIN.IReturn ->
      ASM.IReturn

  | LIN.ILabel l ->
      ASM.ILabel (Label.print l)

(* [cons i is] prepends instruction [i] in front of the instruction
   sequence [is], unless it is an [INop] instruction. *)

let cons i is =
  match i with
  | ASM.INop ->
      is
  | _ ->
      i :: is

(* [translate_procedure p f proc instructions] prepends a translation
   of procedure [proc] in front of the instruction sequence
   [instructions]. A couple of comments are inserted at the beginning
   and end of the procedure to help identify it. *)

let translate_procedure
    (p : LIN.program)
    (name : string)
    (proc : LIN.procedure)
    (instructions : ASM.instructions)
    : ASM.instructions
    =
  ASM.IComment (true, "begin " ^ name) ::
  List.fold_right (fun instruction instructions ->
    cons (translate_instruction p proc instruction) instructions
  ) proc.LIN.code (
  ASM.IComment (false, "end " ^ name) ::
  instructions
  )

(* [translate_program p] translates the program [p]. *)

let translate_program (p : LIN.program) : ASM.program = {
  ASM.globals = p.LIN.globals;
  ASM.entry   = Label.print (proc2label p "_main");
  ASM.code    = StringMap.fold (translate_procedure p) p.LIN.defs []
}

