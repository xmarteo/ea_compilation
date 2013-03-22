(* This module prints [ASM] programs. It is slightly more than just a
   printer: it also inserts code and directives that reserve space for
   global variables, initialize [$gp], transfer control to the entry
   procedure when the program is started, halt the program when the
   entry procedure returns, and provide code for the primitive
   operations. One might say that this is really a translation of
   [ASM] to MIPS assembly language. *)

open Print

val print_program: ASM.program printer

