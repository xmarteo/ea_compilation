open Printf
open Print
open ASM

(* Some of the instructions that we emit are in fact pseudo-instructions. *)

let reg () r =
  sprintf "$%s" (MIPS.print r)

let lab () l =
  l

  (* $t9 is used as a reserved intermediary register *)
let print_instruction () = function
  | IComment (break, c) ->
      sprintf "%s# %s" (if break then "\n" else "") c
  | INop ->
      sprintf "nop"
  | IConst (r, i) -> (* we actually store 2n+1 *)
      sprintf "\
      li    $t9, 2       \n\
      li    %a, %ld      \n\
      mul   %a, %a, $t9  \n\
      addi  %a, 1        \n\
      " reg r i
        reg r reg r
        reg r
  | IUnOp (op, r1, r2) -> (* we convert from the "2n+1" form to n then back *)
      let s1 = sprintf "\
      sub %a, 1          \n\
      div %a, %a, $t9    \n\
      " reg r2 reg r2 reg r2
      and s2 = sprintf " \n\
      mul %a, %a, $t9    \n\
      addi %a, 1         \n\
      " reg r2 reg r2 reg r2
      in
      s1 ^ PrintOps.unop reg () (op, r1, r2) ^ s2
  | IBinOp (op, r, r1, r2) -> (* we convert from the "2n+1" form to n then back *)
      sprintf "\
      sub %a, 1          \n\
      div %a, %a, $t9    \n\
      sub %a, 1          \n\
      div %a, %a, $t9    \n\
      %s %a, %a, %a      \n\
      mul %a, %a, $t9    \n\
      addi %a, 1"
      reg r1 reg r1 reg r1
      reg r2 reg r2 reg r2
      (PrintOps.binop op) reg r reg r1 reg r2
      reg r reg r reg r
  | ICall l ->
      sprintf "jal   %s" l
  | ILoad (r1, r2, o) ->
      sprintf "lw    %a, %ld(%a)" reg r1 o reg r2
  | IStore (r1, o, r2) ->
      sprintf "sw    %a, %ld(%a)" reg r2 o reg r1
  | IGoto l ->
      sprintf "j     %s" l
  | IUnBranch (cond, r, l) -> (* we convert from the "2n+1" form to n *)
      sprintf "\
      sub %a, 1       \n\
      div %a, %a, $t9 \n\
      " reg r reg r reg r
      ^
      sprintf "\
      %a, %a          \n\
      " (PrintOps.uncon reg) (cond, r) lab l
  | IBinBranch (cond, r1, r2, l) -> (* we convert both from the "2n+1" form to n *)
      let s1 = sprintf "\
      sub %a, 1       \n\
      div %a, %a, $t9 \n\
      " reg r2 reg r2 reg r2
      and s2 = sprintf "\n\
      mul %a, %a, $t9 \n\
      addi %a, 1      \n\
      " reg r2 reg r2 reg r2
      in
      s1 ^ sprintf "%s %a, %a, %s" (PrintOps.bincon cond) reg r1 reg r2 l ^ s2
  | IReturn ->
      sprintf "jr    $ra"
  | ILabel l ->
      sprintf "%s:" l

let print_program () p =

  (* Allocate space for the globals. *)

  let globals =
    sprintf "\
      .data                  \n\
      globals:               \n\
        .space %ld           \n\
      heap_info:
	.space 12
    " p.globals
  in

  (* When invoked without the [-notrap] option, [spim] inserts a code
     at [__start] that invokes the procedure [main] and exits after
     the procedure has returned. It is up to us to define [main]. All
     we have to do is initialize [$gp] and perform a tail call to
     [_main], so that [_main] will return directly to [__start]. *)

  let start =
    sprintf "\
      .text                 \n\
      main:                 \n\
      la    $gp, globals    \n\
      # allocate the heap   \n\
      li    $a0, %ld        \n\
      li    $v0, 9          \n\
      syscall               \n\
      # saving heap size    \n\
      move  $a2, $a0        \n\
      # computing heap_info \n\
      la    $a0, heap_info  \n\
      # beginning of heap   \n\
      sw    $v0, 0($a0)     \n\
      # current position    \n\
      sw    $v0, 4($a0)     \n\
      # end of heap         \n\
      add   $a2, $a2, $v0   \n\
      sw    $a2, 8($a0)     \n\
      j     %s              \n\
    " 65536l p.entry

  in

  (* Include implementations of the primitive operations. *)

  let support =
    "\
      write:                \n\
      li    $v0, 1          \n\
      syscall               \n\
      j     $ra             \n\
      .data                 \n\
      nl:                   \n\
      .asciiz \"\\n\"       \n\
      .align 2              \n\
      .text                 \n\
      writeln:              \n\
      sub   $a0, 1          \n\
      div   $a0, $a0, $t9   \n\
      li    $v0, 1          \n\
      syscall               \n\
      la    $a0, nl         \n\
      li    $v0, 4          \n\
      syscall               \n\
      j     $ra             \n\
      readln:               \n\
      li    $v0, 5          \n\
      syscall               \n\
      j     $ra             \n\
      alloc:                \n\
      la    $a1, heap_info  \n\
      # adding extra word   \n\
      addi  $t1, $a0, 4     \n\
      # current position    \n\
      lw    $v0, 4($a1)     \n\
      move  $t0, $v0        \n\
      # end of the heap     \n\
      lw    $a2, 8($a1)     \n\
      add   $v0, $t1, $v0   \n\
      # must have v0<=a2    \n\
      bgt   $v0, $a2, oom   \n\
      sw    $v0, 4($a1)     \n\
      # length is written   \n\
      move  $v0, $t0        \n\
      div   $a0, $a0, 4     \n\
      sw    $a0, 0($v0)     \n\
      addi  $v0, $v0, 4     \n\
      j     $ra             \n\
      .data                 \n\
      error_str:            \n\
      .asciiz \"out0fMem\"  \n\
      .align 2              \n\
      .text                 \n\
      oom:                  \n\
      # means out of mem.   \n\
      la $a0, error_str     \n\
      li $v0, 4             \n\
      syscall               \n\
      li $v0, 10            \n\
      syscall               \n\
      j     $ra             \n\
    "
  in

  (* The bulk of the code. *)

  let bulk =
    sprintf "%a" (termlist nl print_instruction) p.code
  in

  globals ^ start ^ support ^ bulk

