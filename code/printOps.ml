open MIPSOps
open Printf

(* Some of the instructions that we emit are in fact pseudo-instructions. *)

(* We use [addu], [addiu], and [subu] instead of [add], [addi], and
   [sub]. The only difference is that the former never generate
   overflow exceptions. This is what we desire, since the semantics
   of Pseudo-Pascal says nothing about overflow exceptions. Overflow
   is silent. *)

let unop reg f (op, dst, src) =
  match op with
  | UOpAddi 0l ->
      sprintf "move  %a, %a" reg dst reg src (* pseudo-instruction *)
  | UOpAddi i ->
      sprintf "addiu %a, %a, %ld" reg dst reg src i
  | UOpSlli i ->
      sprintf "sll   %a, %a, %ld" reg dst reg src i
  | UOpSlti i ->
      sprintf "slti  %a, %a, %ld" reg dst reg src i

let binop = function
  | OpAdd ->
      "addu "
  | OpSub ->
      "subu "
  | OpMul ->
      "mul  "
  | OpDiv ->
      "div  " (* pseudo-instruction *)
  | OpLt ->
      "slt  "
  | OpLe ->
      "sle  " (* pseudo-instruction *)
  | OpGt ->
      "sgt  " (* pseudo-instruction *)
  | OpGe ->
      "sge  " (* pseudo-instruction *)
  | OpEq ->
      "seq  " (* pseudo-instruction *)
  | OpNe ->
      "sne  " (* pseudo-instruction *)

let uncon reg f (cond, src) =
  match cond with
  | UConGez ->
      sprintf "bgez  %a" reg src
  | UConGtz ->
      sprintf "bgtz  %a" reg src
  | UConLez ->
      sprintf "blez  %a" reg src
  | UConLtz ->
      sprintf "bltz  %a" reg src

let bincon = function
  | ConEq ->
      "beq  "
  | ConNe ->
      "bne  "
