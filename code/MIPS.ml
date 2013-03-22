(* A machine word is 4 bytes. *)

let word =
  4l

(* The MIPS CPU contains 32 general-purpose 32-bit registers
   that are numbered 0 to 31.

   Register 0, known as [$0], always contains the hardwired value 0.

   Register 1, known as [$at], is conventionally reserved for use by
   the assembler for expanding certain pseudo-instructions into actual
   hardware instructions. This means that we cannot use it.

   Registers 2 and 3, known as [$v0] and [$v1], are normally used to
   return values from functions. In our compiler, only [$v0] is used
   for this purpose. We use [$v1] as a reserved register for spilling
   and refer to it as [$st0].

   Registers 4 to 7, known as [$a0] to [$a3], are used to pass the
   first four arguments to functions. Remaining arguments are passed
   on the stack.

   Registers 8 to 15, 24 and 25, known as [$t0] to [$t9], are
   caller-saved registers.

   Registers 16 to 23, known as [$s0] to [$s7], are callee-saved
   registers.

   Registers 26 and 27, known as [$k0] and [$k1], are reserved for use
   by the operating system kernel.

   Register 28, known as [$gp] for global pointer, points into the
   middle of a 64K block of memory in the heap that holds constants
   and global variables.

   Register 29, known as [$sp] for stack pointer, points to the last
   location in use on the stack.

   Register 30, known as [$fp] for frame pointer, is used in our
   compiler as a reserved register for spilling. We refer to it as
   [$st1].

   Register 31, known as [$ra], contains the return address for the
   current function. It is written by the [jal] instruction. *)

type register =
    int

let equal : register -> register -> bool = (=)

module RegisterSet = struct

  include Set.Make (struct
    type t = register
    let compare = (-)
  end)

  let disjoint s1 s2 =
    is_empty (inter s1 s2)

  let of_list rs =
    List.fold_right add rs empty

end

module RegisterMap = struct

  include Map.Make (struct
    type t = register
    let compare = (-)
  end)

  let lift f s =
    RegisterSet.fold (fun x m ->
      add x (f x) m
    ) s empty

end

(* Naming convention. *)

let zero =
  0

let v0, v1, a0, a1, a2, a3 =
  2, 3, 4, 5, 6, 7

let t0, t1, t2, t3, t4, t5, t6, t7, t8, t9 =
  8, 9, 10, 11, 12, 13, 14, 15, 24, 25

let s0, s1, s2, s3, s4, s5, s6, s7 =
  16, 17, 18, 19, 20, 21, 22, 23

let gp =
  28

let sp, fp =
  29, 30

let ra =
  31

let st0, st1 =
  v1, fp

let print = function
  | 0 ->
      "zero"
  | 1 ->
      "at"
  | 2 ->
      "v0"
  | 4 ->
      "a0"
  | 5 ->
      "a1"
  | 6 ->
      "a2"
  | 7 ->
      "a3"
  | 8 ->
      "t0"
  | 9 ->
      "t1"
  | 10 ->
      "t2"
  | 11 ->
      "t3"
  | 12 ->
      "t4"
  | 13 ->
      "t5"
  | 14 ->
      "t6"
  | 15 ->
      "t7"
  | 24 ->
      "t8"
  | 25 ->
      "t9"
  | 16 ->
      "s0"
  | 17 ->
      "s1"
  | 18 ->
      "s2"
  | 19 ->
      "s3"
  | 20 ->
      "s4"
  | 21 ->
      "s5"
  | 22 ->
      "s6"
  | 23 ->
      "s7"
  | 3 ->
      "v1"
  | 30 ->
      "fp"
  | 28 ->
      "gp"
  | 29 ->
      "sp"
  | 31 ->
      "ra"
  | _ ->
      assert false

(* Calling convention. *)

let parameters =
  [ a0; a1; a2; a3 ]

let result =
  v0

let caller_saved =
  RegisterSet.of_list (
    if Settings.few then
      [ v0; a0; a1; a2; a3; t0; t1; ra ]
    else
      [ v0; a0; a1; a2; a3; t0; t1; t2; t3; t4; t5; t6; t7; t8; t9; ra ]
  )

let callee_saved =
  RegisterSet.of_list (
    if Settings.few then
      [ s0; s1 ]
    else
      [ s0; s1; s2; s3; s4; s5; s6; s7 ]
  )

let allocatable =
  (* The register [$zero] can be viewed as allocatable, but this requires building
     ad hoc interference edges -- see [Build]. In TD7, in order to simplify things,
     this register is made non-allocatable. *)
  RegisterSet.add zero (RegisterSet.union caller_saved callee_saved)

let registers =
  RegisterSet.union allocatable
    (RegisterSet.of_list [ gp; sp; st0; st1 ])

