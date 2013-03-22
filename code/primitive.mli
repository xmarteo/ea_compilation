(* These are the functions provided by the runtime system. *)

type primitive =

    (* Unary functions for writing an integer out. *)

  | Write
  | Writeln

    (* Nullary function for reading an integer in. *)

  | Readln

    (* Unary function for allocating a memory block.
       Parameter is the desired block size in bytes.
       This operation is not available at the source
       level, because it is untyped. Instead, in [PP],
       the expression form [EArrayAlloc] is used. *)

  | Alloc

(* Callees are subjects of function calls. *)

type callee =

    (* User-defined function. *)

  | CUserFunction of string

    (* System-defined function. *)

  | CPrimitiveFunction of primitive

