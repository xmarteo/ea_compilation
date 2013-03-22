open Primitive

let callee = function
  | CUserFunction f ->
      f
  | CPrimitiveFunction p ->
      match p with
      | Write ->
	  "write"
      | Writeln ->
	  "writeln"
      | Readln ->
	  "readln"
      | Alloc ->
	  "alloc"

