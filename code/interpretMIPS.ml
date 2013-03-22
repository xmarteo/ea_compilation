open Integer
open MIPSOps

let b2i = function
  | true ->
      1l
  | false ->
      0l

let unop op i =
  match op with
  | UOpAddi j ->
      i + j
  | UOpSlli j ->
      i lsl (Int32.to_int j)
  | UOpSlti j ->
      b2i (i < j)

let bop2iop op i1 i2 =
  b2i (op i1 i2)

let binop = function
  | OpAdd ->
      (+)
  | OpSub ->
      (-)
  | OpMul ->
      ( * )
  | OpDiv ->
      (/)
  | OpLt ->
      bop2iop (<)
  | OpLe ->
      bop2iop (<=)
  | OpGt ->
      bop2iop (>)
  | OpGe ->
      bop2iop (>=)
  | OpEq ->
      bop2iop (=)
  | OpNe ->
      bop2iop (<>)

let uncon cond i =
  match cond with
  | UConGez ->
      i >= 0l
  | UConGtz ->
      i > 0l
  | UConLez ->
      i <= 0l
  | UConLtz ->
      i < 0l

let bincon cond i1 i2 =
  match cond with
  | ConEq ->
      i1 = i2
  | ConNe ->
      i1 <> i2

