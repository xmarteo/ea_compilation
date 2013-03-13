(* This module offers functions that count how many times each
   pseudo-register is used within a piece of [ERTL] code. This
   is used in [Coloring] to drive the spilling heuristics. *)

open ERTL

let lookup uses r =
  try
    Register.Map.find r uses
  with Not_found ->
    0

let count r uses =
  Register.Map.add r (lookup uses r + 1) uses

let examine_instruction _ instruction uses =
  match instruction with
  | INewFrame _
  | IDeleteFrame _
  | ICall _
  | ITailCall _
  | IGoto _
  | IReturn _ ->
      uses
  | IGetHwReg (r, _, _)
  | ISetHwReg (_, r, _)
  | IGetStack (r, _, _)
  | ISetStack (_, r, _)
  | IConst (r, _, _)
  | IGetGlobal (r, _, _)
  | ISetGlobal (_, r, _)
  | IUnBranch (_, r, _, _) ->
      count r uses
  | IUnOp (_, r1, r2, _)
  | ILoad (r1, r2, _, _)
  | IStore (r1, _, r2, _)
  | IBinBranch (_, r1, r2, _, _) ->
      count r1 (count r2 uses)
  | IBinOp (_, r, r1, r2, _) ->
      count r (count r1 (count r2 uses))

let examine_procedure proc =
  let uses =
    Label.Map.fold examine_instruction proc.graph Register.Map.empty
  in
  lookup uses

