open ERTL

(* If [i] can store a non-zero value into pseudo-register [r], then
   [nonzeroable i] returns the singleton [r], otherwise it returns the
   empty set. *)

let nonzeroable = function
  | ICall _
  | INewFrame _
  | IDeleteFrame _
  | ISetHwReg _
  | ISetStack _
  | IStore _
  | ISetGlobal _
  | IGoto _
  | IUnBranch _
  | IBinBranch _
  | IReturn _
  | ITailCall _
  | IConst (_, 0l, _) ->
      Register.Set.empty
  | IGetHwReg (r, _, _)
  | IGetStack (r, _, _)
  | IConst (r, _, _)
  | IUnOp (_, r, _, _)
  | IBinOp (_, r, _, _, _)
  | ILoad (r, _, _, _)
  | IGetGlobal (r, _, _) ->
      Register.Set.singleton r

