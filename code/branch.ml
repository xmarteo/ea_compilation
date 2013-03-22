open LTL

let compress entry graph =

  (* Build a table that maps every graph label to a distinct ``point''
     in the sense of [UnionFind]. *)

  let points =
    Label.Map.mapi (fun label _ ->
      UnionFind.fresh label
    ) graph
  in

  let lookup label =
    try
      Label.Map.find label points
    with Not_found ->
      assert false
  in

  (* For every [IGoto] instruction, make the source label an alias for
     the target label, unless the former is already an alias for the
     latter (which means that the graph contains a cycle of [IGoto]
     instructions). *)

  Label.Map.iter (fun source i ->
    let source = lookup source in
    match i with
    | IGoto target ->
	let target = lookup target in
	if UnionFind.equivalent source target then
	  assert false (* can happen if the program contains an empty infinite loop, but let's ignore that *)
	else
	  UnionFind.union source target
    | _ ->
	()
  ) graph;

  (* Transform the graph by replacing every label with its representative. *)

  let rep label =
    UnionFind.find (lookup label)
  in

  rep entry, Label.Map.map (function
    | INewFrame l ->
	INewFrame (rep l)
    | IDeleteFrame l ->
	IDeleteFrame (rep l)
    | IGetStack (r, s, l) ->
	IGetStack (r, s, rep l)
    | ISetStack (s, r, l) ->
	ISetStack (s, r, rep l)
    | IConst (r, k, l) ->
	IConst (r, k, rep l)
    | IUnOp (op, r1, r2, l) ->
	IUnOp (op, r1, r2, rep l)
    | IBinOp (op, r1, r2, r3, l) ->
	IBinOp (op, r1, r2, r3, rep l)
    | ICall (c, l) ->
	ICall (c, rep l)
    | ILoad (r1, r2, o, l) ->
	ILoad (r1, r2, o, rep l)
    | IStore (r1, o, r2, l) ->
	IStore (r1, o, r2, rep l)
    | INewArray l ->
	INewArray (rep l)
    | IGoto l ->
	IGoto (rep l) (* instruction will be unreachable *)
    | IUnBranch (c, r, l1, l2) ->
	IUnBranch (c, r, rep l1, rep l2)
    | IBinBranch (c, r1, r2, l1, l2) ->
	IBinBranch (c, r1, r2, rep l1, rep l2)
    | IReturn ->
	IReturn
    | ITailCall callee ->
	ITailCall callee
  ) graph

