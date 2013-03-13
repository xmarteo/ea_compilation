module type S =
  sig
    include Map.S
    val minimum: 'a t -> key * 'a
    val find_remove: key -> 'a t -> 'a * 'a t
    val update: key -> ('a -> 'a) -> 'a t -> 'a t
    val restrict: (key -> bool) -> 'a t -> 'a t
  end

module Make(Ord: Map.OrderedType) = struct

  include Map.Make(Ord)

  (* Finding the minimum key in a map. *)

  (* Objective Caml 3.12 offers a library function to do this, but this
     function was absent in earlier versions, hence the following hack. *)

  exception Minimum

  let minimum m =
    let r = ref None in
    try
      iter (fun key data -> r := Some (key, data); raise Minimum) m;
      (* If we are still here, then the map is empty. *)
      raise Not_found
    with Minimum ->
      (* If we get here, then we have found the first element. *)
      match !r with
      | Some binding ->
	binding
      | None ->
  	assert false

  (* Finding an element and removing it. This could be done in a single
     traversal if we had access to the internals of Objective Caml's [Map]
     module, but we don't. *)

  let find_remove x m =
    find x m, remove x m

  (* Updating the data associated with an element in one single
     traversal. This could be done in a single traversal if we had access to
     the internals of Objective Caml's [Map] module, but we don't. *)

  let rec update x f m =
    let data = find x m in
    let data' = f data in
    if data == data' then m else add x data' m

  (* Restricting the domain of a map. *)

  let restrict p m =
    fold (fun x d m ->
      if p x then
        add x d m
      else
        m
    ) m empty

end
