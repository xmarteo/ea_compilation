include Map.Make(String)

let addm m1 m2 =
  fold add m1 m2

exception Duplicate of key

let of_association_list l =
  List.fold_left (fun m (key, data) ->
    if mem key m then
      raise (Duplicate key)
    else
      add key data m
  ) empty l

let domain m =
  fold (fun key _ s ->
    StringSet.add key s
  ) m StringSet.empty

let to_association_list m =
  fold (fun key data l ->
    (key, data) :: l
  ) m []

let lift f s =
  StringSet.fold (fun x m ->
    add x (f x) m
  ) s empty

