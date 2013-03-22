open Integer

let rec combine xs1 xs2 =
  match xs1, xs2 with
  | [], _
  | _, [] ->
      []
  | x1 :: xs1, x2 :: xs2 ->
      (x1, x2) :: combine xs1 xs2

let rec subtract xs1 xs2 =
  match xs1, xs2 with
  | [], _ ->
      []
  | _, [] ->
      xs1
  | _ :: xs1, _ :: xs2 ->
      subtract xs1 xs2

let mirror l =
  List.map (fun (x, y) -> (y, x)) l

let length l =
  Int32.of_int (List.length l)

let rec prefix k l =
  match k, l with
  | 0l, _
  | _, [] ->
      []
  | _, x :: xs ->
      x :: prefix (k - 1l) xs

let memoize f =
  let table = Hashtbl.create 131 in
  fun key ->
    try
      Hashtbl.find table key
    with Not_found ->
      let data = f key in
      Hashtbl.add table key data;
      data

let multiples (k : int32) : unit -> int32 =
  (* Allocate an internal counter, which contains the next number
     that will be delivered. *)
  let c = ref 0l in
  (* Return a function which increments this counter and returns
     its previous value. *)
  fun () ->
    let m = !c in
    c := m + k;
    m

