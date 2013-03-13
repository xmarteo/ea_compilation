(* All of the operations offered by Objective Caml's standard [Map]
   module are available here. Consult [Map.S] in Objective Caml's
   documentation. *)

include Map.S with type key = string

(* [addm m1 m2] adds the bindings in the map [m1] to the map [m2],
   overriding any previous binding if [m1] and [m2] have common
   keys. *)

val addm: 'a t -> 'a t -> 'a t

(* [of_association_list l] turns an association list into a map.
   It raises [Duplicate key] if a key appears twice in the list.
   [to_association_list m] turns a map into an association list. *)

exception Duplicate of key
val of_association_list: (key * 'a) list -> 'a t
val to_association_list: 'a t -> (key * 'a) list

(* [domain m] is the domain of the map [m], that is, the set of keys
   that are defined. *)

val domain: 'a t -> StringSet.t

(* [lift f s] turns the set [s] into a map where every element [x]
   is mapped to [f x]. *)

val lift: (key -> 'a) -> StringSet.t -> 'a t

