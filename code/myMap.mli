(* This module augments the module [Map] of the Objective Caml standard
   library with a small number of additional functions. It is of little
   interest. *)

module type S =
  sig

    include Map.S

    val minimum: 'a t -> key * 'a
    (** [minimum m] returns the binding that corresponds to the minimum
        (smallest) key within the map [m]. If [m] is empty, [Not_found]
        is raised. *)

    val find_remove: key -> 'a t -> 'a * 'a t
    (** [find_remove x m] returns a pair of the current binding of [x]
        in [m], and a map containing the same bindings as [m], except
        for [x] which is unbound in the returned map. [Not_found] is
        raised if no binding for [x] exists. *)

    val update: key -> ('a -> 'a) -> 'a t -> 'a t
    (** If [m] maps [x] to [d], then [update x f m] maps [x] to [f d]
        and coincides with [m] elsewhere. A binding for [x] in [m]
        must exist. *)

    val restrict: (key -> bool) -> 'a t -> 'a t
    (** [restrict p m] is the restriction of the map [m] to only
        the keys that satisfy predicate [p]. *)

  end

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
