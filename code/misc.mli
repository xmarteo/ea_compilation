(* [combine] turns a pair of lists into a list of pairs. It never
   fails: the length of the output list is the minimum of the lengths
   of the input lists. *)

val combine: 'a list -> 'b list -> ('a * 'b) list

(* [subtract xs1 xs2] returns the list [xs1] deprived of as many
   elements as there are in the list [xs2]. *)

val subtract: 'a list -> 'b list -> 'a list

(* [mirror] reverses the order of the pair components in a list
   of pairs. *)

val mirror: ('a * 'b) list -> ('b * 'a) list

(* [length l] is the length of the list [l]. *)

val length: 'a list -> int32

(* [prefix k xs] returns the prefix of length [k] of the list [xs].
   If [xs] has length less than [k], [xs] is returned. *)

val prefix: int32 -> 'a list -> 'a list

(* [memoize f] produces a memoizing version of the function [f].
   It requires the domain of [f] to support generic equality. *)

val memoize: ('a -> 'b) -> ('a -> 'b)

(* A facility for generating integer sequences of the form 0, 4, 8, etc.
   The call [multiples k] produces a *function* which, every time it is
   invoked, produces the next multiple of [k]. *)

val multiples: int32 -> (unit -> int32)

