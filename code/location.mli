(* ['a t] is the type of values tagged with a location, that is,
   records that contain a start position, an end position, and a value
   of type ['a]. *)

type 'a t

(* [make startpos endpos v] is the result of tagging value [v] with
   positions [startpos] and [endpos]. *)

val make: Lexing.position -> Lexing.position -> 'a -> 'a t

(* [content tv] is the actual content (that is, the raw value) of the
   tagged value [tv]. *)

val content: 'a t -> 'a

(* [startpos tv] and [endpos tv] are the start and positions recorded
   for value [tv]. *)

val startpos: 'a t -> Lexing.position
val endpos: 'a t -> Lexing.position

