(* This module helps report errors or informational messages. *)

(* [log k msg] displays the message [msg stderr] if the verbosity
   level selected by the user is at least [k]. *)

val log: int -> (out_channel -> unit) -> unit

(* [error2 p1 p2 msg] displays the error message [msg], referring to
   the position range [p1--p2], and stops the program. *)

val error2: Lexing.position -> Lexing.position -> string -> 'a

(* [error tv msg] displays the error message [msg], referring to the
   position range carried by the tagged value [tv], and stops the
   program. *)

val error: 'a Location.t -> string -> 'b

(* [errors tvs msg] displays the error message [msg], referring to the
   position ranges carried by the tagged values [tvs], and stops the
   program. *)

val errors: 'a Location.t list -> string -> 'b

