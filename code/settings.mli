(* These flags tell which intermediate programs should be
   displayed. *)

val dpp: bool
val dupp: bool
val drtl: bool
val dcse: bool
val dertl: bool
val dltl: bool
val dlin: bool
val dasm: bool

(* This flag tells whether liveness information should be
   displayed. *)

val dlive: bool

(* This tells whether an interference graph should be displayed, and,
   if so, for which procedure. *)

val dgraph: string option

(* This tells whether hardware register allocation and stack slot
   allocation should be made verbose, and, if so, for which procedure
   in particular. *)

val dcolor: string option
val dspill: string option

(* This flag pretends that the processor has fewer registers. Among
   other things, this helps visualize interference graphs. *)

val few: bool

(* This flag prevents common sub-expression elimiation (CSE). *)

val nocse: bool

(* These flags tell which intermediate programs should be
   interpreted. *)

val ipp: bool
val iupp: bool
val irtl: bool
val icse: bool
val iertl: bool
val iltl: bool
val ilin: bool

(* This integer value indicates how verbose we should be.
   It is used by [Error.log]. *)

val verbose: int

(* This function, controlled by the [-delimit] flag, possibly inserts
   begin and end markers around its second argument. This is used to
   delimit procedures in displayed code, and is useful for exporting
   code fragments into course slides. *)

val delimit: string -> string -> string

(* This is the name of the file that we should compile. *)

val filename: string

