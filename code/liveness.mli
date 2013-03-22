(* This module performs liveness analysis over the control flow
   graph of a single [ERTL] procedure. *)

(* In the following, a ``variable'' means a pseudo-register or an
   allocatable hardware register. *)

(* We collect liveness information about variables. We are not
   interested in collecting information about non-allocatable hardware
   registers such as [$gp], [$sp], etc. so they are considered never
   defined and never used as far as [ERTL] is concerned. *)

open ERTL

(* This is the lattice of sets of variables. *)

module L : sig
  type t = Register.Set.t * MIPS.RegisterSet.t
  val bottom: t
  val join: t -> t -> t
  val equal: t -> t -> bool
  val diff: t -> t -> t
  val psingleton: Register.t -> t
  val hsingleton: MIPS.register -> t
end

(* [defined i] is the set of variables defined at (written by)
   instruction [i]. *)

val defined: instruction -> L.t

(* A valuation is a function that maps a program point (a control flow
   graph label) to the set of variables that are live after that
   point. *)

type valuation =
    Label.t -> L.t

(* [analyze proc] analyzes the procedure [proc] and returns a valuation. *)

val analyze: procedure -> valuation

(* Pure instructions whose destination pseudo-register is dead after the
   instruction will be eliminated during the translation of [ERTL] to [LTL].
   [eliminable liveafter i] returns [Some successor], where [successor] is
   [i]'s single successor, if instruction [i] is eliminable. Otherwise, it
   returns [None]. *)

val eliminable: L.t -> instruction -> Label.t option

