(* This module builds an interference graph for an [ERTL] procedure.
   This is done by running a liveness analysis and exploiting its
   result. [build] returns both the result of the liveness analysis
   and the interference graph. *)

val build: ERTL.procedure -> Liveness.valuation * Interference.graph

