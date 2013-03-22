(* This module optimizes [LTL] code by suppressing all [IGoto]
   instructions. In short, every instruction whose successor is an
   [IGoto] instruction is modified so that its successor is the
   successor of the [IGoto] instruction, and this is repeated until no
   reachable [IGoto] instructions remain. Unreachable [IGoto]
   instructions remain in the graph, but will be implicitly eliminated
   in the translation of [LTL] to [LIN]. *)

val compress: Label.t -> LTL.graph -> Label.t * LTL.graph

