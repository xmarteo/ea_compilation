(* This module offers an abstract type of control flow graph labels,
   used in all intermediate languages that follow [RTL].

   This module really is a copy of module [Atom]. However, [Label.t]
   is an abstract type -- it is not equal to [Atom.t] or
   [Register.t]. *)

include AtomSig.S

