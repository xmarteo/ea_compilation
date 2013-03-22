(* This module translates [LPP] to [PP]. The translation simply
   involves dropping locations. In a realistic compiler, [LPP] and
   [PP] would not be made two distinct languages, so no translation
   would be required. *)

val translate_program: LPP.program -> PP.program

