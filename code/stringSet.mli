(* All of the operations offered by Objective Caml's standard [Set]
   module are available here. Consult [Set.S] in Objective Caml's
   documentation. *)

include Set.S with type elt = string

val of_list: elt list -> t

