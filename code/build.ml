open ERTL
open Interference

let build (proc : procedure) =

  (* Perform liveness analysis. *)

  let liveafter = Liveness.analyze proc in

  (* Create an interference graph whose vertices are the procedure's
     pseudo-registers. This graph initially has no edges. *)

  let graph = create proc.locals in

  (* Iterate over all instructions in the control flow graph and populate the
     interference graph with interference and preference edges. *)

  let graph =
    Label.Map.fold (fun label i graph ->
      let live = liveafter label in
      match Liveness.eliminable live i with

      | Some _ ->

	  (* This instruction is eliminable and should be ignored. Eliminable
	     instructions have not been eliminated yet, because we are still
	     in between ERTL and LTL. They *will* be eliminated soon, though,
	     so there is no reason to take them into account while building
	     the interference graph. *)

	  graph

      | None ->

	  (* Create interference edges. The general rule is, every
	     pseudo-register or hardware register that is defined (written) by
	     an instruction interferes with every pseudo-register or hardware
	     register (other than itself) that is live immediately after the
	     instruction executes.

	     An exception to the general rule can be made for move
	     instructions. In a move instruction, we do not need the source
	     and destination pseudo-registers to be assigned distinct hardware
	     registers, since they contain the same value -- in fact, we would
	     like them to be assigned the same hardware register. So, for a
	     move instruction, we let the register that is defined (written)
	     interfere with every pseudo-register, other than itself *and
	     other than the source pseudo-register*, that is live immediately
	     after the instruction executes. This optimization is explained in
	     Chapter 10 of Appel's book (p. 221).

	     This special case is only a hack that works in many cases. There
	     are cases where it doesn't suffice. For instance, if two
	     successive move instructions have the same source [r0], then
	     their destinations [r1] and [r2] *will* be considered as
	     interfering, even though it would be in fact be correct and
	     desirable to map both of them to the same hardware register. A
	     more general solution would be to perform a static analysis that
	     determines, for every program point, which pseudo-registers
	     definitely hold the same value, and to exploit this information
	     to build fewer interference edges. *)

	  let defined = Liveness.defined i in
	  let exceptions =
	    match i with
	    | IUnOp (MIPSOps.UOpAddi 0l, _, sourcer, _)
	    | ISetHwReg (_, sourcer, _) ->
		 Liveness.L.psingleton sourcer
	    | IGetHwReg (_, sourcehwr, _) ->
		Liveness.L.hsingleton sourcehwr
	    | _ ->
		Liveness.L.bottom
	  in
	  let graph =
	    mki graph (Liveness.L.diff live exceptions) defined
	  in

	  (* Create preference edges between pseudo-registers. Two registers
	     should preferably be assigned the same color when they are
	     related by a move instruction, so that the move instruction can
	     be eliminated. *)
   
	  let graph =
	    match i with
	    | IUnOp (MIPSOps.UOpAddi 0l, r1, r2, _) ->
		mkppp graph r1 r2
	    | IGetHwReg (r, hwr, _)
	    | ISetHwReg (hwr, r, _) ->
		mkpph graph r hwr
	    | _ ->
		graph
	  in

	  (* Add interference edges between the hardware register [$zero]
	     and every pseudo-register that the instruction renders
	     nonzeroable. See [Zero] for an explanation. *)

	  let graph =
	    mkiph graph (Zero.nonzeroable i) (MIPS.RegisterSet.singleton MIPS.zero)
	  in

	  graph

    ) proc.graph graph
  in

  (* Done. *)

  liveafter, graph

