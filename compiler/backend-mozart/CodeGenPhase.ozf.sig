(* -*- sml -*- *)

(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature CODE_GEN_PHASE_COMPONENT =
    sig
	structure CodeGenPhase:
	    sig
		val translate: string * 'a * string -> unit
	    end
    end
