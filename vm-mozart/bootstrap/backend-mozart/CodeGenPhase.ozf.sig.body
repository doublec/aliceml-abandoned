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

import
    structure FlatGrammar
from "x-alice:/backend-common/FlatGrammar.ozf"

signature CODE_GEN_PHASE_COMPONENT =
    sig
	structure CodeGenPhase:
	    sig
		val translate:
		    string *                  (* input filename *)
		    FlatGrammar.component *   (* component *)
		    string *                  (* output filename *)
		    string option *           (* assembly filename *)
		    string ->                 (* pickle header *)
		    unit
	    end
    end
