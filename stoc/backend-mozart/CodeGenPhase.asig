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
    structure Source
from "../infrastructure/Source.ozf"

import
    structure Inf
from "../common/Inf.ozf"

import
    structure FlatGrammar
from "../backend-common/FlatGrammar.ozf"

signature CODE_GEN_PHASE_COMPONENT =
    sig
	structure CodeGenPhase:
	    sig
		type t

		val translate: Source.desc * FlatGrammar.component -> t
		val sign: t -> Inf.sign
		val apply: t -> unit
		val save: string * bool * t -> unit
	    end
    end
