(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

import
    structure Source
from "../infrastructure/Source"

import
    signature CONTEXT
from "../infrastructure/CONTEXT-sig"

import
    structure Inf
from "../common/Inf"

import
    structure FlatGrammar
from "../backend-common/FlatGrammar"

signature CODE_GEN_PHASE_COMPONENT =
    sig
	structure CodeGenPhase:
	    sig
		structure C: CONTEXT

		type t

		val translate: C.t * Source.desc * FlatGrammar.component -> t
		val sign: t -> Inf.sign
		val save: t * string * bool -> unit
		val apply: t -> unit
	    end
    end
