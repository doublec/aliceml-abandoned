(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature SIMPLIFY =
    sig
	structure Intermediate: INTERMEDIATE = PostTranslationIntermediate
	structure Simplified: SIMPLIFIED = Simplified

	val simplifyDec: Intermediate.dec -> Simplified.dec
    end
