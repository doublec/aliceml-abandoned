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
	structure Intermediate: INTERMEDIATE
	structure Simplified: SIMPLIFIED

	val simplifyDec: Intermediate.dec -> Simplified.dec
    end
where Intermediate = PostTranslationIntermediate
where Simplified = Simplified
