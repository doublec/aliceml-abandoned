(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *   Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Main =
    MakeMain(structure Composer = Composer
	     structure Compiler = SMLToMozartCompiler
	     structure TargetInitialContext = MozartTargetInitialContext)
