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

structure SMLToMozartMain =
    MakeMain(structure Composer = Composer
	     structure Compiler = SMLToMozartCompiler
	     structure TargetInitialContext = MozartTargetInitialContext)
