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
    MakeBatchCompiler(structure Composer = Composer
		      structure Compiler = SMLToMozartCompiler
		      val executableHeader =
			  "#!/bin/sh\nexec stow $0 \"$@\"\n")
