(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure SMLToMozartBatchCompiler =
    MakeBatchCompiler(structure RecursiveCompiler =
			  SMLToMozartRecursiveCompiler
		      val executableHeader =
			  "#!/bin/sh\nexec stow $0 \"$@\"\n")
