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

structure SMLToStockwerkBatchCompiler =
    MakeBatchCompiler(structure RecursiveCompiler =
			  SMLToStockwerkRecursiveCompiler
		      val executableHeader =
			  "#!/bin/sh\nexec stow $0 \"$@\"\n")
