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

structure SMLToStockwerkMain =
    MakeMain(structure Composer = Composer
	     structure Compiler = SMLToStockwerkCompiler
	     val executableHeader = "#!/bin/sh\nexec stow $0 \"$@\"\n")
