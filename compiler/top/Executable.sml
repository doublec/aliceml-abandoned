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

SMLofNJ.Internals.GC.messages false;
CM.make();
local
    fun stoc (_, []) =
	(Main.ozifyStringToStdOut (TextIO.inputAll TextIO.stdIn)
	;OS.Process.success)
      | stoc (_, [infile]) =
	(Main.ozifyFileToStdOut infile; OS.Process.success)
      | stoc (_, [infile, outfile]) =
	(Main.ozifyFileToFile (infile, outfile); OS.Process.success)
      | stoc (_, _) = 2
in
    val _ = SMLofNJ.exportFn ("stoc-frontend", stoc)
end;
