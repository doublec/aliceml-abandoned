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
    fun hdl f x =
	(f x; OS.Process.success)
	handle e =>
	    (TextIO.output (TextIO.stdErr,
			    "uncaught exception " ^ exnName e ^ "\n");
	     OS.Process.failure)
    fun stoc (_, []) =
	hdl Main.ozifyStringToStdOut (TextIO.inputAll TextIO.stdIn)
      | stoc (_, [infile]) =
	hdl Main.ozifyFileToStdOut infile
      | stoc (_, [infile, outfile]) =
	hdl Main.ozifyFileToFile (infile, outfile)
      | stoc (_, _) = 2
in
    val _ = SMLofNJ.exportFn ("stoc-frontend", stoc)
end;
