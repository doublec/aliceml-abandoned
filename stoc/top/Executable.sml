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
	((Main.ozifyStringToStream (TextIO.inputAll TextIO.stdIn,
				    TextIO.stdOut); 0) handle _ => 1)
      | stoc (_, [infile]) =
	((Main.ozifyFileToStream (infile, TextIO.stdOut); 0) handle _ => 1)
      | stoc (_, [infile, outfile]) =
	((Main.ozifyFile (infile, outfile); 0) handle _ => 1)
      | stoc (_, _) = 2
in
    val _ = SMLofNJ.exportFn ("stoc-frontend", stoc)
end;
