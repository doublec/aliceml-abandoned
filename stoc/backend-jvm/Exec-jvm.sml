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
    val rec ds = fn
	([prog]) =>
	    (CodeGen.genProgramCode ("Emil", Main.simplifyString prog); 0)
    val rec dc = fn
	(fi::rest) =>
	    (CodeGen.genProgramCode ("Emil", Main.simplifyFile fi); dc (rest))
      | (nil) => 0

    fun dmlc (_, []) =
	(print "Syntax: \ndmlc file1 [file2..filen]\n"; 0)
	| dmlc (_, x) = (dc x) handle _ => 1
in
    val _ = SMLofNJ.exportFn ("dmlc", dmlc)
end;
