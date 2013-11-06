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

SMLofNJ.Internals.GC.messages false;
CM.make' "main-mozart.cm";

local
    fun main _ =
	OS.Process.exit (SMLToMozartEmacsToplevel.main (SMLofNJ.getArgs ()))
in
    val _ = SMLofNJ.exportFn ("alice-mozart", main)
end;
