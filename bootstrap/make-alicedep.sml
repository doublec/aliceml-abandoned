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
CM.make' "make-depend.cm";

local
    fun main _ = OS.Process.exit (MakeDepend.depend (SMLofNJ.getArgs ()))
in
    val _ = SMLofNJ.exportFn ("alicedep", main)
end;
