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
CM.make ();

local
    fun getArgs () =
	let
	    val args = SMLofNJ.getArgs ()
	in
	    case SMLofNJ.SysInfo.getOSKind () of
		SMLofNJ.SysInfo.WIN32 => tl args
	      | _ => args
	end

    fun collect args =
	Collect.alicegtk args
	handle e =>   (*--**DEBUG*)
	       (TextIO.output (TextIO.stdErr,
			       "uncaught exception " ^ exnName e ^ "\n");
		OS.Process.failure)

    fun main _ = OS.Process.exit (collect (getArgs ()))
in
    val _ = SMLofNJ.exportFn ("collect", main)
end;
