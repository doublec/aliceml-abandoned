(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

SMLofNJ.Internals.GC.messages false;
CM.make' "main-com+.cm";

local
    fun getArgs () =
	let
	    val args = SMLofNJ.getArgs ()
	in
	    case SMLofNJ.SysInfo.getOSKind () of
		SMLofNJ.SysInfo.WIN32 => tl args
	      | _ => args
	end

    fun stoc args = SMLToComPlusBatchCompiler.stoc args
		    handle Crash.Crash message =>
			       (TextIO.output (TextIO.stdErr,
					       "CRASH: " ^ message ^ "\n");
				OS.Process.failure)
			 | e =>   (*--**DEBUG*)
			       (TextIO.output (TextIO.stdErr,
					       "uncaught exception " ^
					       exnName e ^ "\n");
				OS.Process.failure)

    fun main _ = OS.Process.exit (stoc (getArgs ()))
in
    val _ = SMLofNJ.exportFn ("stoc-com+", main)
end;
