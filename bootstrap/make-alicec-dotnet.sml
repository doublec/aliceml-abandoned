(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

SMLofNJ.Internals.GC.messages false;
CM.make' "main-dotnet.cm";

local
    fun getArgs () =
	let
	    val args = SMLofNJ.getArgs ()
	in
	    case SMLofNJ.SysInfo.getOSKind () of
		SMLofNJ.SysInfo.WIN32 => tl args
	      | _ => args
	end

    fun stoc args = SMLToDotNetBatchCompiler.stoc args
	handle e =>
	let
	    val hist  = List.rev(SMLofNJ.exnHistory e)
	    val trace = String.concat(List.map (fn s => s ^ "\n") hist)
	in
	    case e of
		Crash.Crash message =>
		    TextIO.output (TextIO.stdErr, "CRASH: " ^ message ^ "\n")
	      | _ => ();
	    TextIO.output (TextIO.stdErr, "uncaught exception " ^
					  exnName e ^ ":\n" ^ trace);
	    OS.Process.failure
	end

    fun main _ = OS.Process.exit (stoc (getArgs ()))
in
    val _ = SMLofNJ.exportFn ("stoc-dotnet", main)
end;
