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
    fun getArgs () =
	let
	    val args = SMLofNJ.getArgs ()
	in
	    case SMLofNJ.SysInfo.getOSKind () of
		SMLofNJ.SysInfo.WIN32 => tl args
	      | _ => args
	end
    fun stoc nil =
	hdl Main.comifyStringToStdOut (TextIO.inputAll TextIO.stdIn)
      | stoc [infile] =
	hdl Main.comifyFileToStdOut infile
      | stoc [infile, outfile] =
	hdl Main.comifyFileToFile (infile, outfile)
      | stoc _ = OS.Process.failure
in
    val _ = SMLofNJ.exportFn ("stoc-com+", fn _ => stoc (getArgs ()))
end;
