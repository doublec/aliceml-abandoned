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
    fun getArgs () =
	let
	    val args = SMLofNJ.getArgs ()
	in
	    case SMLofNJ.SysInfo.getOSKind () of
		SMLofNJ.SysInfo.WIN32 => tl args
	      | _ => args
	end

    fun hdl f x =
	(f x; OS.Process.success)
	handle e =>
	    (TextIO.output (TextIO.stdErr,
			    "uncaught exception " ^ exnName e ^ "\n");
	     OS.Process.failure)

    fun defaults () = Main.Switches.printComponentSig := false

    fun stoc nil =   (* for testing bootstrapping *)
	(defaults ();
	 hdl Main.flattenString (TextIO.inputAll TextIO.stdIn))
      | stoc ([infile, "-o", outfile] | ["-c", infile, "-o", outfile]) =
	(defaults ();
	 hdl Main.compileForMozart (infile, outfile))
      | stoc [infile, outfile] =
	(defaults ();
	 hdl Main.compileForMozart (infile, outfile))
      | stoc _ = OS.Process.failure
in
    val _ = SMLofNJ.exportFn ("stoc-mozart", fn _ => stoc (getArgs ()))
end;
