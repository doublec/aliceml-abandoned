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

    fun defaults () =
	(Main.Switches.printComponentSig := false;
	 Main.Switches.defaultImport := false)

    fun stoc nil =   (* for testing bootstrapping *)
	(defaults ();
	 hdl Main.flattenString (TextIO.inputAll TextIO.stdIn))
      | stoc ([infile, "-o", outfile] | ["-c", infile, "-o", outfile]) =
	(defaults ();
	 hdl Main.compile (infile, outfile, ""))
      | stoc [infile, outfile] =
	(defaults ();
	 hdl Main.compile (infile, outfile, ""))
      | stoc _ = OS.Process.failure

    fun main _ = stoc (CommandLine.arguments ())
in
    val _ = SMLofNJ.exportFn ("stoc-com+", main)
end;
