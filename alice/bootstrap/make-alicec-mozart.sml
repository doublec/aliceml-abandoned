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
(*Compiler.Profile.setProfMode true;  produces overflow during bootstrapping*)
CM.make' "main-mozart.cm";

local
    fun main' ("--profile"::args) =
	((*Compiler.Profile.setTimingMode true;
	 Compiler.Profile.reset ();*)
	 main' args (*before Compiler.Profile.report TextIO.stdOut*))
      | main' args = SMLToMozartBatchCompiler.main args
	handle e =>
	let
	    val history = List.rev(SMLofNJ.exnHistory e)
	    val trace   = String.concat(List.map (fn s => s ^ "\n") history)
	in
	    case e of
		Crash.Crash message =>
		    TextIO.output (TextIO.stdErr, "CRASH: " ^ message ^ "\n")
	      | IO.Io {name, function, cause} =>
		    TextIO.output (TextIO.stdErr,
				   "Io {name = " ^ name ^
				   ", function = " ^ function ^
				   ", cause = " ^ exnName cause ^ "}\n")
	      | _ => ();
	    TextIO.output (TextIO.stdErr, "uncaught exception " ^
					  exnName e ^ ":\n" ^ trace);
	    OS.Process.failure
	end

    fun main _ = OS.Process.exit (main' (SMLofNJ.getArgs ()))
in
    val _ = SMLofNJ.exportFn ("alicec-mozart", main)
end;
