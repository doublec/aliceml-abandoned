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

functor MakeEmacsToplevel(structure RecursiveCompiler: RECURSIVE_COMPILER):
    INTERACTIVE_TOPLEVEL =
    struct
	fun usage () = TextIO.output (TextIO.stdErr, "Usage: stot\n")

	fun readUntilEOF q =
	    case TextIO.inputLine q of
		"\^D\n" => nil
	      | line => line::readUntilEOF q

	fun readSource q =
	    case readUntilEOF q of
		filename::line::sourceLines =>
		    (*--** do something with filename and line *)
		    String.concat sourceLines
	      | _ =>
		    (TextIO.output (TextIO.stdErr,
				    "### invalid input ignored\n"); "")

	fun eval (source, compilerContext, targetContext) =
	    let
		val (compilerContext, target) =
		    RecursiveCompiler.compileString compilerContext source
	    in
		RecursiveCompiler.Target.apply targetContext target;
		(compilerContext, targetContext)
	    end

	fun loop (compilerContext, targetContext) =
	    loop (eval (readSource TextIO.stdIn,
			compilerContext, targetContext))

	fun stot nil =
	    let
		val (compilerContext, targetContext) =
		    eval ("", RecursiveCompiler.empty,
			  RecursiveCompiler.Target.C.new ())
	    in
		RecursiveCompiler.Switches.Bootstrap.implicitImport := false;
		loop (compilerContext, targetContext)
	    end
	  | stot (_::_) = (usage (); OS.Process.failure)
    end
