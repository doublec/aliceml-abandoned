(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure SMLToComPlusMain =
    let
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	val f: (Source.desc * Url.t -> Composer.Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "SMLToComPlusMain.f")

	structure ComPlusTarget = MakeComPlusTarget(Signature)

	structure FrontendSML =
	    MakeFrontendSML(fun loadSign (desc, url) = !f (desc, url)
			    structure Switches = Switches)

	structure FrontendCommon =
	    MakeFrontendCommon(fun loadSign (desc, url) = !f (desc, url)
			       structure Switches = Switches)

	structure BackendCommon = MakeBackendCommon(Switches)

	structure BackendComPlus = MakeBackendComPlus(ComPlusTarget)

	structure Compiler =
	    MakeCompiler(structure Switches         = Switches
			 structure Target           = ComPlusTarget
			 structure FrontendSpecific = FrontendSML
			 structure FrontendCommon   = FrontendCommon
			 structure BackendCommon    = BackendCommon
			 structure BackendSpecific  = BackendComPlus)

	structure RecursiveCompiler =
	    MakeRecursiveCompiler(structure Composer = Composer
				  structure Compiler = Compiler
				  val extension = "dll")

	structure BatchCompiler =
	    MakeBatchCompiler(structure RecursiveCompiler = RecursiveCompiler
			      val executableHeader = "")

	val _ = f := RecursiveCompiler.acquireSign
    in
	BatchCompiler
    end
