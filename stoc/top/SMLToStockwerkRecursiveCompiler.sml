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

structure SMLToStockwerkMain =
    let
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	val f: (Source.desc * Url.t -> Composer.Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "SMLToStockwerkMain.f")

	structure PickleTarget =
	    MakePickleTarget(structure Sig = Signature)

	structure FrontendSML =
	    MakeFrontendSML(fun loadSign (desc, url) = !f (desc, url)
			    structure Switches = Switches)

	structure FrontendCommon =
	    MakeFrontendCommon(fun loadSign (desc, url) = !f (desc, url)
			       structure Switches = Switches)

	structure BackendCommon = MakeBackendCommon(Switches)

	structure BackendStockwerk =
	    MakeBackendStockwerk(structure Switches = Switches
				 structure PickleTarget = PickleTarget)
	structure Compiler =
	    MakeCompiler(structure Switches         = Switches
			 structure Target           = PickleTarget
			 structure FrontendSpecific = FrontendSML
			 structure FrontendCommon   = FrontendCommon
			 structure BackendCommon    = BackendCommon
			 structure BackendSpecific  = BackendStockwerk)

	structure RecursiveCompiler =
	    MakeRecursiveCompiler(structure Composer = Composer
				  structure Compiler = Compiler
				  val extension = ".stc")

	structure BatchCompiler =
	    MakeBatchCompiler(structure RecursiveCompiler = RecursiveCompiler
			      val executableHeader =
				  "#!/bin/sh\nexec stow $0 \"$@\"\n")

	val _ = f := RecursiveCompiler.acquireSign
    in
	BatchCompiler
    end
