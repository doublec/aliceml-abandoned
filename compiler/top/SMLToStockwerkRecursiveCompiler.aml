(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
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
	structure SMLToStockwerkCompiler =
	    MakeCompiler(structure Switches         = Switches
			 structure Target           = PickleTarget
			 structure FrontendSpecific = FrontendSML
			 structure FrontendCommon   = FrontendCommon
			 structure BackendCommon    = BackendCommon
			 structure BackendSpecific  = BackendStockwerk)

	structure SMLToStockwerkBatchCompiler =
	    MakeBatchCompiler(structure Composer = Composer
			      structure Compiler = SMLToStockwerkCompiler
			      val extension = ".stc"
			      val executableHeader =
				  "#!/bin/sh\nexec stow $0 \"$@\"\n")

	val _ = f := SMLToStockwerkBatchCompiler.acquireSign
    in
	SMLToStockwerkBatchCompiler
    end
