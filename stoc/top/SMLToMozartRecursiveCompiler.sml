(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure SMLToMozartMain =
    let
	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	val f: (Source.desc * Url.t -> Composer.Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "SMLToMozartMain.f")

	structure MozartTarget =
	    MakeMozartTarget(structure Switches = Switches
			     structure Sig = Signature)

	structure FrontendSML =
	    MakeFrontendSML(fun loadSign (desc, url) = !f (desc, url)
			    structure Switches = Switches)

	structure FrontendCommon =
	    MakeFrontendCommon(fun loadSign (desc, url) = !f (desc, url)
			       structure Switches = Switches)

	structure BackendCommon = MakeBackendCommon(Switches)

	structure BackendMozart =
	    MakeBackendMozart(structure Switches = Switches
			      structure MozartTarget = MozartTarget)

	structure SMLToMozartCompiler =
	    MakeCompiler(structure Switches         = Switches
			 structure Target           = MozartTarget
			 structure FrontendSpecific = FrontendSML
			 structure FrontendCommon   = FrontendCommon
			 structure BackendCommon    = BackendCommon
			 structure BackendSpecific  = BackendMozart)

	structure SMLToMozartBatchCompiler =
	    MakeBatchCompiler(structure Composer = Composer
			      structure Compiler = SMLToMozartCompiler
			      val extension = "ozf"
			      val executableHeader =
				  "#!/bin/sh\nexec stow $0 \"$@\"\n")

	val _ = f := SMLToMozartBatchCompiler.acquireSign
    in
	SMLToMozartBatchCompiler
    end
