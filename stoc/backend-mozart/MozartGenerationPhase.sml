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

structure MozartEngine =
    MakeEngine(fun cmd () =
		   ("/bin/sh",
		    [case OS.Process.getEnv "STOC_MOZART" of
			 SOME s => s
		       | NONE => "stoc-mozart.exe"])
	       structure Code = OzifyFlatGrammar)

structure MozartTargetContext: CONTEXT =
    struct
	type t = MozartEngine.t

	fun clone engine = engine
    end

structure MozartTarget: TARGET =
    struct
	structure C = MozartTargetContext

	type t = string * FlatGrammar.t

	fun apply engine component =
	    raise Crash.Crash "MozartTarget.eval: not implemented"

	fun save engine filename component =
	    MozartEngine.saveValue engine filename
	    (MozartEngine.buildFunctor engine component)
    end

structure MozartGenerationPhase: PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = MozartTarget

	fun translate () (desc, component) =
	    (case Source.url desc of
		 SOME url => Url.toString url
	       | NONE => "", component)
    end
