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
		    (*--** for debugging: ..., "--debug" *)
	       structure Code = OzifyFlatGrammar)

structure MozartTargetContext :> CONTEXT where type t = MozartEngine.t =
    struct
	type t = MozartEngine.t

	fun clone engine = engine
    end

structure MozartTarget :> TARGET
    where C = MozartTargetContext
    where type t = string * FlatGrammar.t =
    struct
	structure C = MozartTargetContext

	type t = string * FlatGrammar.t

	fun apply engine component =
	    raise Crash.Crash "MozartTarget.eval: not implemented"

	fun save engine filename component =
	    MozartEngine.saveValue engine filename
	    (MozartEngine.buildFunctor engine component)
    end

structure MozartGenerationContext :> CONTEXT where type t = string =
    struct
	type t = string

	fun clone s = s
    end

structure MozartGenerationPhase (*:> PHASE
    where C = MozartGenerationContext
    where I = FlatGrammar
    where O = MozartTarget*) =
    (*--** if I insert the above, SML/NJ crashes when compiling top/Main.sml *)
    struct
	structure C = MozartGenerationContext
	structure I = FlatGrammar
	structure O = MozartTarget

	fun translate inFilename component = (inFilename, component)
    end
