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
    MakeEngine (val cmd = "/bin/sh"
		val args = ["stoc-mozart.exe"]
		structure Code = OzifyFlatGrammar)

structure MozartContext :> CONTEXT where type t = MozartEngine.t =
    struct
	type t = MozartEngine.t

	fun clone engine = engine
    end

structure MozartTarget: TARGET =
    (*--** :> TARGET where structure C = MozartContext *)
    struct
	structure C = MozartContext

	type t = string * FlatGrammar.t

	fun apply engine component =
	    raise Crash.Crash "MozartTarget.eval: not implemented"

	fun save engine filename component =
	    MozartEngine.saveValue engine filename
	    (MozartEngine.buildFunctor engine component)
    end

structure MozartGenerationPhase =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = MozartTarget

	fun translate component = component
    end
