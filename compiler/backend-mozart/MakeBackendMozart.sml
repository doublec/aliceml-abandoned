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

	val engine: MozartEngine.t option ref = ref NONE

	fun new () =
	    (if (isSome (!engine)) then ()
	     else engine := SOME (MozartEngine.start ());
	     valOf (!engine))

	fun clone engine = engine
    end

functor MakeMozartTarget(structure Switches: SWITCHES
			 structure Sig: SIGNATURE
			     where type t = FlatGrammar.sign): TARGET =
    struct
	structure C = MozartTargetContext
	structure Sig = Sig

	type t = string * FlatGrammar.t

	fun sign (_, (_, (_, exportSign))) = exportSign

	fun apply engine component =
	    raise Crash.Crash "MozartTarget.apply: not implemented"

	fun save engine filename component = (*
	    MozartEngine.saveValue engine filename
	    (MozartEngine.buildFunctor engine component) *) ()
    end

functor MakeBackendMozart(structure Switches: SWITCHES
			  structure MozartTarget: TARGET
			      where type t = string * FlatGrammar.t): PHASE =
    MakeTracingPhase(structure Phase =
			 struct
			     structure C = EmptyContext
			     structure I = FlatGrammar
			     structure O = MozartTarget

			     fun translate () (desc, component) =
				 (case Source.url desc of
				      SOME url => Url.toString url
				    | NONE => "", component)
			 end
		     structure Switches = Switches
		     val name = "Assembling")
