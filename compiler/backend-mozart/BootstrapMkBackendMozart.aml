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
		    [(*--**"/opt/mozart-1.1.1/bin/ozd", "-p",*)
		     case OS.Process.getEnv "STOC_MOZART" of
			 SOME s => s
		       | NONE => "stoc-mozart.exe"])
	       structure Code = PickleFlatGrammar)

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

	fun save engine filename component =
	    MozartEngine.saveValue engine filename
	    (MozartEngine.buildFunctor engine component)
    end

fun parseDesc desc =
    case Source.url desc of
	SOME url =>
	    (case (Url.getScheme url, Url.getAuthority url) of
		 (NONE, NONE) =>
		     Url.toString url
	       | (SOME "file", NONE) =>
		     Url.toString (Url.setScheme (url, NONE))
	       | _ => raise Crash.Crash "MakeBackendMozart.parseUrl")
      | NONE => OS.FileSys.tmpName ()

functor MakeBackendMozart(structure Switches: SWITCHES
			  structure MozartTarget: TARGET
			      where type t = string * FlatGrammar.t): PHASE =
    MakeTracingPhase(structure Phase =
			 struct
			     structure C = EmptyContext
			     structure I = FlatGrammar
			     structure O = MozartTarget

			     fun translate () (desc, component) =
				 (parseDesc desc, component)
			 end
		     structure Switches = Switches
		     val name = "Assembling")
