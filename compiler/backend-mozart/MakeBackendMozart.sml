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

structure MozartEngine =
    MakeEngine(fun cmd () =
		   ("/bin/sh",
		    [(*--**"/opt/mozart-1.1.1/bin/ozd", "-p",*)
		     case OS.Process.getEnv "STOC_MOZART" of
			 SOME s => s
		       | NONE => "stoc-mozart.exe"])
	       structure Code = PickleFlatGrammar)

functor MakeMozartTarget(structure Switches: SWITCHES
			 structure Sig: SIGNATURE
			     where type t = FlatGrammar.sign): TARGET =
    struct
	structure C = MozartEngine.C
	structure Sig = Sig

	type t = string * FlatGrammar.t

	fun sign (_, (_, _, _, exportSign)) = exportSign

	fun save context targetFilename (sourceFilename, component) =
	    MozartEngine.save context
	    (MozartEngine.link context (sourceFilename, component),
	     targetFilename)

	fun apply context (sourceFilename,
			   component as (_, _, exportDesc, _)) =
	    MozartEngine.apply context
	    (MozartEngine.link context (sourceFilename, component), exportDesc)
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
