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

functor MakeComPlusTarget(Sig: SIGNATURE where type t = FlatGrammar.sign):
    TARGET =
    struct
	structure C = EmptyContext
	structure Sig = Sig

	type t = IL.t

	fun sign (_, exportSign) = exportSign

	fun save () filename component =
	    let
		val ilFilename = filename ^ ".il"
		val ilasm =
		    "ilasm /dll \"" ^ ilFilename ^
		    "\" /out=\"" ^ filename ^ "\""
	    in
		IL.outputProgram (ilFilename, component);
		if OS.Process.system ilasm = OS.Process.success then ()
		else
		    raise Error.Error (Source.nowhere,
				       "invocation of `" ^ ilasm ^ "' failed")
	    end
    end

functor MakeBackendComPlus
    (ComPlusTarget: TARGET where type t = IL.t): PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = ComPlusTarget

	val translate = CodeGenPhase.translate
    end
