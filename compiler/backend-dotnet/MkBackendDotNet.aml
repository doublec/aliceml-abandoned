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

	fun apply () component =
	    raise Crash.Crash "ComPlusTarget.apply: not implemented"

	fun save () filename component = IL.outputProgram (filename, component)
    end

functor MakeBackendComPlus
    (ComPlusTarget: TARGET where type t = IL.t): PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = ComPlusTarget

	val translate = CodeGenPhase.translate
    end
