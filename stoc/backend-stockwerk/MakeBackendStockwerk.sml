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

functor MakePickleTarget(structure Sig: SIGNATURE
			     where type t = PickleGrammar.sign): TARGET =
    struct
	structure C = EmptyContext
	structure Sig = Sig

	type t = PickleGrammar.t

	fun sign (PickleGrammar.Tuple #[PickleGrammar.Vector _,
					PickleGrammar.Closure (_, #[]),
					PickleGrammar.Sign exportSign]) =
	    exportSign
	  | sign _ = raise Crash.Crash "MakeBackendStockwerk.sign"

	fun save () name value =
	    let
		val outstream = PrimPickle.openOut name
	    in
		OutputPickle.output (outstream, value);
		PrimPickle.closeOut outstream
	    end
    end

functor MakeBackendStockwerk(structure Switches: SWITCHES
			     structure PickleTarget: TARGET
				where type t = PickleGrammar.t): PHASE =
    MakeTracingPhase(structure Phase =
			 struct
			     structure C = EmptyContext
			     structure I = FlatGrammar
			     structure O = PickleTarget

			     val translate = CodeGenPhase.translate
			 end
		     structure Switches = Switches
		     val name = "Emitting Pickle")
