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

signature BATCH_COMPILER =
  sig

    structure Composer: COMPOSER
    structure Switches: SWITCHES

    val acquireSign :		Source.desc * Url.t -> Composer.Sig.t

    val stoc :			string list -> OS.Process.status

    (*DEBUG*)
    val parseString :		string -> InputGrammar.t
    val parseFile :		string -> InputGrammar.t

    val abstractString :	string -> AbstractGrammar.t
    val abstractFile :		string -> AbstractGrammar.t

    val elabString :		string -> TypedGrammar.t
    val elabFile :		string -> TypedGrammar.t

    val translateString :	string -> IntermediateGrammar.t
    val translateFile :		string -> IntermediateGrammar.t

    val flattenString :		string -> FlatGrammar.t
    val flattenFile :		string -> FlatGrammar.t

  end
