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

signature ENVIRONMENT =
    sig
	type t

	val new: unit -> t
	val startFn: t -> unit
	val endFn: t -> FlatGrammar.id vector * int
	val declare: t * FlatGrammar.id -> PickleGrammar.id
	val fresh: t -> PickleGrammar.id
	val lookup: t * FlatGrammar.id -> PickleGrammar.idRef
	val lookupStamp: t * Stamp.t -> PickleGrammar.idRef option
	val lookupShared: t * Stamp.t -> PickleGrammar.instr option
	val declareShared: t * Stamp.t * PickleGrammar.instr -> unit
    end
