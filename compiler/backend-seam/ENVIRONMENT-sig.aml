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
	val declare: t * FlatGrammar.id -> Pickle.id
	val fresh: t -> Pickle.id
	val lookup: t * FlatGrammar.id -> Pickle.idRef
	val lookupStamp: t * Stamp.t -> Pickle.idRef
    end
