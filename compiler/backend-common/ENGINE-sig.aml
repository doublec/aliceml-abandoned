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

signature ENGINE =
    sig
	structure C: CONTEXT

	type code
	type component

	type exportDesc = (Label.t * FlatGrammar.id) vector

	exception Format of string

	val link: C.t -> code -> component                 (* Format *)
	val save: C.t -> component * string -> unit        (* Format *)
	val apply: C.t -> component * exportDesc -> unit   (* Format *)
    end
