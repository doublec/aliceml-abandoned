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

signature CODE =
    sig
	type t

	val externalize: TextIO.outstream * t -> unit
    end

signature ENGINE =
    sig
	type t
	type code
	type value

	exception Format of string

	val start: unit -> t
	val stop: t -> unit

	val buildFunctor: t -> code -> value   (* Format *)
	val saveValue: t -> string -> value -> unit   (* Format *)

	val valueToString: value -> string
    end
