(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$Author$
 *   $Revision$
 *)

(* The Komponist *)

signature COMPOSER =
    sig
	structure Sig: SIGNATURE

	exception Corrupt
	exception Conflict

	(*--** this is preliminary *)
	val sign:	Url.t -> Sig.t option
	val enterSign:	Url.t * Sig.t -> unit	(* [Conflict] *)

	val start:	Url.t -> unit		(* [Corrupt, IO.Io] *)
    end
