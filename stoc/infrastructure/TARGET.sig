(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2001
 *   Andreas Rossberg, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature TARGET =
  sig
    structure C: CONTEXT
    structure Sig: SIGNATURE

    type t

    val sign: t -> Sig.t

    val save: C.t -> string -> t -> unit
    val apply: C.t -> t -> unit
  end
