(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

signature UNSAFE_COMPONENT =
    sig
	val load: Url.t -> 'component
	val replaceSign: 'component * Inf.t -> 'component
	val save: string * 'component -> unit
    end
