(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature RECURSIVE_COMPILER =
    sig
	structure Composer: COMPOSER
	structure Switches: SWITCHES
	structure Target: TARGET

	type context
	val empty: context

	val extension: string

	val compileSign: string -> Composer.Sig.t
	val compileFileToFile: string * string -> Composer.Sig.t
	val acquireSign: Source.desc * Url.t -> Composer.Sig.t

	val compileFile: context -> string -> context * Target.t
	val compileString: context -> string -> context * Target.t

	(*DEBUG*)
	val processFile: (Source.desc * string -> 'a) -> string -> 'a
	val processString: (Source.desc * string -> 'a) -> string -> 'a
    end
