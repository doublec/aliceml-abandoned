(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *   Andreas Rossberg, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature URL =
    sig
	eqtype url
	type t = url

	type scheme = string option
	type authority = string option
	type device = char option
	type path = string list
	type query = string option
	type fragment = string option

	exception Malformed

	val empty: url
	val fromString: string -> url              (* Malformed *)
	val toString: url -> string
	val isAbsolute: url -> bool
	val resolve: url -> url -> url
	val compare: url * url -> order
	val hash: url -> int

	(* Accessing URL Constituents *)

	val getScheme: url -> scheme
	val getAuthority: url -> authority
	val getDevice: url -> device
	val isAbsolutePath: url -> bool
	val getPath: url -> path
	val getQuery: url -> query
	val getFragment: url -> fragment

	val setScheme: url * scheme -> url         (* Malformed *)
	val setAuthority: url * authority -> url   (* Malformed *)
	val setDevice: url * device -> url         (* Malformed *)
	val makeAbsolutePath: url -> url
	val makeRelativePath: url -> url
	val setPath: url * path -> url
	val setQuery: url * query -> url           (* Malformed *)
	val setFragment: url * fragment -> url
    end
