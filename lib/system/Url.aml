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

(*
 * Parts of this have been adapted from Mozart's URL module
 * which has been written Denys Duchier and Christian Schulte.
 *)

structure Url :> URL =
    struct
	type scheme = string option
	type authority = string option
	type device = char option
	type path = string list
	type query = string option
	type fragment = string option

	type url = {scheme: scheme,
		    authority: authority,
		    device: device,
		    absolute: bool,
		    path: path,
		    query: query,
		    fragment: fragment}
	type t = url

	exception Malformed

	(*
	 * Split a string at the 1st occurrence of a separator character.
	 * Return a tuple (prefix, sep, suffix).  The argument function
	 * has to return true iff a given character is a separator.
	 * If the input is exhausted without finding a separator character,
	 * the prefix contains the whole string, the suffix is nil, and
	 * NONE is returned as separator.
	 *)

	fun split f cs =
	    let
		fun split' (c::cr, f, prefix) =
		    if f c then (List.rev prefix, SOME c, cr)
		    else split' (cr, f, c::prefix)
		  | split' (nil, _, _) = (cs, NONE, nil)
	    in
		split' (cs, f, nil)
	    end

	(*
	 * Decode/encode a path component using '%xx' escapes.
	 *)

	local
	    fun hexval c =
		let
		    val i = Char.ord c
		in
		    if Char.ord #"0" <= i andalso i <= Char.ord #"9"
		    then i - Char.ord #"0"
		    else if Char.ord #"A" <= i andalso i <= Char.ord #"F"
		    then i - Char.ord #"A" + 10
		    else if Char.ord #"a" <= i andalso i <= Char.ord #"f"
		    then i - Char.ord #"a" + 10
		    else raise Malformed
		end
	in
	    fun decode (#"%"::c1::c2::rest) =
		Char.chr (hexval c1 * 16 + hexval c2)::decode rest
	      | decode (#"%"::_) = raise Malformed
	      | decode (c::rest) = c::decode rest
	      | decode nil = nil
	end

	fun isPathChar #";" = true   (*--** should not be *)
	  | isPathChar (#"-" | #"_" | #"." | #"!") = true
	  | isPathChar (#"~" | #"*" | #"'" | #"(") = true
	  | isPathChar (#")" | #":" | #"@" | #"&") = true
	  | isPathChar (#"=" | #"+" | #"$" | #",") = true
	  | isPathChar c = Char.isAlphaNum c

	local
	    val hex = #[#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7",
			#"8", #"9", #"A", #"B", #"C", #"D", #"E", #"F"]

	    fun encode' (c1::cr, f) =
		if f c1 then c1::encode' (cr, f)
		else
		    let
			(*--** does not work for unicode characters *)
			val i = Char.ord c1
			val h1 = Vector.sub (hex, i div 16)
			val h2 = Vector.sub (hex, i mod 16)
		    in
			#"%"::h1::h2::encode' (cr, f)
		    end
	      | encode' (nil, _) = nil
	in
	    fun encode f s = String.implode (encode' (String.explode s, f))
	end

	(*
	 * A path is represented by a sequence of strings.
	 * Normalizing a path is the process of eliminating occurrences
	 * of path components "." and ".." by interpreting them relative
	 * to the stack of path components.  A leading "." may not be
	 * thrown out because ./foo and foo should be treated differently:
	 * the first one is really an absolute path, whereas the second
	 * one is relative.
	 *)

	local
	    fun normalizePath' (["."], nil) = nil
	      | normalizePath' (["."], stack) = List.rev (""::stack)
	      | normalizePath' ("."::rest, stack) =
		normalizePath' (rest, stack)
	      | normalizePath' ([".."], nil) = [".."]
	      | normalizePath' ([".."], _::stack) = List.rev (""::stack)
	      | normalizePath' (".."::rest, nil) =
		".."::normalizePath' (rest, nil)
	      | normalizePath' (".."::rest, _::stack) =
		normalizePath' (rest, stack)
	      | normalizePath' (s::rest, stack) =
		normalizePath' (rest, s::stack)
	      | normalizePath' (nil, stack) = List.rev stack
	in
	    fun normalizePath ("."::rest) = "."::normalizePath' (rest, nil)
	      | normalizePath s = normalizePath' (s, nil)
	end

	fun toLower cs = String.implode (List.map Char.toLower cs)

	fun toDevice c =
	    if
		Char.ord c >= Char.ord #"a" andalso Char.ord c <= Char.ord #"z"
		orelse
		Char.ord c >= Char.ord #"A" andalso Char.ord c <= Char.ord #"Z"
	    then Char.toLower c
	    else raise Malformed

	fun isStart (#"/" | #"\\" | #":" | #"?" | #"#") = true
	  | isStart _ = false

	fun isPath (#"/" | #"\\" | #"?" | #"#") = true
	  | isPath _ = false

	fun isQuery #"#" = true
	  | isQuery _ = false

	(* Accessing URL Constituents *)

	fun getScheme ({scheme, ...}: url) = scheme
	fun getAuthority ({authority, ...}: url) = authority
	fun getDevice ({device, ...}: url) = device
	fun isAbsolutePath ({absolute, ...}: url) = absolute
	fun getPath ({path, ...}: url) = path
	fun getQuery ({query, ...}: url) = query
	fun getFragment ({fragment, ...}: url) = fragment

	fun setScheme ({scheme = _, authority, device, absolute, path,
			query, fragment}: url, scheme) =
	    (if isSome scheme then
		 case split isStart (String.explode (valOf scheme)) of
		     (_, NONE, _) => ()
		   | (_, SOME _, _) => raise Malformed
	     else ();
	     {scheme = scheme, authority = authority, device = device,
	      absolute = absolute, path = path, query = query,
	      fragment = fragment})

	fun setAuthority ({scheme, authority = _, device, absolute, path,
			   query, fragment}: url, authority) =
	    (if isSome authority then
		 case split isStart (String.explode (valOf authority)) of
		     (_, NONE, _) => ()
		   | (_, SOME _, _) => raise Malformed
	     else ();
	     {scheme = scheme, authority = authority, device = device,
	      absolute = absolute, path = path, query = query,
	      fragment = fragment})

	fun setDevice ({scheme, authority, device = _, absolute, path,
			query, fragment}: url, device) =
	    {scheme = scheme, authority = authority,
	     device = Option.map toDevice device,
	     absolute = absolute, path = path, query = query,
	     fragment = fragment}

	fun makeAbsolutePath ({scheme, authority, device, absolute = _, path,
			       query, fragment}: url) =
	    {scheme = scheme, authority = authority, device = device,
	     absolute = true, path = path, query = query,
	     fragment = fragment}

	fun makeRelativePath ({scheme, authority, device, absolute = _, path,
			       query, fragment}: url) =
	    {scheme = scheme, authority = authority, device = device,
	     absolute = false, path = path, query = query,
	     fragment = fragment}

	fun setPath ({scheme, authority, device, absolute, path = _,
		      query, fragment}: url, path) =
	    {scheme = scheme, authority = authority, device = device,
	     absolute = absolute, path = path, query = query,
	     fragment = fragment}

	fun setQuery ({scheme, authority, device, absolute, path,
		       query = _, fragment}: url, query) =
	    (if isSome query then
		 case split isQuery (String.explode (valOf query)) of
		     (_, NONE, _) => ()
		   | (_, SOME _, _) => raise Malformed
	     else ();
	     {scheme = scheme, authority = authority, device = device,
	      absolute = absolute, path = path, query = query,
	      fragment = fragment})

	fun setFragment ({scheme, authority, device, absolute, path,
			  query, fragment = _}: url, fragment) =
	    {scheme = scheme, authority = authority, device = device,
	     absolute = absolute, path = path, query = query,
	     fragment = fragment}

	val empty =
	    {scheme = NONE, authority = NONE, device = NONE,
	     absolute = false, path = nil, query = NONE, fragment = NONE}

	(*
	 * This parser traverses the string only once and uses character
	 * sets to recognize the crucial characters that determine the
	 * breaking points in a url.
	 *
	 * This parser is a state machine, with 6 states, each of which is
	 * implemented by a procedure:
	 *
	 * parseStart		the initial state: what is at the front of the
	 *			url is disambiguated by the 1st separator we
	 *			find or the eos
	 * parseAuthority	entered when we encounter the // thing
	 * parsePathDev		recognize a device or the next path component
	 * parsePath		recognize the next path component
	 * parseQuery		after `?'
	 * parseFragment	after `#'
	 *)

	(*--** when we have the {url where absolute = true} syntax,
	 * this should be rewritten to not use state for simplicity
	 *)

	fun fromString s =
	    let
		val scheme: string option ref = ref NONE
		val authority: string option ref = ref NONE
		val device: char option ref = ref NONE
		val absolute = ref false
		val path: string list ref = ref nil
		val query: string option ref = ref NONE
		val fragment: string option ref = ref NONE

		fun pushPath s = path := String.implode (decode s)::(!path)

		fun parseStart cs =
		    case split isStart cs of
			(prefix, NONE, _) =>
			    (* hit the end without finding a separator *)
			    pushPath prefix
		      | ([c], SOME #":", suffix) =>
			    (* found the device separator *)
			    (device := SOME (toDevice c);
			     case suffix of
				 (#"/" | #"\\")::rest =>
				     (absolute := true; parsePath rest)
			       | _::_ => parsePathDev suffix
			       | nil => ())
		      | (prefix, SOME #":", suffix) =>
			    (* found the scheme separator *)
			    (scheme := SOME (toLower prefix);
			     (* check for //authority *)
			     case suffix of
				 (#"/" | #"\\")::(#"/" | #"\\")::rest =>
				     parseAuthority rest
			       | (#"/" | #"\\")::rest =>
				     (absolute := true; parsePath rest)
			       | _ => parsePathDev suffix)
		      | (nil, SOME (#"/" | #"\\"), (#"/" | #"\\")::rest) =>
			    (* found //authority at start *)
			    parseAuthority rest
		      | (nil, SOME (#"/" | #"\\"), suffix) =>
			    (* found absolute path at start *)
			    (absolute := true; parsePath suffix)
		      | (prefix, SOME (#"/" | #"\\"), suffix) =>
			    (pushPath prefix; parsePath suffix)
		      | (prefix, SOME #"?", suffix) =>
			    (case prefix of
				 _::_ => pushPath prefix
			       | nil => ();
			     parseQuery suffix)
		      | (prefix, SOME #"#", suffix) =>
			    (case prefix of
				 _::_ => pushPath prefix
			       | nil => ();
			     parseFragment suffix)
		      | (_, SOME _, _) => raise Assert.failure
		and parseAuthority cs =
		    let
			val (prefix, sep, suffix) = split isPath cs
		    in
			authority := SOME (toLower prefix);
			case sep of
			    NONE => ()
			  | SOME (#"/" | #"\\") =>
				(absolute := true; parsePath suffix)
			  | SOME #"?" => parseQuery suffix
			  | SOME #"#" => parseFragment suffix
			  | SOME _ => raise Assert.failure
		    end
		and parsePathDev (c::(#":")::cr) =
		    (device := SOME (toDevice c); parsePath cr)
		  | parsePathDev cs = parsePath cs
		and parsePath cs =
		    let
			val (prefix, sep, suffix) = split isPath cs
		    in
			pushPath prefix;
			case sep of
			    NONE => ()
			  | SOME (#"/" | #"\\") => parsePath suffix
			  | SOME #"?" => parseQuery suffix
			  | SOME #"#" => parseFragment suffix
			  | SOME _ => raise Assert.failure
		    end
		and parseQuery cs =
		    let
			val (prefix, sep, suffix) = split isQuery cs
		    in
			query := SOME (String.implode prefix);
			case sep of
			    NONE => ()
			  | SOME #"#" => parseFragment suffix
			  | SOME _ => raise Assert.failure
		    end
		and parseFragment cs = fragment := SOME (String.implode cs)
	    in
		parseStart (String.explode s);
		{scheme = !scheme,
		 authority = !authority,
		 device = !device,
		 absolute = !absolute,
		 path = normalizePath (List.rev (!path)),
		 query = !query,
		 fragment = !fragment}
	    end

	fun slashit nil = nil
	  | slashit (ss as "/"::_) = ss
	  | slashit ss = "/"::ss

	fun toString {scheme, authority, device, absolute, path,
		      query, fragment} =
	    let
		val l = if isSome fragment then ["#", valOf fragment] else nil
		val l = if isSome query then "?"::valOf query::l else l
		val l =
		    List.foldr (fn (s, rest) =>
				encode isPathChar s::slashit rest) nil path @ l
		val l = if absolute then slashit l else l
		val l =
		    if isSome device then String.str (valOf device)::":"::l
		    else l
		val l =
		    if isSome authority then
			"/"::"/"::valOf authority::slashit l
		    else l
		val l = if isSome scheme then valOf scheme::":"::l else l
	    in
		String.concat l
	    end

	fun isAbsolute ({scheme, device, absolute, path, ...}: url) =
	    isSome scheme orelse isSome device orelse absolute orelse
	    (case path of
		 ("." | "..")::_ => true
	       | ""::_ => false
	       | s::_ => String.sub (s, 0) = #"~"
	       | nil => false)

	exception Done of url

	(*--** use {rel where scheme = scheme} instead of setScheme *)

	fun atLast (nil, ss2) = ss2
	  | atLast (ss1, ss2) = List.revAppend (List.tl (List.rev ss1), ss2)

	fun resolve _ (rel as {scheme = SOME _, ...}) = rel
	  | resolve (base as {scheme, authority, device, ...}: url) rel =
	    let
		val rel =
		    if isSome scheme then setScheme (rel, scheme)
		    else rel
		val _ = if isSome (#authority rel) then raise Done rel else ()
		val rel =
		    if isSome authority then setAuthority (rel, authority)
		    else rel
		val _ = if isSome (#device rel) then raise Done rel else ()
		val rel =
		    if isSome device then setDevice (rel, device)
		    else rel
		val _ = if #absolute rel then raise Done rel else ()
		val rel =
		    if #absolute base then makeAbsolutePath rel
		    else rel
		val basePath = #path base
		val relPath =
		    case #path rel of
			nil => [""]
		      | (path as _::_) => path
	    in
		setPath (rel, normalizePath (atLast (basePath, relPath)))
	    end
	    handle Done url => url

	(*--**
	 * Check whether there may be cases where
	 *    URL.fromString s1 <> URL.fromString s2
	 * but
	 *    URL.compare (URL.fromString s1, URL.fromString s2) = EQUAL
	 * This must not happen!
	 *)

	fun compare (url1, url2) =
	    String.compare (toString url1, toString url2)

	fun hash url = StringHashKey.hash (toString url)
    end
