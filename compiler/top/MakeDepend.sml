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

signature IMPORT_PARSER =
    sig
	val parse: string -> string list
    end

signature MAKE_DEPEND =
    sig
	val analyzeString: string -> string list
	val analyzeFile: string -> string list
    end

functor MkMakeDepend(structure ImportParser: IMPORT_PARSER
		     val extension: string): MAKE_DEPEND =
    struct
	fun warn message =
	    TextIO.output (TextIO.stdErr, "### warning: " ^ message ^ "\n")

	local
	    val homeref: string option ref = ref NONE
	in
	    fun stockhome () =
		case !homeref of
		    NONE =>
			let
			    val home =
				case OS.Process.getEnv "STOCKHOME" of
				    SOME s => s ^ "/"
				  | NONE => (warn "STOCKHOME not set"; "")
			in
			    homeref := SOME home; home
			end
		  | SOME home => home
	end

	fun parseUrl url =
	    case (Url.getScheme url, Url.getAuthority url) of
		(NONE, NONE) =>
		    Url.toString url
	      | (SOME "file", NONE) =>
		    Url.toString (Url.setScheme (url, NONE))
	      | (SOME "x-alice", NONE) =>
		    stockhome () ^
		    Url.toString (Url.setScheme (Url.makeRelativePath url,
						 NONE))
	      | _ => raise Crash.Crash "MkMakeDepend.parseUrl"

	fun pathCeil filename =
	    let
		val fro = "." ^ extension
		val n = String.size filename
		val m = String.size fro
	    in
		if n > m andalso String.substring (filename, n - m, m) = fro
		then filename
		else filename ^ fro
	    end

	fun pathFloor filename =
	    let
		val fro = "." ^ extension
		val n = String.size filename
		val m = String.size fro
	    in
		if n > m andalso String.substring (filename, n - m, m) = fro
		then String.substring (filename, 0, n - m)
		else filename
	    end

	fun urlCeil url =
	    case List.rev (Url.getPath url) of
		last::rest => Url.setPath (url, List.rev (pathCeil last::rest))
	      | nil => url

	fun analyzeImport (desc, url) =
	    let
		val url =
		    case desc of
			SOME base => Url.resolve base url
		      | NONE => url
		val targetFilename = parseUrl url
	    in
		pathFloor targetFilename ^ "." ^ extension
	    end

	fun analyze (desc, source) =
	    List.map (fn s => analyzeImport (desc, Url.fromString s))
	    (ImportParser.parse source)

	fun readFile filename =
	    let
		val file   = TextIO.openIn filename
		val source = TextIO.inputAll file
		val _      = TextIO.closeIn file
	    in
		source
	    end

	fun analyzeString source = analyze (NONE, source)

	fun analyzeFile filename =
	    let
		val url = Url.fromString filename
	    in
		analyze (SOME url, readFile (parseUrl url))
	    end
    end

structure ImportParser :> IMPORT_PARSER = (*--**REWRITE*)
    struct
	datatype token =
	    STRING of string
	  | ID of string

	fun skipComment cs = skipComment' (cs, 1)
	and skipComment' (#"(":: #"*"::rest, level) =
	    skipComment' (rest, level + 1)
	  | skipComment' (#"*":: #")"::rest, level) =
	    if level = 1 then rest else skipComment' (rest, level - 1)
	  | skipComment' (_::rest, level) = skipComment' (rest, level)
	  | skipComment' (nil, _) = raise Crash.Crash "unterminated comment"

	fun getString cs =
	    let
		val (cs, rest) = getString' (cs, nil)
	    in
		(SOME (STRING (String.implode (List.rev cs))), rest)
	    end
	and getString' (#"\""::rest, cs) = (cs, rest)
	  | getString' (#"\\":: #"\n":: #"\\"::rest, cs) =
	    getString' (rest, cs)
	  | getString' (#"\\"::c::rest, cs) = getString' (rest, c::cs)
	  | getString' (c::rest, cs) = getString' (rest, c::cs)
	  | getString' (nil, _) = raise Crash.Crash "unterminated string"

	fun getKeyword cs =
	    let
		val (cs, rest) = getKeyword' (cs, nil)
	    in
		(SOME (ID (String.implode (List.rev cs))), rest)
	    end
	and getKeyword' (c::rest, cs) =
	    if Char.isAlphaNum c then getKeyword' (rest, c::cs)
	    else (cs, c::rest)
	  | getKeyword' (nil, cs) = (cs, nil)

	fun getToken (#"(":: #"*"::rest) = getToken (skipComment rest)
	  | getToken (#"\""::rest) = getString rest
	  | getToken (cs as c::cr) =
	    if Char.isAlpha c then getKeyword cs
	    else getToken cr
	  | getToken nil = (NONE, nil)

	fun tokenize cs =
	    case getToken cs of
		(SOME token, rest) => token::tokenize rest
	      | (NONE, _) => nil

	fun getFrom (ID "from"::STRING url::rest) = (url, rest)
	  | getFrom (_::rest) = getFrom rest
	  | getFrom nil = raise Crash.Crash "missing url"

	fun getImports (ID "import"::tokens) =
	    let
		val (url, rest) = getFrom tokens
	    in
		url::getImports rest
	    end
	  | getImports _ = nil

	fun parse s = getImports (tokenize (String.explode s))
    end

structure MakeDepend =
    MkMakeDepend(structure ImportParser = ImportParser
		 val extension = "ozf")

exception FAIL

fun basename filename =
    let
	val cs = List.rev (String.explode filename)
	fun cutExtension (#"."::rest) =
	    (case rest of
		 (#"/" | #"\\")::_ => cs
	       | _::_ => rest
	       | nil => cs)
	  | cutExtension ((#"/" | #"\\")::_) = cs
	  | cutExtension (_::rest) = cutExtension rest
	  | cutExtension nil = cs
    in
	String.implode (List.rev (case cs of
				      #"."::_ => cs
				    | _ => cutExtension cs))
    end

fun stodep arguments =
    (List.app (fn filename =>
	       let
		   val filenames = MakeDepend.analyzeFile filename
	       in
		   TextIO.print (basename filename ^ ".ozf" ^ ":");
		   List.app (fn filename =>
			     TextIO.print (" \\\n\t" ^ filename)) filenames;
		   TextIO.print "\n\n"
	       end
	       handle Crash.Crash s =>
		      (TextIO.output (TextIO.stdErr,
				      "while processing " ^ filename ^
				      ": crashed: " ^ s ^ "\n");
		       raise FAIL)
		    | e =>
		      (TextIO.output (TextIO.stdErr,
				      "while processing " ^ filename ^
				      ": uncaught exception " ^ exnName e ^
				      "\n");
		       raise FAIL)) arguments;
     OS.Process.success)
    handle FAIL => OS.Process.failure
