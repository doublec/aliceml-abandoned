(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature FILTER =
    sig
	val filterFile : string list -> OS.Process.status
    end

structure Filter :> FILTER =
    struct
	fun doPurify tokens =
	    case tokens of
		"__extension__"::sr         => doPurify sr
	      | "__ssize_t"::sr             => "unsigned int " ^ doPurify sr
	      | "__const"::sr               => "const " ^ doPurify sr
	      | "(__const"::sr              => "(const " ^ doPurify sr
	      | "*__const"::sr              => "* " ^ doPurify sr
	      | "__restrict"::sr            => doPurify sr
	      | "*__restrict"::sr           => "* " ^ doPurify sr
	      | "**__restrict"::sr          => "** " ^ doPurify sr
	      | "__attribute__"::sr         => ";"
	      | "__attribute__((format"::sr => ";"
	      | s::sr                       => s ^ " " ^ doPurify sr
	      | nil                         => ""

	fun purify s = doPurify (String.tokens Char.isSpace s)

	fun isNoCodeLine s =
	    case String.explode s of
		#"#"::_ => true
	      | _       => false

	fun doFiltering(rs, ws) =
	    case TextIO.inputLine rs of
		"" => ()
	      | s =>
		    (if isNoCodeLine s then ()
		     else
			 case purify s of
			     "" => ()
			   | l  => TextIO.output (ws, l ^ "\n");
		     doFiltering(rs, ws))

	fun filterFile [inFile, outFile] =
	    let
		val rs = TextIO.openIn inFile
		val ws = TextIO.openOut outFile
	    in
		doFiltering(rs, ws);
		TextIO.closeIn rs;
		TextIO.closeOut ws;
		OS.Process.success
	    end
	  | filterFile _ =
	    (TextIO.output (TextIO.stdErr,
			    "Usage: filter <infile> <outfile>\n");
	     OS.Process.failure)
    end
