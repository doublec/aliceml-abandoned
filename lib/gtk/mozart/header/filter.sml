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
	val filterFile : string -> string -> OS.Process.status
    end

structure Filter : FILTER =
    struct
	fun doPurify tokens =
	    (case tokens of
		 "__extension__"::sr         => doPurify sr
	       | "__ssize_t"::sr             => "unsigned int " ^ (doPurify sr)
	       | "__const"::sr               => "const " ^ (doPurify sr)
	       | "(__const"::sr              => "(const " ^ (doPurify sr)
	       | "*__const"::sr              => "* " ^ (doPurify sr)
	       | "__attribute__"::sr         => ";"
	       | "__attribute__((format"::sr => ";"
	       | s::sr                       => s ^ " " ^ (doPurify sr)
	       | nil                         => "")

	fun sepToken #" "  = true
	  | sepToken #"\t" = true
	  | sepToken #"\n" = true
	  | sepToken _     = false

	fun purify s = doPurify (String.tokens sepToken s)

	fun isNoCodeLine s =
	    (case String.explode s of
		 #"#"::_ => true
	       | _       => false)

	fun doFiltering(rs, ws) =
	    (case TextIO.endOfStream rs of
		 true  => ()
	       | false =>
		     let
			 val s = TextIO.inputLine rs
		     in
			 case isNoCodeLine s of
			     true  => doFiltering(rs, ws)
			   | false =>
				 (case purify s of
				      "" => doFiltering(rs, ws)
				    | l  =>(TextIO.output (ws, (l ^ "\n")); doFiltering(rs, ws)))
		     end)
		 
	fun filterFile inFile outFile =
	    let
		val rs = TextIO.openIn inFile
		val ws = TextIO.openOut outFile
	    in
		(doFiltering(rs, ws);
		 TextIO.closeIn rs;
		 TextIO.closeOut ws;
		 OS.Process.success)
	    end
    end

local
    fun main _ =
	(OS.Process.system "gcc -E `gtk-config --cflags gtk` -o gtkraw.c gtk.c";
	 Filter.filterFile "gtkraw.c" "gtkheader.c";
	 OS.Process.system "rm -f gtkraw.c")
in
    val _ = SMLofNJ.exportFn("filter", main)
end
