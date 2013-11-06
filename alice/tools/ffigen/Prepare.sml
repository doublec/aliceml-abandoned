(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

structure Prepare :> PREPARE =
struct
    open TextIO

    local
	val c = ref 0
    in
	fun incCounter () = c := !c + 1
	fun decCounter () = if !c > 0 then (c := !c - 1; true) else false
    end

    fun keyFilter ks =
	case ks of
	    "("::"(noreturn)"::")"::";"::kr => keyFilter kr
	  | k::kr =>
	      (case k of
		   "__extension__"              => ""
		 | "__ssize_t"                  => "unsigned int"
		 | "(__const"                   => "(const"
		 | "__const"                    => "const"
		 | "*__const"                   => "* "
		 | "__restrict"                 => ""
		 | "*__restrict"                => "* "
		 | "**__restrict"               => "** "
		 | "__attribute__"              => ";"
		 | "__attribute__((format"      => (incCounter(); ";")
		 | "__attribute__(("            => (incCounter(); "")
		 | "__attribute__((__cdecl__))" => ""
		 | "__attribute__((dllimport))" => ""
		 | "__builtin_va_list"          => "void *"
		 | "dllimport"                  => ""
		 | "))"                         => 
			   if decCounter() then "" else "))"
                 | "inline"                     => ""
		 | k                            => k
	      ) :: (keyFilter kr)
	  | nil  => nil

    fun rebuildLine ts =
	case ts of
	     nil => ""
 	 | t::tr => t ^ " " ^ (rebuildLine tr)

    local
	fun delim c =
	    case c of
		#" "  => true
	      | #"\t" => true
	      | #"\n" => true
	      | _     => false 
    in
	fun filterLine line =
	    case explode line of
                nil      => ""
	      | #"#"::_  => ""
	      | _        => rebuildLine(keyFilter(String.tokens delim line))
    end

    fun filterLines fin fout =
	case inputLine fin of
	    "" => ()
	| line => (case filterLine line of
		       "" => ()
		     | l  => output(fout, l^"\n")
		   ; filterLines fin fout)

    fun prepare inFile outFile =
	let val status = OS.Process.system ("gcc -E -Wall -undef -U__GNUC__ `pkg-config --cflags "^
					    "gtk+-2.0 libgnomecanvas-2.0` "^inFile^" > expanded.tmp")
	in if status = OS.Process.failure then 
	    print "Error: gcc returned error code!\n"
	   else let val fin = openIn "expanded.tmp"
		    val fout = openOut outFile
		in
		    filterLines fin fout;
		    closeOut fout;
		    closeIn fin
		end
	end
end
