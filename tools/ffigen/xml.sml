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


structure xml :> XML =
struct

    open Util

    datatype token = 
	OPEN_TAG of string 
      | CLOSE_TAG of string 
      | TEXT of string

    fun print_tokens nil = print "\n"
      | print_tokens ((OPEN_TAG s)::xr) = (print("<"^s^"> "); print_tokens xr)
      | print_tokens ((CLOSE_TAG s)::xr) = (print("</"^s^"> "); print_tokens xr)
      | print_tokens ((TEXT s)::xr) = (print("["^s^"] "); print_tokens xr)
	
    datatype tag = 
	T of string * tagentry
    and tagentry =
	STR of string 
      | CHILDS of tag list

    fun getTagContent' nil acc = (String.implode(rev acc),nil)
      | getTagContent' (x::xr) acc = if x <> #"<" then getTagContent' xr (x::acc)
				  else (String.implode(rev acc),x::xr)

    fun getTagContent xr = getTagContent' xr nil
	
    fun getTagName' nil acc = (String.implode(rev acc),nil)
      | getTagName' (x::xr) acc = if x <> #">" then getTagName' xr (x::acc)
				  else (String.implode(rev acc),xr)
    fun getTagName xr = getTagName' xr nil
       
    fun lexerXML nil = nil 
      |	lexerXML ((#" ")::xr) = lexerXML xr
      | lexerXML ((#"\n")::xr) = lexerXML xr
      | lexerXML ((#"\t")::xr) = lexerXML xr
      | lexerXML ((#"<")::(#"!")::xr) =  let val (name,xr') = getTagName xr
					 in lexerXML xr' end
      | lexerXML ((#"<")::(#"/")::xr) = let val (name,xr') = getTagName xr
					in (CLOSE_TAG name)::(lexerXML xr') end
      | lexerXML (#"<"::xr) = let val (name,xr') = getTagName xr
			      in (OPEN_TAG name)::(lexerXML xr') end
      | lexerXML xr = let val (text,xr') = getTagContent xr
		      in (TEXT text)::(lexerXML xr') end

    fun parseXML'' (xr as ((CLOSE_TAG name)::_)) acc = (rev acc,xr)
      | parseXML'' ((TEXT _)::_) acc = raise Error "parseXML'': unexpexted text."
      | parseXML'' xr acc = let val (tag,xr') = parseXML xr
			in parseXML'' xr' (tag::acc) end

    and parseXML' ((TEXT txt)::xr) = (STR txt,xr)
      |	parseXML' (xr as ((CLOSE_TAG _)::_)) = (STR "",xr)
      | parseXML' xr = let val (lst,xr') = parseXML'' xr nil
		       in (CHILDS lst,xr') end

    and parseXML ((OPEN_TAG name)::xr) = 

	let val (entry,xr') = parseXML' xr
	in case xr' of
	    ((CLOSE_TAG name')::xr'') => 
		if name = name' then (T(name,entry),xr'')
		else raise Error ("ParserXML: Closing tag has not the same name as opening tag: " ^ name ^ "<>" ^ name')
	  | ((OPEN_TAG name')::xr) => raise Error ("ParserXML: Unexpected opening tag: "^name')
	  | ((TEXT name')::xr) => raise Error ("ParserXML: Unexpected text: "^name')
	  | nil => raise Error ("ParserXML: Unexpected end of file.")
	end

    fun xml_load name =  
	let 
	    val file = TextIO.openIn name
	    val _ = print ("loading "^name^"...\n")
	    val lst = String.explode(TextIO.inputAll file)
	    val _ = TextIO.closeIn file

	    val _ = print "lexing...\n"
	    val tokens = lexerXML lst
	    (*val _ = print_tokens tokens*)

	    val _ = print "parsing...\n"
	    val (tag,xr) = parseXML tokens

	    val _ = print "(done)\n"

	in case xr of 
	    nil => tag
	  | _ => raise Error "loadXML: File has unneeded tags at the end."
	end
    
    fun getTags name nil = nil
      | getTags name ((tag as (T(name',_)))::tr) = 
	if name = name' then tag::(getTags name tr)
	else getTags name tr

    fun xml_get_child(name,T(_,STR _)) = nil
      | xml_get_child(name,T(_,CHILDS tr)) = getTags name tr

    fun xml_get_childs(T(_,STR _)) = nil
      | xml_get_childs(T(_,CHILDS tr)) = tr
end
