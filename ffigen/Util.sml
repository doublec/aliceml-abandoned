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


structure Util = 
struct

    open TypeTree

    (* Standardfehlermeldung *)
    exception Warning of string
    exception Error of string

    (* Erstellt aus einer Liste von strings [a,b,c] den string a*b*c *)
    local
	fun Product' nil = ""
	  | Product' [s] = s
	  | Product' (""::sr) = Product' sr
	  | Product' (s::""::sr) = Product' (s::sr)
	  | Product' (s::sr) = s ^ " * " ^ Product' sr
    in
	fun Product nil = "unit"
	  | Product xr = Product' xr
    end
	
    (* Erstellt aus einer Liste von strings [a,b,c] den string a*b*c *)
    fun Kommas nil = ""
      | Kommas [s] = s
      | Kommas (s::sr) = s ^ "," ^ Kommas sr

    (* Konkateniert eine Liste von strings *)

    fun concat(nil,sep) = ""
      | concat([x],sep) = x
      | concat(x::y::xr,sep) = x ^ sep ^ (concat(y::xr,sep))
  
    (* Zerlegt einen Dateinamen in Name und Erweiterung *)
    local 
	fun split' xr nil = (String.implode(rev xr),"")
	  | split' xr (#"."::yr) = (String.implode(rev xr), String.implode yr)
	  | split' xr (y::yr) = split' (y::xr) yr
    in
	fun split_filename s = split' nil (String.explode s)
    end

	
    (* Spezielles Mapping auf Listen *)
    fun map' f nil = nil
      | map' f (x::xr) = (case f x of 
			      NONE => map' f xr
			    | (SOME x) => x :: (map' f xr))

    (* Zerlegt einen String anhand eines Trennzeichens *)
    local 
	fun remove_sep nil sep = nil
	  | remove_sep (x::xr) sep = 
	    if x = sep then remove_sep xr sep else (x::xr)

	fun take nil sep r = (rev r,nil)
	  | take (x::xr) sep r = 
	      if x = sep then (rev r,remove_sep xr sep)
	      else take xr sep (x::r)

	fun separate' nil sep r = rev r
	  | separate' xr sep r = 
	      let val (str,rest) = take xr sep nil
	      in separate' rest sep ((String.implode str)::r)
	      end 
    in
	fun separate(str,sep) = separate' (String.explode str) sep nil
    end

    (* Löscht ein Zeichen aus einem String *)

    fun remove_char(s,c) = String.implode(List.filter (fn x => x <> c) (String.explode s))

    (* Liest alle Zeilen aus einer Textdatei und gibt diese in 
     Form einer Liste zurück *)

    fun readLines file = 
	if TextIO.endOfStream file then nil
	else let val l = remove_char(TextIO.inputLine file,#"\n")
	     in 
		 if l = "" orelse String.isPrefix "//" l then readLines file
		 else l::(readLines file) 
	     end

    (* Entfernt alle Elemente einer Liste bis zu dem Element s (ausschließlich s)*)

    fun removeHead nil s = nil
      | removeHead (x::xr) s = if x = s then xr else removeHead xr s

    (* Liefert alle Elemente bis zu dem ersten Auftreten von s zurück *)

    fun removeTail nil s = nil
      | removeTail (x::xr) s = if x = s then nil else x::(removeTail xr s)

    (* Stellt fest ob das Element c in der Liste vorkommt *)

    fun occurs nil c = false
      | occurs (x::xr) c = if x = c then true else occurs xr c

    (* Spaltet eine Liste anhand des Elements c *)

    fun split_list s c = (removeTail s c,removeHead s c)

    (* Einfache Lookup Funktion *)

    fun lookup nil x = NONE
      | lookup ((x,y)::xr) x' = if x = x' then SOME y else lookup xr x'

    (* Prüft ob die Funktion f für ein Listenelement true liefert *)

    fun list_or f nil = false
      | list_or f (x::xr) = if f x then true else list_or f xr

    (* Liefert das erste Element zurueck auf dem die funktion nicht NONE liefert. *)

    fun get_first_some f nil = NONE
      | get_first_some f (x::xr) = (case f x of 
					NONE => get_first_some f xr
				      |(SOME y) => SOME y)

    (* Sucht ein Element in einer Liste *)

    fun isInList nil c = false
      | isInList (x::xr) c = (c = x) orelse (isInList xr c)

    (* Liefert maximales Element einer Liste *)

    fun maxInt nil = 0
      | maxInt [x] = x
      | maxInt (x::xr) = 
	let val r = maxInt xr
	in if x > r then x else r end

    (* Schreibt ein Wort groß *)

    fun Capital nil = nil
      | Capital (x::xr) = (Char.toUpper x)::xr

    (* Partial Mapping for Lists *)

    fun mapPartial f nil = nil
      | mapPartial f (x::xr) = 
	(case f x of
	     NONE => mapPartial f xr
	   |(SOME x') => x' ::(mapPartial f xr))
	
end
