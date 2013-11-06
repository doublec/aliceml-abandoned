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

structure Config :> CONFIG =
struct

    (* Benötigte Strukturen *)
    open Util;
    open PatternMatching;
    open xml;

    type type_descr = {alicetype : string, toword : string, fromword : string } 
    type descr_map = string * type_descr

    type user_fun = {name : string, typ : string,
		     export : string, body : string }

    datatype filter = 
	REJECT of pattern
      | ACCEPT of pattern
      | RENAME of pattern * pattern
     
    datatype filter_cmd =
	CMD_REJECT
      | CMD_ACCEPT of string

    (* Hier wird die Konfiguation gespeichert *)
    val filters = ref nil : filter list list ref
    val defined_types = ref nil : string list ref
    val types = ref nil : descr_map list ref
    val user_funs = ref nil : user_fun list ref
    val enum_as_int = ref nil : string list ref
    val type_dep = ref nil : (string * bool * string) list ref
    val imports = ref nil : (string * string) list ref
  
    (* Bearbeitet den Filterabschnitt der Konfigurationsdatei *)

    fun handle_filter_entry (T("ignore",STR name)) = REJECT(createPattern name)
      | handle_filter_entry (T("accept",STR name)) = ACCEPT(createPattern name)
      | handle_filter_entry (T("rename",CHILDS[T("from",STR name1),T("to",STR name2)])) = 
	                                             RENAME(createPattern name1, createPattern name2)
     
      | handle_filter_entry _ = raise Error "Wrong filter definition."

    fun handle_filter (T("filter",STR _)) = nil
      |	handle_filter (T("filter",CHILDS fr)) = map handle_filter_entry fr

    (* Wendet den Filter an *)

    fun apply_filter_entry name (REJECT pat) = 
	(case matchPattern(name,pat) of
	     NONE => NONE
	   |(SOME _) => SOME CMD_REJECT)

      | apply_filter_entry name (ACCEPT pat) =
	(case matchPattern(name,pat) of
	     NONE => NONE
	   |(SOME _) => SOME(CMD_ACCEPT(name)))

      | apply_filter_entry name (RENAME(pat1,pat2)) =
	(case matchPattern(name,pat1) of
	     NONE => NONE
	   |(SOME ass) => SOME(CMD_ACCEPT(substPattern(pat2,ass))))

    fun apply_filter name nil = CMD_ACCEPT name
      | apply_filter name (x::xr) = 
	(case get_first_some (apply_filter_entry name) x of
	     NONE => CMD_REJECT
	   |(SOME CMD_REJECT) => CMD_REJECT
	   |(SOME(CMD_ACCEPT name')) => apply_filter name' xr)

    (* Bearbeitet einen define_type Abschnitt in der Config Datei *)

    fun handle_define_type (T("define_type",
		       CHILDS[T("name",STR name)])) = name
      | handle_define_type _ = raise Error "Error in define_type section."

    (* Bearbeitet einen type Abschnitt in der Config Datei *)

    fun handle_type (T("type",
		       CHILDS[T("ctype",STR ctype'),
			      T("alicetype",STR alicetype'),
			      T("toword",STR toword'),
			      T("fromword",STR fromword')])) = (remove_char(ctype',#" "),{alicetype = alicetype', 
									toword = toword', fromword = fromword' })
      | handle_type _ = raise Error "Error in type section."

    (* Bearbeitet die enum_as_int Einträge *)

    fun handle_enum_as_int (T("enum_as_int",STR name)) = name
      | handle_enum_as_int _ = raise Error "Error in enum_as_int."

    (* Bearbeitet einen fun Abschnitt in der Config Datei *)

    fun handle_fun (T("fun",
		      CHILDS[T("name",STR Name),
			     T("type",STR Type),
			     T("export",STR Export),
			     T("body",STR Body)])) = {name = Name, typ = Type, 
						      export = Export, body = Body }
      | handle_fun _ = raise Error "Error in fun section."

    (* Bearbeitet einen type_dep Abschnitt in der Config Datei *)

    fun handle_type_dep (T("type_dep",
		      CHILDS[T("type",STR Name),
			     T("poly",STR n),
			     T("where",STR Where)])) = 
	(case Int.fromString n of
	     NONE => (Name, false, Where)
	   |(SOME 0) => (Name, false, Where)
	   | _ => (Name, true, Where))

      | handle_type_dep _ = raise Error "Error in type_dep section."

    (* Bearbeitet einen import Abschnitt in der Config Datei *)

    fun handle_import (T("import",
		      CHILDS[T("struct",STR Name),
			     T("from",STR From)])) = (Name, From)

      | handle_import _ = raise Error "Error in import section."

    (* Liest die Konfigurationsdatei mit dem Namen name ein *)

    fun readCFGFile name = 
	let
	    val xml = xml_load name

	    val _ = case xml of
		T("xml",_) => ()
	      | _ => raise Error "readCFGLine: Top tag must be \"xml\"."

	    val options = xml_get_child("options",xml)

	    (* Liest die Filter *)
	    val filter_tags = xml_get_child("filter",xml)
	    val _ = filters := map handle_filter filter_tags

	   (* Liest die zu deklarierenden Typen *)
	    val define_type_tags = xml_get_child("define_type",xml)
	    val _ = defined_types := map handle_define_type define_type_tags

	    (* Liest die special types *)
	    val type_tags = xml_get_child("type",xml)
	    val _ = types := map handle_type type_tags

	    (* Liest die user_funs *)
	    val fun_tags = xml_get_child("fun",xml)
	    val _ = user_funs := map handle_fun fun_tags

	    (* Liest die enums ein die als Int behandelt werden sollen *)
	    val enum_tags = xml_get_child("enum_as_int",xml)
	    val _ = enum_as_int := map handle_enum_as_int enum_tags

	    (* Liest die type_dep ein *)
	    val type_dep_tags = xml_get_child("type_dep",xml)
	    val _ = type_dep := map handle_type_dep type_dep_tags

	    (* Liest die imports ein *)
	    val import_tags = xml_get_child("import",xml)
	    val _ = imports := map handle_import import_tags
	in
	    ()
	end

    (* Nimmt eine Zeichenfolge und modifiziert diese anhand der Filter *)

    fun applyFilter name = apply_filter name (!filters)

    fun getDefinedTypes() = !defined_types

    (* Liefert die Sonderbehandlung fuer einen bestimmten Typ zurueck. *)
	
    fun isSpecialType typ = lookup (!types) (remove_char(typ,#" "))

    (* Liefert die Benutzerdefinierten Funktionen zurueck *)

    fun getUserFuns () = !user_funs
     
    (* Soll dieses Enum als int behaldelt werden *)
	
    fun handleAsInt s = isInList (!enum_as_int) s

    fun getTypeDep () = !type_dep

    fun getImports () = !imports

end
