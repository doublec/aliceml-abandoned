(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Builtins :> BUILTINS =
    struct
	structure Map = MakeHashImpMap(StringHashKey)

	val builtinTable =
	    let
		val map = Map.new ()
	    in
		Map.insert (map, "=", "opeq");
		Map.insert (map, "<>", "opnoteq");
		Map.insert (map, "Char.<", "Char_opless");
		Map.insert (map, "Char.>", "Char_opgreater");
		Map.insert (map, "Char.<=", "Char_oplessEq");
		Map.insert (map, "Char.>=", "Char_opgreaterEq");
		Map.insert (map, "General.:=", "General_assign");
		Map.insert (map, "Int.~", "Int_opnegate");
		Map.insert (map, "Int.+", "Int_opadd");
		Map.insert (map, "Int.-", "Int_opsub");
		Map.insert (map, "Int.*", "Int_opmul");
		Map.insert (map, "Int.<", "Int_opless");
		Map.insert (map, "Int.>", "Int_opgreater");
		Map.insert (map, "Int.<=", "Int_oplessEq");
		Map.insert (map, "Int.>=", "Int_opgreaterEq");
		Map.insert (map, "Real.~", "Real_opnegate");
		Map.insert (map, "Real.+", "Real_opadd");
		Map.insert (map, "Real.-", "Real_opsub");
		Map.insert (map, "Real.*", "Real_opmul");
		Map.insert (map, "Real./", "Real_opdiv");
		Map.insert (map, "Real.<", "Real_opless");
		Map.insert (map, "Real.>", "Real_opgreater");
		Map.insert (map, "Real.<=", "Real_oplessEq");
		Map.insert (map, "Real.>=", "Real_opgreaterEq");
		Map.insert (map, "String.^", "String_append");
		Map.insert (map, "String.<", "String_opless");
		Map.insert (map, "String.>", "String_opgreater");
		Map.insert (map, "String.<=", "String_oplessEq");
		Map.insert (map, "String.>=", "String_opgreaterEq");
		Map.insert (map, "Word.fromInt'", "Word_fromIntQuote");
		Map.insert (map, "Word.+", "Word_opadd");
		Map.insert (map, "Word.-", "Word_opsub");
		Map.insert (map, "Word.*", "Word_opmul");
		Map.insert (map, "Word.<<", "Word_shl");
		Map.insert (map, "Word.>>", "Word_shr");
		Map.insert (map, "Word.~>>", "Word_arithshr");
		map
	    end

	fun lookup name =
	    case Map.lookup (builtinTable, name) of
		SOME res => res
	      | NONE =>
		    let
			val res = String.map (fn #"." => #"_" | c => c) name
		    in
			Map.insertDisjoint (builtinTable, name, res); res
		    end

	fun lookupClass name = ["Alice", "Builtins", lookup name]

	fun lookupField name = (["Alice", "Prebound"], lookup name)
    end
