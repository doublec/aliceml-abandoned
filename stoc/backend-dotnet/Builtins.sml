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
		val map: (IL.dottedname * string) Map.t = Map.new ()
		val Char = ["Char"]
		val General = ["General"]
		val GlobalStamp = ["GlobalStamp"]
		val Int = ["Int"]
		val Real = ["Real"]
		val String = ["String"]
		val Word = ["Word"]
	    in
		Map.insert (map, "=", (nil, "opeq"));
		Map.insert (map, "<>", (nil, "opnoteq"));
		Map.insert (map, "Char.<", (Char, "opless"));
		Map.insert (map, "Char.>", (Char, "opgreater"));
		Map.insert (map, "Char.<=", (Char, "oplessEq"));
		Map.insert (map, "Char.>=", (Char, "opgreaterEq"));
		Map.insert (map, "General.:=", (General, "assign"));
		Map.insert (map, "Int.~", (Int, "opnegate"));
		Map.insert (map, "Int.+", (Int, "opadd"));
		Map.insert (map, "Int.-", (Int, "opsub"));
		Map.insert (map, "Int.*", (Int, "opmul"));
		Map.insert (map, "Int.<", (Int, "opless"));
		Map.insert (map, "Int.>", (Int, "opgreater"));
		Map.insert (map, "Int.<=", (Int, "oplessEq"));
		Map.insert (map, "Int.>=", (Int, "opgreaterEq"));
		Map.insert (map, "Real.~", (Real, "opnegate"));
		Map.insert (map, "Real.+", (Real, "opadd"));
		Map.insert (map, "Real.-", (Real, "opsub"));
		Map.insert (map, "Real.*", (Real, "opmul"));
		Map.insert (map, "Real./", (Real, "opdiv"));
		Map.insert (map, "Real.<", (Real, "opless"));
		Map.insert (map, "Real.>", (Real, "opgreater"));
		Map.insert (map, "Real.<=", (Real, "oplessEq"));
		Map.insert (map, "Real.>=", (Real, "opgreaterEq"));
		Map.insert (map, "String.^", (String, "append"));
		Map.insert (map, "String.<", (String, "opless"));
		Map.insert (map, "String.>", (String, "opgreater"));
		Map.insert (map, "String.<=", (String, "oplessEq"));
		Map.insert (map, "String.>=", (String, "opgreaterEq"));
		Map.insert (map, "Word.fromInt'", (Word, "fromIntQuote"));
		Map.insert (map, "Word.+", (Word, "opadd"));
		Map.insert (map, "Word.-", (Word, "opsub"));
		Map.insert (map, "Word.*", (Word, "opmul"));
		Map.insert (map, "Word.<<", (Word, "shl"));
		Map.insert (map, "Word.>>", (Word, "shr"));
		Map.insert (map, "Word.~>>", (Word, "arithshr"));
		map
	    end

	fun lookup name =
	    case Map.lookup (builtinTable, name) of
		SOME res => res
	      | NONE =>
		    let
			val ids =
			    List.rev (String.tokens (fn c => c = #".") name)
		    in
			(List.rev (List.tl ids), List.hd ids)
		    end

	fun allTails (ids as _::rest) = ids::allTails rest
	  | allTails nil = nil

	local
	    fun sep' (x::xr, s) = s::x::sep' (xr, s)
	      | sep' (nil, _) = nil
	in
	    fun sep (x::xr, s) = String.concat (x::sep' (xr, s))
	      | sep (nil, _) = ""
	end

	fun makePath dottedname =
	    sep (List.map (fn dottedname => sep (List.rev dottedname, "$"))
		 (List.rev (allTails (List.rev dottedname))), "/")

	fun lookupClass name =
	    (* Alice.opeq *)
	    (* Alice.Array/Array$sub *)
	    (* Alice.Unsafe/Unsafe$Array/Unsafe$Array$sub *)
	    let
		val (dottedname, id) = lookup name
	    in
		["Alice", makePath (dottedname @ [id])]
	    end

	fun lookupField name =
	    (* Alice.Prebound::opeq *)
	    (* Alice.Prebound/Prebound$Array::sub *)
	    (* Alice.Prebound/Prebound$Unsafe/Prebound$Unsafe$Array::sub *)
	    let
		val (dottedname, id) = lookup name
	    in
		(["Alice", makePath ("Prebound"::dottedname)], id)
	    end
    end
