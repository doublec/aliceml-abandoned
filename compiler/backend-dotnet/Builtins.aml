(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
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
		Map.insert (map, "=", "op_Equality");
		Map.insert (map, "<>", "op_Inequality");
		Map.insert (map, "Char.<", "Char_op_LessThan");
		Map.insert (map, "Char.>", "Char_op_GreaterThan");
		Map.insert (map, "Char.<=", "Char_op_LessThanOrEqual");
		Map.insert (map, "Char.>=", "Char_op_GreaterThanOrEqual");
		Map.insert (map, "Future.alarm'", "Future_alarmQuote");
		Map.insert (map, "General.:=", "General_op_Assignment");
		Map.insert (map, "Int.~", "Int_op_UnaryNegation");
		Map.insert (map, "Int.+", "Int_op_Addition");
		Map.insert (map, "Int.-", "Int_op_Subtraction");
		Map.insert (map, "Int.*", "Int_op_Multiply");
		Map.insert (map, "Int.<", "Int_op_LessThan");
		Map.insert (map, "Int.>", "Int_op_GreaterThan");
		Map.insert (map, "Int.<=", "Int_op_LessThanOrEqual");
		Map.insert (map, "Int.>=", "Int_op_GreaterThanOrEqual");
		Map.insert (map, "Real.~", "Real_op_UnaryNegation");
		Map.insert (map, "Real.+", "Real_op_Addition");
		Map.insert (map, "Real.-", "Real_op_Subtraction");
		Map.insert (map, "Real.*", "Real_op_Multiply");
		Map.insert (map, "Real./", "Real_op_Division");
		Map.insert (map, "Real.<", "Real_op_LessThan");
		Map.insert (map, "Real.>", "Real_op_GreaterThan");
		Map.insert (map, "Real.<=", "Real_op_LessThanOrEqual");
		Map.insert (map, "Real.>=", "Real_op_GreaterThanOrEqual");
		Map.insert (map, "String.^", "String_op_Concatenation");
		Map.insert (map, "String.<", "String_op_LessThan");
		Map.insert (map, "String.>", "String_op_GreaterThan");
		Map.insert (map, "String.<=", "String_op_LessThanOrEqual");
		Map.insert (map, "String.>=", "String_op_GreaterThanOrEqual");
		Map.insert (map, "Word.fromInt'", "Word_fromIntQuote");
		Map.insert (map, "Word.+", "Word_op_Addition");
		Map.insert (map, "Word.-", "Word_op_Subtraction");
		Map.insert (map, "Word.*", "Word_op_Multiply");
		Map.insert (map, "Word.<<", "Word_op_LeftShift");
		Map.insert (map, "Word.>>", "Word_op_UnsignedRightShift");
		Map.insert (map, "Word.~>>", "Word_op_SignedRightShift");
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
