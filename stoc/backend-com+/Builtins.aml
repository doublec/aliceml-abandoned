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
		val map: string Map.t = Map.new ()
	    in
		Map.insert (map, "=", "eq");
		Map.insert (map, "<>", "ne");
		Map.insert (map, ":=", "General$assign");
		Map.insert (map, "<", "Int$less");
		Map.insert (map, "~", "Int$uminus");
		Map.insert (map, "-", "Int$minus");
		Map.insert (map, "+", "Int$plus");
		Map.insert (map, "*", "Int$times");
		Map.insert (map, ">", "Int$gt");
		Map.insert (map, "<=", "Int$le");
		Map.insert (map, ">=", "Int$ge");
		Map.insert (map, "div", "Int$div");
		Map.insert (map, "mod", "Int$mod");
		Map.insert (map, "String.^", "String$conc");
		Map.insert (map, "Word.+", "Word$plus");
		Map.insert (map, "Word.<<", "Word$shr");
		Map.insert (map, "Word.>>", "Word$lsr");
		Map.insert (map, "Word.~>>", "Word$asr");
		Map.insert (map, "Word.fromInt'", "Word.fromInt2");
		map
	    end

	fun lookup name =
	    case Map.lookup (builtinTable, name) of
		SOME s => s
	      | NONE => String.map (fn c => if c = #"." then #"$" else c) name
    end
