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
		val map: (string * IL.ty) Map.t = Map.new ()
		val cty = StockWerk.ConstructorTy
		val ty = StockWerk.StockWertTy
	    in
		Map.insert (map, "cons", ("cons", cty));
		Map.insert (map, "ByNeed", ("ByNeed", cty));
		Map.insert (map, "=", ("eq", ty));
		Map.insert (map, "<>", ("ne", ty));
		Map.insert (map, ":=", ("General$assign", ty));
		Map.insert (map, "<", ("Int$less", ty));
		Map.insert (map, "~", ("Int$uminus", ty));
		Map.insert (map, "-", ("Int$minus", ty));
		Map.insert (map, "+", ("Int$plus", ty));
		Map.insert (map, "*", ("Int$times", ty));
		Map.insert (map, ">", ("Int$gt", ty));
		Map.insert (map, "<=", ("Int$le", ty));
		Map.insert (map, ">=", ("Int$ge", ty));
		Map.insert (map, "div", ("Int$div", ty));
		Map.insert (map, "mod", ("Int$mod", ty));
		Map.insert (map, "String.^", ("String$conc", ty));
		Map.insert (map, "Word.+", ("Word$plus", ty));
		Map.insert (map, "Word.-", ("Word$minus", ty));
		Map.insert (map, "Word.*", ("Word$times", ty));
		Map.insert (map, "Word.<<", ("Word$shr", ty));
		Map.insert (map, "Word.>>", ("Word$lsr", ty));
		Map.insert (map, "Word.~>>", ("Word$asr", ty));
		Map.insert (map, "Word.fromInt'", ("Word$fromInt2", ty));
		map
	    end

	fun lookup name =
	    case Map.lookup (builtinTable, name) of
		SOME (id, ty) => (id, ty)
	      | NONE =>
		    (String.map (fn c => if c = #"." then #"$" else c) name,
		     StockWerk.StockWertTy)
    end
