signature EXTRACT =
    sig

	type lex_map = (AbsSyn.regexp * AbsSyn.atexp IntMap.map) StringMap.map

	(* extract :
	 * returns a map containing for each key (found lexid)
	 * the pair of regexp and finishing-action-map
	 *)
	val extract : AbsSyn.lex list -> lex_map

    end
