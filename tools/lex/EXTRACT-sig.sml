signature EXTRACT =
    sig

	type lex_map = (AbsSyn.regexp * AbsSyn.atexp IntMap.map) StringMap.map


	(* extract : returns a map containing for each key (found lexid) the pair of regexp and finishing-action-map
	 * the string contains the filename only used for error messages
	 *)
	val extract : AbsSyn.lex list * string -> lex_map

    end
