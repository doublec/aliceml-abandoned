structure Extract :> EXTRACT= 
    struct

	open AbsSyn


	type lex_map = (regexp * atexp IntMap.map) StringMap.map


	fun eError (e, po) =
	    raise Error ("Error in structure Extract in file " ^ (!errorFile)
			 ^ "\nin line(s) " ^ posToString po ^ ": " ^ e ^ "\n")


	fun ieError (e, po) =
	    raise Error ("Internal Error in structure Extract in file "
			 ^ (!errorFile) ^ "\nin line(s) "
			 ^ posToString po ^ ": " ^ e ^ "\n")


	(* getLexer : lex list * lmatch StringMap.map -> lmatch StringMap.map
	 * returns the given map, additionally containing the lmatch
	 * for each key (lex-id) found in the lex list
	 *)
	fun getLexer ( (SML (e, po) )   :: ll, map) =
	    let
		val map' = fromExp (e, map)
	    in
		getLexer (ll, map')
	    end
	  | getLexer ( (REG (rbl, po) ) :: ll, map) = getLexer (ll, map)
	  | getLexer ( (LEX (lbl, po) ) :: ll, map) = 
	    let
		val map' = fromLbL (lbl, map)
	    in
		getLexer (ll, map')
	    end
	  | getLexer (              _ , map) = map


	and fromExp ( EXP (atl, po), map) = fromAtL (atl, map)


	and fromAtL ( (ATEXP (s, po) )   :: atl, map) = fromAtL (atl, map)
	  | fromAtL ( (PAREXP (ll, po) ) :: atl, map) = 
	    let
		val map' = getLexer (ll, map)
	    in
		fromAtL (atl, map')
	    end
	  | fromAtL ( (REGCASE (atl', lm, po) ) :: atl, map) =
	    let
		val map' = fromAtL (atl', map)
		val lexid = "reglex" ^ posToString po
		val map'' =
		    if StringMap.inDomain (map', lexid)
			then eError("lexer id already defined: " ^ lexid, po)
		    else StringMap.insert(map', lexid, lm)
		val map''' = fromLm (lm, map'') 
	    in
		fromAtL (atl, map''')
	    end
	  | fromAtL (                 _ , map) = map


	and fromLbL ( (LEXBIND (s, lm, po) ) :: lbl, map) = 
	    let
		val map' = if StringMap.inDomain (map,s)
			       then eError("lexer id already defined: " ^ s, po)
			   else StringMap.insert(map, s, lm)
		val map'' = fromLm (lm, map')
	    in
		fromLbL (lbl, map'')
	    end
	  | fromLbL (                       _ , map) = map


	and fromLm ( LMATCH (lrl, po), map) = fromLrL (lrl, map)


	and fromLrL ( (LRULE ( _ , ate, po) ) :: lrl, map) =
	    let
		val map' = fromAtL ([ate], map)
	    in
		fromLrL (lrl, map')
	    end
	  | fromLrL (                       _ , map) = map


	(* numLm : lmatch -> regexp * atexp IntMap.map,
	 * the map contains for each key (finishing position of the regexp)
	 * the associated action (atexp)
	 *)
	fun numLm ( LMATCH (lrl, po) ) = 
	    let
		val i = ref 0
		val endmap = IntMap.empty
		val result = numLrL (lrl, i, endmap, [ ], po)
	    in
		result
	    end


	and numLrL ( (LRULE ( re , ate, po1) ) :: lrl, i, endmap, result, po) =
	    let
		val re' = numReg (re, i)
		val endmap' = IntMap.insert(endmap, !i, ate)
	    in
		numLrL ( lrl, i, endmap', re' :: result, po )
	    end
	  | numLrL ( _ , _, endmap, result, po) = (concat (result, po), endmap)


	(* numerate the leafs from left to right
	 *)
	and numReg (EPS                , _) = EPS
	  | numReg (CAT (re1, re2 , po), i) = CAT (numReg (re1,i),
						   numReg (re2,i), po )
	  | numReg (CLOSURE (re, po)   , i) = CLOSURE (numReg (re,i), po )
	  | numReg (CHARS (bv, _ , po) , i) = ( i:= !i + 1; CHARS (bv, !i, po) )
	  | numReg (ALT (re1, re2, po) , i) = ALT (numReg (re1,i),
						   numReg (re2,i), po )
	  | numReg (REGID (s, po)      , i) =
	    ieError("still regid in regexp :" ^ s, po)
	  | numReg (END _              , i) = ( i:= !i + 1; END (!i) )


	(* merge more than lrules to one regexp
	 *)
	and concat ( re1 :: re2 :: rl, po) =
	    ALT (concat (re2 :: rl, po), re1, po)
	  | concat (             [re], _ ) = re
	  | concat (              nil, po) =
	    ieError ("empty list in concat!", po)


	(* extract : lex list -> (regexp * atexp IntMap.map) StringMap.map,
	 * returns a map containing for each key (found lex-id) the pair of
	 * regexp and finishing-action-map
	 *)
	fun extract ll = 
	    let
		val map = StringMap.empty
		val map' = getLexer(ll, map)
	    in
		StringMap.map numLm map'
	    end


    end
