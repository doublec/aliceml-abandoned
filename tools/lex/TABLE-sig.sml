signature TABLE =
    sig

	type auto_map = (int IntMap.map
			 * int vector IntMap.map
			 * AbsSyn.atexp IntMap.map) StringMap.map


	(* makeAuto :
	 * returns a map containing the minimized automata
	 * and the actions for each automaton
	 *)
	val makeAuto : Extract.lex_map -> auto_map
 
    end
