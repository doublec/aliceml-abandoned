structure Collect :> COLLECT =
    struct

	open AbsSyn


	exception Error of string

	val errorFile = ref "?"

	fun cError (e, po) =
	    (print(  "Error in file " ^ (!errorFile)
		   ^ " in line " ^ posToString po ^ ": " ^ e ^ "\n");
	     raise Error e)


	(* lookup : StringMap.map * string * position -> regexp
	 * returns the regexp for the id
	 *)
	fun lookup (map, id, po) = 
	    case StringMap.find (map, id) of
		SOME (re, _ ) => re
	      | NONE          => cError( "unbound regid '" ^ id ^ "'", po)


	(* insert : StringMap.map * string * (regexp * position) -> StringMap
	 * inserts the regexp for the regid into the map and returns the new Map
	 *)
	fun insert (map, id, (re, po) ) =
	    if StringMap.inDomain (map, id)
		then cError( "regid '"  ^ id ^ "' already declared", po)
	    else StringMap.insert(map, id, (re, po))


	(* checkReg : regexp * StringMap.map -> regexp
	 * replaces all regids in the regexp if they are in the map
	 *)
	fun checkReg ( CAT (re1, re2, po), map ) =
	    CAT (checkReg (re1,map), checkReg (re2,map), po)
	  | checkReg ( CLOSURE (re, po)  , map ) =
	    CLOSURE (checkReg (re,map), po)
	  | checkReg ( ALT (re1, re2, po), map ) =
	    ALT (checkReg (re1,map), checkReg (re2,map), po)
	  | checkReg ( REGID (id, po)    , map ) = lookup (map, id, po)
	  | checkReg ( re                ,   _ ) = re


	(* checkRegList : regbind list * StringMap.map -> StringMap.map
	 * all regbinds into the map 
	 *)
	fun checkRegList ( REGBIND (id, re, po ) :: rbl, map) =
	    let
		val re' =  checkReg (re,map)
		val map' = insert (map, id, (re', po) )
	    in
		checkRegList (rbl, map' )
	    end
	  | checkRegList (                      _ , map ) = map


	(* substLexList : lex list * StringMap.map -> lex list * StringMap.map
	 * removes all REGBINDs from the lex list
	 * and inserts their values into the LEXBINDs,
	 * returns the new lex list and a map containing all regbinds
	 *)
	fun substLexList (ll, map) = substLexList' (ll, map, [ ]) 


	and substLexList' ( (SML (e, po ) )   :: ll, map, result) =
	    let
		val (e', map') = substExp (e,map)
	    in
		substLexList' (ll, map',(SML (e', po) ) :: result)
	    end
	  | substLexList' ( (REG (rbl, _ ) ) :: ll, map, result) =
	    let
		val  map' = checkRegList (rbl, map) 
	    in
		substLexList' (ll, map',result )
	    end
	  | substLexList' ( (LEX (lbl, po) ) :: ll, map, result) =
	    let
		val (lbl', map') = substLexbindList (lbl,map)
	    in
		substLexList' (ll, map', (LEX (lbl', po) ) :: result)
	    end
	  | substLexList' (              _ , map, result) = (rev result, map)


	and substExp ( EXP (al, po), map ) =
	    let
		val (al', map') = substAtList (al, map)
	    in
		(EXP (al', po), map')
	    end

	
	and substAtList ( al, map ) = substAtList' (al, map, [ ] ) 

	    
	and substAtList' ( (ATEXP (s, po) )   :: al, map, result) =
	    substAtList' (al, map, (ATEXP (s, po) )::result )
	  | substAtList' ( (PAREXP (ll, po) ) :: al, map, result) =
	    let
		val (ll', map') = substLexList (ll, map)
	    in
		substAtList' (al, map', (PAREXP (ll', po) ) :: result)
	    end
	  | substAtList' ( (REGCASE (al', lm, po) ) :: al, map, result) =
	    let
		val (al'', map') = substAtList (al', map)
		val (lm', map'') = substLmatch (lm, map')
	    in
		substAtList' (al, map, (REGCASE (al'', lm', po)) :: result)
	    end
          | substAtList' ( _ , map, result) = (rev result, map)
	    
	and substLexbindList (lbl, map) = substLexbindList' (lbl, map, [ ] )


	and substLexbindList' ( (LEXBIND (s, lm, po) ) :: lbl, map, result) =
	    let
		val (lm', map') = substLmatch (lm, map)
	    in
		substLexbindList' (lbl, map', (LEXBIND (s, lm', po)) :: result )
	    end
	  | substLexbindList' ( _ ,  map, result) =(rev result, map)

	    
	and substLmatch ( LMATCH (lrl, po), map) =
	    let
		val (lrl', map') = substLruleList (lrl, map)
	    in
		(LMATCH (lrl', po), map')
	    end

	    
	and substLruleList ( lrl, map ) = substLruleList' (lrl, map, [ ] )

	    
	and substLruleList' ( (LRULE (re, ate, po) ) :: lrl, map, result) =
	    let
		val re' = checkReg (re,map)
		val (atl, map') = substAtList ( [ate], map )
		val ate' = hd atl
	    in
		(* lets add the final-state-token END *)
		substLruleList'
		(lrl, map', (LRULE (CAT (re', END 0, po), ate', po) ) :: result)
	    end
	  | substLruleList' ( _ , map, result) = (rev result, map)
		

	(* collect : lex list * string -> lex list
	 * replaces all regids with there value and removes their declaration
	 *)
	fun collect (lexlist, fileName) = 
	    let
		val _ = errorFile := fileName
		val eofVector = BoolVector.tabulate(257, fn x => x = 256)
		val eofChar = CHARS (eofVector, 0, (~1, ~1))
		val eofMap = StringMap.singleton ("eof", (eofChar, (~1, ~1)) )
		val (ll, _ ) = substLexList (lexlist, eofMap)
	    in
		ll
	    end

    end
