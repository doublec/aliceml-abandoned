structure Output :> OUTPUT =
    struct
	
	open AbsSyn
	
	
	exception Error of string


	fun oError(e, po) =
	    (print ("Internal Error in structure Output in position "
		    ^ posToString po ^ ": " ^ e ^ "\n");
	     raise Error e)


	fun ++ x = ( x := !x + 1; !x)
	

	(* printIdListLl : (string -> unit) -> string -> string list -> unit,
	 * prints with the output-function say a list separated by ","
	 * where each item consists of s ^ 'one-string-from-the-list'
	 *)
	fun printIdListLl say s (id1::id2::is) = (say [s, id1, ", "];
						printIdListLl say s (id2::is) )
	  | printIdListLl say s [id]           = say [s, id]
	  | printIdListLl  _  _  _             =
	    oError("empty list in printIdList", (~1, ~1))


	(* getIdsLb : lexbind list -> string list
	 * returns list of lexids
	 *)
	fun getIdsLb ( (LEXBIND (lexid, _ , _ ) ) :: lbl ) is =
	    getIdsLb lbl (lexid :: is)
	  | getIdsLb                                  _    is = rev is


	(* printOneLb : (string -> unit) -> string -> string list -> unit
	 * prints (with say) alternately a string from the list and the id,
	 * the output ends with the last string from the list
	 *)
	fun printOneLb say id [s]     = say [s]
	  | printOneLb say id (s::ss) = (say [s, id]; printOneLb say id ss)
	  | printOneLb  _  _  _       = ()
			

	(* printOneAn :
	 * help function for printAnnotLb
	 *)
	fun printOneAn say pIdList idList first id =
	    (if !first then (first := false; say ["fun "])
	     else say ["and "];
	     say ["annot_", id, " () = action_", id, "(("];
	     pIdList "annot_" idList;
	     say ["), 0, \"\", 0, 0)\n"] )


	(* printAnnotLb :
	 * prints all annot_... functions
	 *)
	fun printAnnotLb say pIdList idList =
	    let
		val first = ref true
		
		val pOneAn = printOneAn say pIdList idList first
	    in
		app pOneAn idList
	    end


	(* printMainLb :
	 * uses pOneLb for all ids from the first list,
	 * the output ends with a newline
	 *)
	fun printMainLb say pOneLb (id::is) ss = (pOneLb id ss;
						  printMainLb say pOneLb is ss)
	  | printMainLb say  _      _        _ = say ["\n"]


	(* printResultsLb :
	 * prints the dummy_... values
	 *)
	fun printResultsLb say pIdList idList =
	    let
		fun printOne id =
		    (say ["    val ", id, " = ", id,
			  " (getString, strBuf, eof, lexPos, delPos, ",
			  "lineNum, ("];
		     pIdList "dummy_" idList;
		     say ["))\n"])
	    in
		app printOne idList
	    end


	(* printFunctionsLb :
	 * prints the return value tuple
	 *)
	fun printFunctionsLb say num =
	    if num = 1 then say ["fn getString => build getString"]
	    else
		let
		    val n = ref (num - 1)
		    fun printOne i =
			say ["fn getString => #", Int.toString i,
			     " (build getString)"]
		in
		    while !n > 0 do
			(printOne (num - !n);
			 say [", "];
			 n := !n - 1);
			printOne num
		end
	

	(* escape16: int -> string
	 * transforms x into a two-digit string (basis = 256)
	 *)
	fun escape16 x =
	    let
		val hi = ref (Int.toString(x div 256))
		val lo = ref (Int.toString(x mod 256))
	    in
		while size (!hi) <3 do
		    hi := "0" ^ !hi;
		while size (!lo) <3 do
		    lo := "0" ^ !lo;
		"\\" ^ !hi ^ "\\" ^ !lo
	    end
	
	
	(* escape8: int -> string
	 * transforms x into a one-digit string (basis = 256)
	 *)
	fun escape8 x =
	    let
		val s = ref (Int.toString x)
	    in
		while size (!s) <3 do
		    s := "0" ^ !s;
		"\\" ^ !s
	    end
		

	fun prettyOneTr say bigTable state chars x =
	    let
		val space =
		    let
			val n = ref state
			val s = ref "     "
			fun dec n = (n := !n div 10; !n)
		    in
			while dec n > 0 do
			    s := !s ^ " ";
			    !s
		    end
		
		val modulo = if bigTable then 8 else 16
		val escape = if bigTable then escape16 else escape8
		    
	    in
		(say [escape x];
		 ++chars;
		 if !chars mod modulo = 0
		     then say ["\\\n  ", space, "\\"]
		 else ())
	    end
		
	(* printOneTr :
	 * prints one element consisting of
	 * state number and transition string,
	 * the used format depends on the number of states
	 *)
	fun printOneTr say bigTable (state, vec) =
	    let
		val chars = ref 0
		
		val pretty = prettyOneTr say bigTable state chars
	    in
		say ["(*", Int.toString state, "*)\034"];
		Vector.app pretty vec;
		say ["\034"]
	    end


	(* printListTr :
	 * prints all states in the first list of the tuple,
	 * the second one is an accumulator argument
	 *)				
	fun printListTr say pOneTr maxState pointList actPoint
	               (((state, vec)::xs), ts) =
	    let
		val stopt = List.find (fn ( _ , _ , v) => v = vec ) ts
	    in
		maxState := state;
		case stopt of
		    NONE =>
			(say [",\n  "];
			 pOneTr (state, vec);
			 pointList := (++ actPoint) :: (!pointList);
			 printListTr say pOneTr maxState pointList actPoint
		                     (xs,(!actPoint, state, vec)::ts)
			 )
		  | SOME (point, state', _ ) =>
			(say ["\n  (*", Int.toString state, " -> ", 
			      Int.toString state', "*)"];
			      pointList := point :: (!pointList);
			      printListTr say pOneTr maxState pointList actPoint
			                  (xs, ts)
			      )
	    end
	  | printListTr _ _ _ _ _ _      = ()


	(* printPointerTr :
	 * prints the vector that points to the
	 * transition vector, used to minimize the output
	 *)				
	fun printPointerTr say printedStates (p::ps) =
	    let
		fun pretty pos =
		    let
			val s = ref (Int.toString pos)
		    in
			while size (!s) < 5 do
			    s := " " ^ !s;
			    !s
		    end
		
	    in
		(say [","];
		 if !printedStates mod 10 = 0 then say ["\n  "] else ();
		     say [pretty p];
		     ++ printedStates;
		     printPointerTr say printedStates ps)
	    end
	  | printPointerTr _ _ _ = ()


	(* printTransLbL :
	 * prints the transition table,
	 *)
	fun printTransLbL say lexid dtran =
	    let
		val tranList = IntMap.listItemsi dtran
		    
		val bigTable =
		    case List.find (fn (x, _ ) => x > 255) tranList of
			NONE => false
		      | _    => true
			    
		val pOneTr = printOneTr say bigTable
		    
		val maxState = ref 0
		    
		val pointList = ref nil
		    
		val actPoint = ref 0
		    
		val pListTr = printListTr say pOneTr maxState pointList actPoint

		val printedStates = ref 0

		val pPointTr = printPointerTr say printedStates
	    in
		say ["val table_", lexid, " = \n  let\n"];
		
		say ["  val dtran = [ \"\" (* unused state 0 *)"];
		pListTr (tranList, nil);
		say ["]\n\n"];
		
		say ["  val pointer = [ 0 (* unused state 0 *)"];
		pPointTr (rev( !pointList ) );
		say ["]\n\n"];
		
		say ["  in\n  (Vector.fromList pointer, \
		 \Vector.fromList dtran, "];
		say [Bool.toString bigTable, ")\n  end\n\n"]
	    end
		
		
	(* printFinLbL :
	 * prints the final states into a vector
	 *)
	fun printFinLbL say lexid finstates =
	    let
		val maxState = ref ~1
		val printedStates = ref 0
		val numStates = IntMap.numItems finstates
		    
		fun pretty pos =
		    let
			val s = ref (Int.toString pos)
		    in
			while size (!s) < 5 do
			    s := " " ^ !s;
		        !s
		    end
		
		fun printOne state =
		    case IntMap.find(finstates, state) of
			SOME pos => say [pretty pos]
		      | NONE     => (++printedStates;
				     say ["0 (* unused state 0 *)"] )
			    
	    in
		say ["val final_", lexid, " =\n",
		     "  let\n",
		     "    val finlist = ["];
		while ++maxState < numStates + !printedStates - 1 do
		    (printOne (!maxState);
		     say [","];
		     if !maxState mod 10 = 0 then say ["\n    "] else () );
		printOne (!maxState);
		say ["]\n",
		     "  in\n  Vector.fromList finlist\n  end\n\n"]
	    end
			

	(* printActionLbL :
	 * prints the action function
	 *)		
	fun printActionLbL say printAtexp pIdList lexid idList atMap =
	    let
		val first = ref true
		fun pretty n =
		    let
			val s = ref (Int.toString n)
		    in
			while size (!s) < 4 do
			    s := " " ^ !s;
			    !s
		    end
		fun oneAction (n, atexp) =
		    (if !first then (first:=false; say ["    "])
		     else say ["  | "];
		     say [pretty n];
		     say [" => "];
		     printAtexp atexp)
	    in
		say ["fun action_", lexid, " (("];
		pIdList "" idList;
		say ["), pos, yytext, yyline, yycol) =\n  case pos of\n"];
		IntMap.appi oneAction atMap;
		say ["  |    _ => raise Domain\n"]
	    end
		
	
	(* printLexList : outstream * lex list * StringMap.map,
	 * prints the lex list and replaces each lexer declaration
	 * with the automaton from the map
	 *)
	fun printLexList (str, l::ls, autoMap) =
	    let
		fun say (s :: ss) = (TextIO.output (str, s); say ss)
		  | say  _ = ()

		val pIdList = printIdListLl say


		(* printLex : lex -> unit
		 * prints lex
		 *)
		fun printLex ( LEX (lbl, _ ) ) = printLexbind lbl
		  | printLex ( REG ( _ , po) ) =
		    oError("still regid in regexp (printLex)", po)
		  | printLex ( SML (e, _ )   ) = printexp e


		and printLexbind lbl =
		    let

			val idList = getIdsLb lbl nil

			val pAnLb = printAnnotLb say pIdList

			val pOneLb = printOneLb say

			val pMainLb = printMainLb say pOneLb

			val pResLb = printResultsLb say pIdList

			val pFunLb = printFunctionsLb say

		    in

			say ["\n\n(**** BEGIN DECLARATION OF "];
			pIdList "" idList;
			say [" ****)\n\n"];

			say ["val ("];
			pIdList "" idList;
			say [")=\nlet\n"];

			printLexbindList (lbl, idList);

			say ["\n\n(*** some hack to fix type annotation \
			 \problems (thanks to Andreas Rossberg) ***)\n\n"];
			pAnLb idList;
			say ["\nfun annot f = (fn () => f (\
			 \fn () => SOME \"\", ref \"\", ref true, \
			 \ref 0, ref 0, ref 0, ("];
			pIdList "annot_" idList;
			say [")); f)\n\n"];

			pMainLb idList ["val ", " = annot (Lexer.lexer \
			 \(table_", ", action_", ", final_", "))\n"];

			say ["fun build getString =\n",
			     "  let\n",
			     "    val strBuf = ref \"\"\n",
			     "    val eof = ref false\n",
			     "    val lexPos = ref 0\n",
			     "    val delPos = ref 0\n",
			     "    val lineNum = ref 1\n\n"];

			pMainLb idList ["    val ref_", " = ref NONE\n"];

			pMainLb idList ["    fun dummy_", " () = \
			 \valOf (!ref_", ") ()\n"];

			pResLb idList;

			say ["\n  in\n"];
			pMainLb idList ["    ref_", " := SOME ", ";\n"];
			say ["    ("];
			pIdList "" idList;
			say [")\n  end\n"];

			say ["in\n    ("];
			pFunLb (length idList);
			say [")\nend\n"];


			say ["\n\n(**** END DECLARATION OF "];
			pIdList "" idList;
			say [" ****)\n\n"]

		    end
		

		and printLexbindList ((LEXBIND (lexid, _, _)) :: lbl, idList ) =
		    let
			val (finstates, dtran, atMap) =
					valOf (StringMap.find(autoMap, lexid) )
					
			val pTransLbL = printTransLbL say lexid
					
			val pFinLbL = printFinLbL say lexid
					
			val pActLbL = printActionLbL say printAtexp pIdList
					lexid idList

		    in
			say ["\n(**** FINAL-STATES-TABLE of ", lexid,
			      " ****)\n\n"];
			pFinLbL finstates;
			say ["\n(**** TRANSITION-TABLE of ", lexid,
			     " ****)\n\n"];
			pTransLbL dtran;
			say ["\n(**** ACTION-TABLE of ", lexid, " ****)\n\n"];
			pActLbL atMap;
			say ["\n\n"];
			printLexbindList (lbl, idList)
		    end
		  | printLexbindList _  = ()


		and printexp ( EXP (al, _ ) ) = printAtexpList al
		
		
		and printAtexpList ( b::bs ) = (printAtexp b ;
						printAtexpList bs)
		  | printAtexpList         _ = ()
		

		and printAtexp ( ATEXP (s, _ )   ) = say [s, " "]
		  | printAtexp ( PAREXP (ls, _ ) ) =
		    (say ["( "];
		     printLexList (str, ls, autoMap);
		     say [")\n"])
		  | printAtexp ( REGCASE (al, lm, pos) ) =
		    let
			val lexid = "reglex" ^ posToString pos
		    in
			(say ["\n\n(**** BEGIN REGCASE DECLARATION OF ",
			      lexid,
			      " ****)\n\n",
			      "let\n\n"];
			 printLexbind [LEXBIND (lexid, lm, pos)];
			 say ["in\n"];
			 say [lexid, "(Lexer.fromString (\n"];
			 printAtexpList al;
			 say [")) ()\n"];
			 say ["end\n\n"]
			 )
		    end 
		
	    in
		(printLex l; printLexList (str, ls, autoMap))
	    end

      | printLexList           _  = ()
	


    end
