structure Output :> OUTPUT =
    struct

	open AbsSyn


	exception Error of string


	fun oError(e, po) = (print ("Internal Error in structure Output in position " ^ posToString po ^ ": " ^ e ^ "\n");
			     raise Error e)

	(*            \|/
	 * some c++  \'_'/
	 *            _|_
	 *)
	fun ++ x = ( x := !x + 1; !x)
	    

	(* printLexList : outstream * lex list * StringMap.map,
	 * prints the lex list and replaces each lexer declaration with the automaton from the map
	 *)
	fun printLexList (str, l::ls, autoMap) =
	    let
		fun say string = TextIO.output (str, string)


		(* printIdList : string -> string list -> unit,
		 * prints a list separated by "," where each item consists of s ^ 'one-string-from-the-list' 
		 *)
		fun printIdList s (id1::id2::is) = (say (s ^ id1 ^ ", "); printIdList s (id2::is) )
		  | printIdList s [id]           = say (s ^ id)
		  | printIdList _  _             = oError("empty list in printIdList", (~1, ~1))


		(* printLex : lex -> unit, prints lex
		 *)
		fun printLex ( LEX (lbl, _ ) ) = printLexbind lbl
		  | printLex ( REG ( _ , po) ) = oError("still regid in regexp (printLex)", po)
		  | printLex ( SML (e, _ )   ) = printexp e


		and printLexbind lbl =
		    let

			(* getIds : lexbind list -> string list, returns list of lexids
			 *)
			fun getIds ( (LEXBIND (lexid, _ , _ ) ) :: lbl ) is = getIds lbl (lexid :: is)
			  | getIds                                  _    is = rev is


			val idList = getIds lbl nil


			(* printAnnot : string list -> unit, prints all annot_... functions
			 *)
			fun printAnnot idList =
			    let
				val first = ref true

				fun printOne id = (if !first then (first := false; say "fun ") else say "and "; 
						   say ("annot_" ^ id ^ " () = action_" ^ id ^ "((");
						   printIdList "annot_" idList;
						   say "), 0, \"\", 0, 0)\n" )

			    in
				app printOne idList
			    end


			(* printOne : string -> string list -> unit, prints alternately a string from the list and the id,
			 * the output ends with the last string from the list
			 *)
			fun printOne id [s]     = say s
			  | printOne id (s::ss) = (say (s^id); printOne id ss)
			  | printOne _   _      = ()
			

			(* printMain : string list -> string list -> unit, uses printOne for all ids from the first list,
			 * the output ends with a newline
			 *)
			fun printMain (id::is) ss = (printOne id ss; printMain is ss)
			  | printMain   _       _ = say "\n"


			(* printResults : string list -> unit, prints the dummy_... values
			 *)
			fun printResults idList =
			    let
				fun printOne id = (say ("\tval " ^ id ^ " = " ^ id ^ "(string, lexPos, lineNum, (");
						   printIdList "dummy_" idList;
						   say "))\n")
			    in
				app printOne idList
			    end

			(* printFunctions : int -> unit, prints the return value tuple
			 *)
			fun printFunctions num =
			    let
				val n = ref (num - 1)
				fun printOne i = say ("fn string => #" ^ Int.toString i ^ " (build string)")
			    in
				while !n > 0 do
				    (printOne (num - !n);
				     say ", ";
				     n := !n - 1);
				printOne num
			    end

		    in

			say "\n\n(**** DECLARATION OF ";
			printIdList "" idList;
			say " BEGIN ****)\n\n";

			say "val (";
			printIdList "" idList;
			say ")=\nlet\n";

			printLexbindList (lbl, idList);

			say "\n\n(**** some hack to fix type annotation problems (thanks to Andreas Rossberg) ****)\n\n";
			say "val annot_ref = ref 0\n\n";
			printAnnot idList;
			say "\nfun annot f = (fn () => f (\"\", annot_ref, annot_ref, (";
			printIdList "annot_" idList;
			say ")); f)\n\n";

			printMain idList ["val ", " = annot (Lexer.lexer (table_", ", action_", ", final_", "))\n"];

			say "fun build string =\n\tlet\n";
			say "\tval lexPos = ref 0\n";
			say "\tval lineNum = ref 1\n\n";

			printMain idList ["\tval ref_", " = ref NONE\n"];

			printMain idList ["\tfun dummy_", " () = valOf (!ref_", ") ()\n"];

			printResults idList;

			say "\n\tin\n";
			printMain idList ["\tref_", " := SOME ", ";\n"];
			say "\t(";
			printIdList "" idList;
			say ")\n\tend\n";

			say "in\n(";
			printFunctions (length idList);
			say ")\nend\n";


			say "\n\n(**** DECLARATION OF ";
			printIdList "" idList;
			say " END ****)\n\n"

		    end
		    

		and printLexbindList ( (LEXBIND (lexid, _ , _ ) ) :: lbl, idList ) =
		    let
			val (finstates, dtran, atMap) = valOf (StringMap.find(autoMap, lexid) )
	
   
			(* printTrans : IntMap.map -> unit, prints the transition table,
			 *)
			fun printTrans dtran =
			    let
				val tranList = IntMap.listItemsi dtran


				val bigTable =
				    case List.find (fn (x, _ ) => x > 255) tranList of
					NONE => false
				      | _    => true
					    
				(* printOne : int -> int vector, prints one element consisting of 
				 * state number and transition string, the used format depends on the number of states
				 *)
				fun printOne (state, vec) =
				    let
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
				
					fun escape8 x = 
					    let
						val s = ref (Int.toString x)
					    in
						while size (!s) <3 do
						    s := "0" ^ !s;
						"\\" ^ !s
					    end
				
					val chars = ref 0
					    
					fun pretty x =
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
						(say (escape x);
						 ++chars;
						 if !chars mod modulo = 0
						     then say ("\\\n\t" ^ space ^ "\\")
						 else ())
				    end
				
				    in
					say ("(*" ^ Int.toString state ^ "*)\034" );
					Vector.app pretty vec;
					say "\034"
				    end
			

				(* printNum : int * int -> unit, used to print comments to show those states
				 * that are not printed because of minimization
				 *)
				fun printNum (state, state') = say ("(*" ^ Int.toString state ^ "*)\"" ^ Int.toString state' ^ "\"")
				    

				val maxState = ref 0


				val pointList = ref nil


				val actPoint = ref 0


				(* printList : (int * int vector) list * (int * int * int vector) list -> unit,
				 * prints all states in the first list, the second one is an accumulator argument
				 *)				    
				fun printList (((state, vec)::xs), ts) =
				    let
					val stopt = List.find (fn ( _ , _ , v) => v = vec ) ts
				    in
					maxState := state;
					case stopt of
					    NONE                     => (say ",\n\t";
									 printOne (state, vec);
									 pointList := (++ actPoint) :: (!pointList);
									 printList (xs,(!actPoint, state, vec)::ts) )
					  | SOME (point, state', _ ) => (say ("\n\t(*" ^ Int.toString state ^ " -> ");
									 say (Int.toString state' ^ "*)");
									 pointList := point :: (!pointList);
									 printList (xs, ts) )
				    end
				  | printList _       = ()
				   

				val printedStates = ref 0


				(* printPointer : int list -> unit, prints the vector that points to the
				 * transition vector, used to minimize the output
				 *)				
				fun printPointer (p::ps) =
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
					(say (",");
					 if !printedStates mod 16 = 0 then say "\n\t" else ();
					 say ( pretty p);
					 ++ printedStates;
					 printPointer ps)
				    end
				  | printPointer _ = ()



			    in
				say ("val table_" ^ lexid ^ " = \n\tlet\n");
				
				say ("\tval dtran = [ \"\" (* unused state 0 *)");
				printList (tranList, nil);
				say ("]\n\n");
				    
				say ("\tval pointer = [ 0 (* unused state 0 *)");
				printPointer (rev( !pointList ) );
				say ("]\n\n");

				say ("\tin\n\t(Vector.fromList pointer, Vector.fromList dtran, ");
				say ( Bool.toString bigTable ^ ")\n\tend\n\n")
			    end
		
		
			(* printFin : IntMap.map -> unit, prints the final states into a vector
			 *)
			fun printFin finstates =
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
					SOME pos => say(pretty pos)
				      | NONE     => (++printedStates; say "0 (* unused state 0 *)" )
					    
					    
			    in
				say ("val final_" ^ lexid ^ " =\n\tlet\n");
				
				say "\tval finlist = [";
				while ++maxState < numStates + !printedStates - 1 do
				    (printOne (!maxState);
				     say ",";
				     if !maxState mod 16 = 0 then say "\n\t  " else () );
				    printOne (!maxState);
				    say "]\n";
				    say "\tin\n\tVector.fromList finlist\n\tend\n\n"
			    end
			

			(* printAction : IntMap.map -> unit, prints the action function
			 *)		
			fun printAction atMap = 
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
				    (if !first then (first:=false; say "\t  ") else say "\t| ";
				     say (pretty n);
				     say " => ";
				     printAtexp atexp)
			    in
				say ("fun action_" ^ lexid ^ " ((");
				printIdList "" idList;
				say ("), pos, yytext, yyline, yycol) =\n\tcase pos of\n");
				IntMap.appi oneAction atMap;
				say "\t|    _ => raise Domain\n"
			    end
			    
		    in
			say ("\n(**** FINAL-STATES-TABLE of " ^ lexid ^ " ****)\n\n");
			printFin finstates;
			say ("\n(**** TRANSITION-TABLE of " ^ lexid ^ " ****)\n\n");
			printTrans dtran;
			say ("\n(**** ACTION-TABLE of " ^ lexid ^ " ****)\n\n");
			printAction atMap;
			say "\n\n";
			printLexbindList (lbl, idList)
		    end
		  | printLexbindList ( _                             ) = ()


		and printexp ( EXP (al, _ ) ) = printAtexpList al
		    
		    
		and printAtexpList ( b::bs ) = (printAtexp b ; printAtexpList bs) 
		  | printAtexpList         _ = ()
		    

		and printAtexp ( ATEXP (s, _ )   ) = say (s ^ " " )
		  | printAtexp ( PAREXP (ls, _ ) ) = (say ( "( "); printLexList (str, ls, autoMap); say ")\n")
		    

	    in
		(printLex l; printLexList (str, ls, autoMap))
	    end

      | printLexList           _  = ()
	    


    end
