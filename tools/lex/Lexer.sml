structure Lexer :> LEXER =
    struct

	exception Error 


	fun lError s = (print ("Lex Error: " ^ s ^ "\n"); raise Error)

	(* lexer : takes an automaton and a string with position-references
	 * and returns the lexing function : unit -> 'a
	 * x is a tuple of the lexids in the same lexbind list
	 *)
	fun lexer ((pointer, dtran, bigTable), action, finstates)
	          (string, lexPos, lineNum, x)  =
	    let

		val stateStack = ref [1]
		val newLines = ref [~1]
		val firstPos = ref 0
		val numBack = ref 0
		val inputSize = size string


		(* actChar : unit -> int
		 * returns the ord of the actual character
		 *)
		fun actChar () = (ord (String.sub (string, !lexPos) ) )
		    handle Subscript => 256


		(* pretty : string -> string
		 *replaces "\n" and "\t" with "\\n" and "\\t" in the string
		 *)
		fun pretty s =
		    let
			val cs = explode s
			fun p ( nil, xs ) = implode (rev xs)
			  | p (#"\n"::cs, xs ) = p (cs, #"n" :: #"\\" :: xs)
			  | p (#"\t"::cs, xs ) = p (cs, #"t" :: #"\\" :: xs)
			  | p (c::cs, xs) = p (cs, c::xs)
		    in
			p (cs, nil)
		    end


		(* goBack : unit -> unit
		 * sets all necessary parameters one step back
		 *)
		fun goBack () =
		    (if actChar() = 10
			 then (newLines := tl (!newLines);
			       lineNum := !lineNum - 1)
		     else ();
		     stateStack := tl (!stateStack);
		     lexPos := !lexPos - 1;
		     numBack := !numBack + 1)


		(* finAction : int -> int
		 * returns the position of the END-leaf for the state
		 *)
		fun finAction state = Vector.sub( finstates, state)
				    

		fun errorInfo () = 
		    if !lexPos < inputSize
			then (substring(string, !lexPos + 1, !numBack ))
		    else "EOF"
			handle Subscript =>
			    substring(string, !lexPos + 1,!numBack - 1 ) ^ "EOF"


		(* getAction : unit -> 'a
		 * looks for the longest match and returns
		 * the Action of the END-leaf
		 *)
		fun getAction () =
		    (case !stateStack of
			nil => 
			    let
				val err = errorInfo ()
			    in
				stateStack := [1];
				numBack := 0;
				lexPos := !lexPos + 1;
				lError("no rule matches for '" ^ pretty err
				       ^ "' in line: " ^ Int.toString(!lineNum))
			    end
		      | (state :: _ ) => 
			    (case finAction state of
				 0  => (goBack ();
					getAction ())
			       | p => let
					  val len = length (!stateStack) - 1
					  val yytext = substring (string, !lexPos - len, len)
					      handle Subscript => ""
					  val newLines = ref (!newLines)
					  val lines = ref 0
					  val yycol = (while !firstPos < hd (!newLines) do (newLines := tl (!newLines);
											    lines := !lines + 1);
						       !firstPos - hd (!newLines)  ) 
				      in
					  stateStack := [1];
					  numBack := 0;
					  newLines := [hd (!newLines)];
					  action (x, p, yytext, !lineNum - !lines, yycol)
				      end))


		(* trans : int -> int
		 * returns the state for a transition
		 * with a character of ord chr
		 *)
		and trans chr =
		    let
			val state = hd (!stateStack)

			fun getTrans (s, c) =
			    if bigTable then ord( String.sub (s, 2 * c) ) * 256 + ord( String.sub (s, 2 * c + 1) )
			    else ord( String.sub (s, c) )
		    in
			if chr = 10 then (newLines := !lexPos :: (!newLines); lineNum := !lineNum + 1) else ();
			getTrans( Vector.sub( dtran,  Vector.sub (pointer, state) ), chr )
		    end


		(* lex' : unit -> 'b, returns the longest match token from the actual position on
		 *)
		and lex' () = ( firstPos := !lexPos; lex () )

		and lex () =
		    let
			val chr = actChar ()
		    in
			case trans chr of
			    0 => (if chr = 10 then (newLines := tl (!newLines); lineNum := !lineNum - 1) else ();
				  getAction () )
			  | n => (stateStack := n :: (!stateStack) ;
				  if chr = 256 then () else lexPos := !lexPos + 1;
				  lex () )
		    end
	    in
		lex'
	    end



    end
