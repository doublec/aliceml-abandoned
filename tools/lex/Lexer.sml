structure Lexer :> LEXER =
    struct

	exception Error 


	fun lError s = (print ("Lex Error: " ^ s ^ "\n"); raise Error)

	(* lexer :
	 * takes an automaton and a string with position-references
	 * and returns the lexing function : unit -> 'a
	 * x is a tuple of the lexids in the same lexbind list
	 *)
	fun lexer ((pointer, dtran, bigTable), action, finstates)
	          (getChar, eof, strBuf, strBack, lexPos, lineNum, x) =
	    let

		val stateStack = ref [1]
		val newLines = ref [~1]
		val firstPos = ref 0


		(* oldChar : unit -> bool
		 * recycling of already read chars
		 *)
		fun oldChar () =
		    case !strBack of
			[]      => false
		      | (x::xs) => (strBuf := x :: !strBuf;
				    strBack := xs;
				    true)


		(* newChar : unit -> unit
		 * puts the next char from getChar to strBuf
		 *)
		fun newChar () =
		    if oldChar () then ()
		    else
			if !eof then strBuf := NONE :: !strBuf
			else
			    case getChar() of
				NONE   => (eof := true;
					   strBuf := NONE :: !strBuf)
			      | SOME c => strBuf := SOME c :: !strBuf


		(* actChar : unit -> int
		 * returns the ord of the actual character
		 *)
		fun actChar () = ord (valOf (hd (!strBuf)))
		    handle Option => 256


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
		     strBack := hd (!strBuf) :: !strBack;
		     strBuf := tl (!strBuf);
		     lexPos := !lexPos - 1)


		(* charList : char option list -> char list -> char list
		 * 
		 *)
		fun charList nil ys            = ys
		  | charList (NONE :: xs) ys   =
		    charList xs (#"F" :: #"O" :: #"E" :: ys)
		  | charList (SOME c :: xs) ys = charList xs (c :: ys)


		fun errorInfo () = implode (rev (charList (!strBack) []))


		fun buildResult p =
		    let
			val len = length (!stateStack) - 1
			val yytext = implode (charList (!strBuf) [])
			val newLines = ref (!newLines)
			val lines = ref 0
			val yycol =
			    (while !firstPos<hd (!newLines) do
				 (newLines := tl (!newLines);
				  lines := !lines + 1);
			     !firstPos - hd (!newLines) ) 
		    in
			stateStack := [1];
			strBuf := [];
			newLines := [hd (!newLines)];
			action (x, p, yytext, !lineNum - !lines, yycol)
		    end


		fun noAction () =
		    let
			val err = errorInfo ()
		    in
			stateStack := [1];
			strBuf := [];
			lexPos := !lexPos + 1;
			lError("no rule matches for '" ^ pretty err
			       ^ "' in line: " ^ Int.toString(!lineNum))
		    end


		(* getAction : unit -> 'a
		 * looks for the longest match and returns
		 * the Action of the END-leaf
		 *)
		fun getAction () =
		    case !stateStack of
			nil           => noAction ()
		      | (state :: _ ) => finAction state
			 

		and finAction state =
		    case Vector.sub (finstates, state) of
			0 => (goBack () handle Empty => noAction();
			      getAction ())
			    
		      | p => buildResult p
			 
			

		(* trans : int -> int
		 * returns the state for a transition
		 * with a character of ord chr
		 *)
		and trans chr =
		    let
			val state = hd (!stateStack)

			fun getTrans (s, c) =
			    if bigTable
				then ord( String.sub (s, 2 * c) ) * 256 +
				    ord( String.sub (s, 2 * c + 1) )
			    else ord( String.sub (s, c) )
		    in
			if chr = 10 then (newLines := !lexPos :: (!newLines);
					  lineNum := !lineNum + 1)
			else ();
			getTrans (Vector.sub
				  (dtran, Vector.sub (pointer, state)), chr)
		    end


		(* lex' : unit -> 'a
		 * returns the longest match token from the actual position on
		 *)
		and lex' () = ( firstPos := !lexPos; lex () )

		and lex () =
		    let
			val chr = (newChar (); actChar ())
		    in
			case trans chr of
			    0 => (if chr = 10 then (newLines := tl (!newLines);
						    lineNum := !lineNum - 1)
				  else ();
				  strBack := hd (!strBuf) :: !strBack;
				  strBuf := tl (!strBuf);
				  getAction () )
			  | n => (stateStack := n :: (!stateStack) ;
				  if chr = 256 then ()
				  else lexPos := !lexPos + 1;
				  lex () )
		    end
	    in
		lex'
	    end


	fun fromString s =
	    let
		val count = ref ~1
	    in
		fn () => ((count := ! count + 1 ;
			   SOME (String.sub ( s , ! count )))
			  handle Subscript => NONE )
	    end 


	fun fromStream instream = fn () => TextIO.input1 instream

    end
