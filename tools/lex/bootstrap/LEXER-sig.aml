signature LEXER =
    sig


	(* the string contains some information about the position the error
	 * occurred
	 *)
	exception RegMatch of string

	val lexer :
	    (* the automaton *)
	    (int vector              (* pointer to the transition table *)
	     * string vector         (* transition table *)
	     * bool)                 (* are there more than 255 states *)
	    *
	    (* the action-function *)
	    ('a                      (* tuple of lexer in the same and-block *)
	     * int                   (* number of action *)
	     * string                (* yytext *)
	     * int                   (* yyline *)
	     * int                   (* yycol *)
	     -> 'b)                  (* action *)
	    *
	    int vector               (* final states *)
	    ->
	    (* the input handling *)
	    (unit -> string option)  (* getString-function *)
	    * string ref             (* string buffer (strBuf) *)
	    * bool ref               (* eof reached? *)
	    * int ref                (* absolute lexing position *) 
	    * int ref                (* relative (to strBuf) lexing position *)
	    * int ref                (* line counter *)
	    * 'a                     (* the lexer tuple again *)
	    ->
	    (unit -> 'b)             (* the lexer *)


	(* some getString generators *)

	val fromString : string -> (unit -> string option)

	val fromList   : string list -> (unit -> string option)

	val fromStream : TextIO.instream -> (unit -> string option)

    end
