signature ABS_SYN =
    sig

	type position = int * int


	(* make position printable *)

	val posToString : position -> string


	datatype regexp = 
	    EPS
	  | CAT of regexp * regexp * position
	  | CLOSURE of regexp * position
	  | CHARS of BoolVector.vector * int * position
          | ALT of regexp * regexp * position
	  | REGID of string * position
          | END of int


	and regbind = REGBIND of string * regexp * position


	and atexp =
	    ATEXP of string * position
	  | PAREXP of lex list * position
	  | REGCASE of atexp list * lmatch * position

	and exp = EXP of atexp list * position


	and lrule = LRULE of regexp * atexp * position


	and lmatch = LMATCH of lrule list * position


	and lexbind = LEXBIND of string * lmatch * position


	and lex =
            SML of exp * position
	  | REG of regbind list * position
	  | LEX of lexbind list * position

    end

