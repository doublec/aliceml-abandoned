signature PARSE =
    sig
	(* parse : returns the abstract datatree
	 *)
	val parse : string -> AbsSyn.lex list
    end
