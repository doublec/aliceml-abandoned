signature COLLECT =
    sig
	(* collect : replaces all regids with there value
	 * and removes their declaration
	 * the string contains the filename (only used for error messages)
	 *)
	val collect : AbsSyn.lex list * string -> AbsSyn.lex list
    end
