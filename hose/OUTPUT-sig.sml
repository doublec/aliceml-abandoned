signature OUTPUT =
    sig

	(* printLexList : prints the outputfile
	 *)
	val printLexList : TextIO.outstream * AbsSyn.lex list * Table.auto_map -> unit

    end
