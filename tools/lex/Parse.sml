structure Parse :> PARSE =
    struct

	(* structure deklarations for ml-yacc and ml-lex *)

	structure HoseLrVals = HoseLrValsFun(structure Token = LrParser.Token)
	    
	structure HoseLex = HoseLexFun(structure Tokens = HoseLrVals.Tokens);

	structure HoseParser = Join(structure LrParser = LrParser
				    structure ParserData = HoseLrVals.ParserData
				    structure Lex = HoseLex)


	fun print_error (s, i, _ ) =
	    print ("Error in structure Parse in file " ^ (!AbsSyn.errorFile)
		   ^ "\nin line " ^ (Int.toString i) ^ ": " ^ s ^ "\n")
	    

	fun invoke lexstream = HoseParser.parse(0, lexstream, print_error, () )
	    
	    
	(* parse : string -> AbsSyn.lex list, returns the abstract datatree
	 *)
	fun parse str = 
	    let
		val _ = HoseLex.UserDeclarations.pos := 1
		val begin = ref true
		fun input _ = if !begin then (begin := false; str) else ""
		val lexer = HoseParser.makeLexer input
		val (result,_) = invoke lexer
	    in
		result
	    end
	
		    
    end
