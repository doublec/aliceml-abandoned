structure ParsingPhase :> PARSING_PHASE =
  struct

    (* Import *)

    structure C = EmptyContext
    structure I = Source
    structure O = InputGrammar
    structure E = ParsingError


    (* Build Yacc parser *)

    structure LrVals = LrVals(structure Token        = LrParser.Token
			      structure DerivedForms = DerivedForms)

    structure LexerError = LexerError(structure Tokens = LrVals.Tokens
				      type error       = ParsingError.error)

    structure Lexer  = Lexer (structure Tokens     = LrVals.Tokens
			      structure LexerError = LexerError)

    structure Lexer' = CountPosLexer(structure Lexer      = Lexer
				     structure LexerError = LexerError
				     val error            = ParsingError.error)

    structure Parser = Join  (structure LrParser   = LrParser
			      structure ParserData = LrVals.ParserData
			      structure Lex        = Lexer')


    (* The actual parsing function *)

    fun parse source =
	let
	    val yyread = ref false
	    fun yyinput _ =
		if !yyread then
		    ""
		else
		    ( yyread := true; Source.toString source )

	    val lexer = Parser.makeLexer yyinput

	    fun onError(s, pos1, pos2) = E.error((pos1,pos2), E.SyntaxError s)
	in
	    #1 (Parser.parse(0, lexer, onError, ()))
	end

    fun translate() = parse

  end
