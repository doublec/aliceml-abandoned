functor Parse  (structure Grammar_Core: GRAMMAR_CORE
					where type Info = Source.position
		structure Grammar_Module: GRAMMAR_MODULE
					where Core = Grammar_Core
					where type Info = Source.position
		structure Grammar_Program: GRAMMAR_PROGRAM
					where Module = Grammar_Module
					where type Info = Source.position

		structure DerivedForms_Core:    DERIVEDFORMS_CORE
					where Grammar = Grammar_Core
		structure DerivedForms_Module:  DERIVEDFORMS_MODULE
					where Grammar = Grammar_Module
		structure DerivedForms_Program: DERIVEDFORMS_PROGRAM
					where Grammar = Grammar_Program

		structure Error: ERROR  where type position = Source.position)
:> PARSE where Grammar = Grammar_Program
= struct

    (* Import *)

    structure Grammar = Grammar_Program

    type source  = Source.source
    type Program = Grammar.Program


    (* Build Yacc parser *)

    structure LrVals = LrVals(structure Token  = LrParser.Token
			      structure Error  = Error
			      structure Grammar_Core    = Grammar_Core
			      structure Grammar_Module  = Grammar_Module
			      structure Grammar_Program = Grammar_Program
			      structure DerivedForms_Core   = DerivedForms_Core
			      structure DerivedForms_Module = DerivedForms_Module
			      structure DerivedForms_Program =
							DerivedForms_Program)

    structure Lexer  = Lexer (structure Tokens = LrVals.Tokens
			      structure Source = Source
			      structure Error  = Error)

    structure Parser = Join  (structure LrParser   = LrParser
			      structure ParserData = LrVals.ParserData
			      structure Lex        = Lexer)


    (* The actual parsing function *)

    fun parse source =
	let
	    val yyread = ref false
	    fun yyinput _ =
		if !yyread then
		    ""
		else
		    ( yyread := true; source )

	    val lexer = Parser.makeLexer yyinput

	    fun onError(s, pos1, pos2) = Error.error((pos1,pos2), s)
	in
	    #1 (Parser.parse(0, lexer, onError, ()))
	end

  end
