signature MAIN =
sig
    val main : string * string list -> OS.Process.status
    val test : string -> unit
end


structure Main :> MAIN =
struct
    structure LrVals = LrVals(structure Token  = LrParser.Token)
    structure Lexer  = Lexer (structure Tokens = LrVals.Tokens)
    structure Parser = Join  (structure LrParser   = LrParser
			      structure ParserData = LrVals.ParserData
			      structure Lex        = Lexer)

    (* Sometimes I get the feeling they just designed ML-Yacc
     * to scare away people from ever using SML modules again...
     *)
    exception LexerError  = Lexer.UserDeclarations.Error
    exception ParserError = LrVals.ParserData.Header.Error

    fun parse source =
	let
	    val yyread    = ref false
	    fun yyinput _ = if !yyread then "" else (yyread := true; source)
	    val lexer     = Parser.makeLexer yyinput
	    fun onError(s, pos, _) = raise ParserError(pos,s)
	in
	    #1 (Parser.parse(0, lexer, onError, ()))
	end


    fun error s =
	( TextIO.output(TextIO.stdErr, s ^ "\n")
	; OS.Process.failure
	)
    fun errorPos(~1,  s) = error("(end of file): " ^ s)
      | errorPos(pos, s) = error(Int.toString pos ^ ": " ^ s)

    fun main _ =
	(Machine.run(parse(TextIO.inputAll(TextIO.stdIn))); OS.Process.success)
	handle LexerError(pos, s)  => errorPos(pos, "Lexical error: " ^ s)
	     | ParserError(pos, s) => errorPos(pos, "Syntax error: " ^ s)
	     | Machine.Error s     => error("Runtime error: " ^ s)
	     | e => error("Internal error: exception " ^ General.exnName e)

    fun test name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	in
	    TextIO.closeIn file;
	    Machine.run(parse source)
	end
end
