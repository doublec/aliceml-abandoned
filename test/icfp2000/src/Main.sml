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

    fun parse source =
	let
	    val yyread    = ref false
	    fun yyinput _ = if !yyread then "" else (yyread := true; source)
	    val lexer     = Parser.makeLexer yyinput

(*	    fun onError(s, pos1, pos2) = E.error((pos1,pos2), E.SyntaxError s)
*)	    fun onError(s, pos1, pos2) = raise Fail s
	in
	    #1 (Parser.parse(0, lexer, onError, ()))
	end

    fun main _ =
	(Machine.run(parse(TextIO.inputAll(TextIO.stdIn))); OS.Process.success)
	handle _ => OS.Process.failure

    fun test name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	in
	    TextIO.closeIn file;
	    Machine.run(parse source)
	end
	handle Machine.Error s => print(s ^ "\n")
end
