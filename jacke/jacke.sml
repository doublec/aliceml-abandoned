(*structure jackeLrVals =
       jackeLrValsFun(structure Token = LrParser.Token)

structure jackeLex =
    jackeLexFun(structure Tokens = jackeLrVals.Tokens);

structure jackeParser =
    Join(structure LrParser = LrParser
	 structure ParserData = jackeLrVals.ParserData
	 structure Lex = jackeLex);


fun invoke lexstream =
    let fun print_error (s,i:int,_) =
	TextIO.output(TextIO.stdOut,
		      "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
    in jackeParser.parse(0,lexstream,print_error,())
    end;


fun parse () = 
    let val lexer = jackeParser.makeLexer
	(fn _ => TextIO.inputLine TextIO.stdIn)
	val dummyEOF = jackeLrVals.Tokens.EOF(0,0)
	val dummySEMI = jackeLrVals.Tokens.SEMI(0,0)
	fun loop lexer =
	    let val (result,lexer) = invoke lexer
		val (nextToken,lexer) = jackeParser.Stream.get lexer
	    in case result
		of SOME r =>
		    TextIO.output(TextIO.stdOut,
                                  "result = " ^ (Int.toString r) ^ "\n")
	      | NONE => ()
		(*    if jackeParser.sameToken(nextToken,dummyEOF) then ()
		    else loop lexer*)
	    end
    in loop lexer
    end
*)


structure Parse =
struct 
  structure jackeLrVals = jackeLrValsFun(structure Token = LrParser.Token)
  structure Lex = jackeLexFun(structure Tokens = jackeLrVals.Tokens)
  structure jackeP = Join(structure ParserData = jackeLrVals.ParserData
                        structure Lex=Lex
                        structure LrParser = LrParser)
  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
          val file = TextIO.openIn filename
          fun get _ = TextIO.input file
          fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
          val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
          val (absyn, _) = jackeP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
           absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

end

