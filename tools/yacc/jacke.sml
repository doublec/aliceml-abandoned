(* main parsing routines
   - constructing parser
   - generating parse tree
   - performing semantic analysis
*)

(* replaced 'lexer' with 'lexxer' because of problems with hose *)


structure Parse =
struct 
  structure jackeLrVals = jackeLrValsFun(structure Token = LrParser.Token)
  structure Lex = jackeLexFun(structure Tokens = jackeLrVals.Tokens)
  structure jackeP = Join(structure ParserData = jackeLrVals.ParserData
			  structure Lex=Lex
			  structure LrParser = LrParser)

  fun invole lexstream =
      let fun printError (s,i:int,_) = TextIO.output(TextIO.stdOut,
						       "Error, line "^(Int.toString i) ^", "^s^"\n")
      in jackeP.parse(0,lexstream,printError,())
      end

  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
          val file = TextIO.openIn filename
          fun get _ = TextIO.input file
          fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
          val lexxer = LrParser.Stream.streamify (Lex.makeLexer get)
          val (absyn, _) = jackeP.parse(30,lexxer,parseerror,())
	  val _ = AbsSyn.semanticalAnalysis absyn
	  val absyn = AbsSyn.removePos absyn
       in TextIO.closeIn file;	   
           absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

  (* testing purposes only *)
  fun try filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
          val file = TextIO.openIn filename
          fun get _ = TextIO.input file
          fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
          val lexxer = LrParser.Stream.streamify (Lex.makeLexer get)
          val (absyn, _) = jackeP.parse(30,lexxer,parseerror,())
       in TextIO.closeIn file;	   
           absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error
end
