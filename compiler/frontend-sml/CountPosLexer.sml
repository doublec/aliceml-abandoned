signature LEXER_ERRORS =
  sig
    type error
    exception Error of Source.pos * error
    val toString: error -> string
  end

functor CountPosLexer(
	structure Lexer: LEXER
	where type UserDeclarations.pos = int
	where type ('a,'b) UserDeclarations.token = ('a,'b) LrParser.Token.token
	structure LexerError: LEXER_ERRORS
) : LEXER =
  struct

    structure UserDeclarations =
      struct
	open Lexer.UserDeclarations
	type pos = Source.pos
      end

    fun makeLexer yyinput =
	let
	    val lin = ref 1
	    val col = ref 0
	    val buf = ref ""
	    val pos = ref 0

	    fun count(pos, newPos, lin, col) =
		if pos = newPos then
		    (lin,col)
		else case String.sub(!buf, pos)
		    of #"\n" => count(pos+1, newPos, lin+1, 0)
		     | #"\t" => count(pos+1, newPos, lin, col+8-(col mod 8))
		     |  _    => count(pos+1, newPos, lin, col+1)

	    fun transform(pos1, pos2) =
		let
		    val pos1' as (lin1,col1) = count(!pos, pos1, !lin, !col)
		    val pos2' as (lin2,col2) = count(pos1, pos2, lin1, col1)
		in
		    lin := lin2 ;
		    col := col2 ;
		    pos := pos2 ;
		    (pos1',pos2')
		end

	    fun yyinput' n =
		let
		    val  s    = yyinput n
		    val (l,c) = count(!pos, String.size(!buf), !lin, !col) ;
		in
		    lin := l ;
		    col := c ;
		    buf := s ;
		    pos := 0 ;
		    s
		end

	    val lexer = Lexer.makeLexer yyinput'
	in
	    fn () =>
		let
		    val LrParser.Token.TOKEN(term, (svalue,pos1,pos2)) = lexer()
		    val (pos1', pos2') = transform(pos1, pos2)
		in
		    LrParser.Token.TOKEN(term, (svalue, pos1', pos2'))
		end
		handle LexerError.Error(position, e) =>
		    Error.error(transform position, LexerError.toString e)
	end

  end
