(*
structure M =
  struct
    datatype t = T
  end

signature S =
  sig
    structure X : sig type t end
    val x : M.t
  end

structure N :> S =
  struct
    structure X = M
    val x = X.T
  end

val M.T = N.x
*)


signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
	datatype state = STATE
	datatype term = T
	datatype nonterm = NT
	datatype action = ERROR
	type table
	val mkLrTable : ('a,'b) pairlist
    end

signature TOKEN =
    sig
	structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of ('a * 'b * 'b)
	val sameToken : ('a,'b) token
    end

signature LR_PARSER =
    sig
	structure LrTable : LR_TABLE
	structure Token : TOKEN where LrTable = LrTable
    end

signature LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
	   end
   end

signature PARSER_DATA =
   sig
	type pos
	type svalue
 	type arg
 
	type result

	structure LrTable : LR_TABLE
	structure Token : TOKEN where LrTable = LrTable
    end

functor Join(structure Lex : LEXER
	     structure ParserData: PARSER_DATA
where type svalue = Lex.UserDeclarations.svalue
where type pos = Lex.UserDeclarations.pos
where type ('a,'b) Token.token = ('a,'b) Lex.UserDeclarations.token
	     structure LrParser : LR_PARSER
where type ('a,'b) LrTable.pairlist = ('a,'b) ParserData.LrTable.pairlist
where type LrTable.state = ParserData.LrTable.state
where type LrTable.term = ParserData.LrTable.term
where type LrTable.nonterm = ParserData.LrTable.nonterm
where type LrTable.action = ParserData.LrTable.action
where type LrTable.table = ParserData.LrTable.table

where type ('a,'b) Token.LrTable.pairlist = ('a,'b) ParserData.Token.LrTable.pairlist
where type Token.LrTable.state = ParserData.Token.LrTable.state
where type Token.LrTable.term = ParserData.Token.LrTable.term
where type Token.LrTable.nonterm = ParserData.Token.LrTable.nonterm
where type Token.LrTable.action = ParserData.Token.LrTable.action
where type Token.LrTable.table = ParserData.Token.LrTable.table
where type ('a,'b) Token.token = ('a,'b) ParserData.Token.token
)
  = struct end

