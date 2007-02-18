(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2006
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(*
 * Standard ML lexical analysis
 *
 * Definition, sections 2.1-2.5, 3.1
 *
 * Extensions and modifications:
 *   - line comments
 *   - more liberal constant prefixes (allow 0xw)
 *   - binary int and word constants (0b010, 0wb010)
 *   - allow underscores in numbers
 *   - longids have been moved to the context-free grammar,
 *     so the LONGID token is substituted by a DOT token
 *   - #[ keyword for vector expressions
 *   - FINALLY keyword
 *   - ASSERT keyword(s)
 *   - EXTTYPE and CONSTRUCTOR keywords for extensible datatypes
 *   - NON keyword added for negated patterns
 *   - WITHVAL and WITHFUN keywords for bindings inside pattern
 *   - LAZY and SPAWN keywords for futures
 *   - FCT keyword for functor expressions and signatures
 *   - ANY keyword for top signature
 *   - PACK and UNPACK keyword for first class structures
 *   - IMPORT and FROM keywords added
 *   - PRIMITIVE, OVERLOAD, EQEQTYPE, REFTYPE, and PERVASIVE keywords added
 *
 * Notes:
 *   Since all lexical classes must be disjoint:
 *   - There is no single class ID, use ALPHA|SYMBOL|STAR|EQUALS.
 *   - There is no class LAB, use ALPHA|SYMBOL|NUMERIC|DIGIT|STAR.
 *   - ID does not contain `=' and `*', those are EQUALS and STAR.
 *   - INT does not contain positive decimal integers without leading 0,
 *     and single DIGIT integers, those are in NUMERIC, DIGIT, and ZERO.
 *   - NUMERIC does not contain single digit numbers, those are in DIGIT.
 *   - DIGIT does not contain 0, that is ZERO.
 *
 *   The parser uses a global variable to recognise nested comments, so it is
 *   not reentrant.
 *)


    structure E = ParsingError

    val error = LexerError.error


  (* Types to match structure LEXER.UserDeclaration *)

    type ('a,'b) token = ('a,'b) Tokens.token
    type pos           = int
    type svalue        = Tokens.svalue
    type lexresult     = (svalue, pos) token


  (* Source positions *)

    fun toLRPos(yypos, yytext) =
	let
	    val yypos = yypos - 2	(* bug in ML-Lex... *)
	in
	    (yypos, yypos + String.size yytext)
	end


  (* Handling nested comments *)

    val nesting = ref [] : int list ref

    fun nest yypos = nesting := yypos-2 :: !nesting
    fun nested ()  = not (List.null(!nesting))
    fun unnest ()  = ( nesting := List.tl(!nesting) ; List.null(!nesting) )

    fun eof() =
	case !nesting
	  of []    => raise LexerError.EOF(fn i => Tokens.EOF i)
	   | i0::_ =>
		( nesting := [];
		  raise LexerError.EOF(fn(i1,i2) =>
					  error((i0,i2), E.UnclosedComment)))



  (* Some helpers to create tokens *)

    open Tokens


    fun token(TOKEN, yypos, yytext) =
        TOKEN(toLRPos(yypos, yytext))

    fun tokenOf(TOKEN, toVal, yypos, yytext) =
	let
	    val i as (l,r) = toLRPos(yypos, yytext)
	in
	    TOKEN(toVal(yytext,i), l, r)
	end

    fun ASSERT_ d_opt (l,r) = ASSERT(d_opt, l, r)

    fun error'(yypos, yytext, e) = error(toLRPos(yypos, yytext), e)



  (* Convert identifiers and constants *)

    datatype radix = datatype StringCvt.radix

    fun toId(s,i) = s

    fun toDigit(s,i) = Char.ord(String.sub(s,0)) - Char.ord #"0"

    fun toInt(s,i) =
	case String.sub(s,0)
	  of #"~" => toInt'("~", String.extract(s,1,NONE), i)
	   |   _  => toInt'("", s, i)
	(* Be careful to handle minInt correctly - let scan do the trick... *)

    and toInt'(s0,s,i) =
	case String.sub(s,0)
	  of #"0" => (if String.size s = 1 then LargeInt.fromInt 0 else
		      case String.sub(s,1)
			of #"b" => toInt''(s0^String.extract(s,2,NONE), BIN, i)
			 | #"x" => toInt''(s0^String.extract(s,2,NONE), HEX, i)
			 |   _  => toInt''(s0^s, DEC, i)
		     )
	   |   _  => toInt''(s0^s, DEC, i)

    and toInt''(s,b,i) = Option.valOf(StringCvt.scanString (LargeInt.scan b) s)
			 handle Overflow => error(i, E.IntTooLarge)

    fun toWord(s,i) =
	case (String.sub(s,1), String.sub(s,2))
	  of ( (#"b",_) | (_,#"b") ) => toWord'(String.extract(s,3,NONE), BIN,i)
	   | ( (#"x",_) | (_,#"x") ) => toWord'(String.extract(s,3,NONE), HEX,i)
	   |            _            => toWord'(String.extract(s,2,NONE), DEC,i)

    and toWord'(s,b,i) = Option.valOf(StringCvt.scanString (LargeWord.scan b) s)
			 handle Overflow => error(i, E.WordTooLarge)

    fun toReal(s,i)    = Option.valOf(StringCvt.scanString LargeReal.scan s)
			 handle Overflow => error(i, E.RealTooLarge)


    fun toString(s,i) =
	let
            fun base(s,b,m) =
		WideChar.chr(Option.valOf(StringCvt.scanString (Int.scan b) s))
		handle (Chr | Overflow) =>
			 error(i, E.EscapeCharTooLarge m)

	    fun dec s     = base(s, DEC, false)
	    fun unicode s = base(s, HEX, true)

	    fun convert(k,cs) =
		case String.sub(s,k)
		  of #"\"" => WideString.implode(List.rev cs)
		   | #"\\" => escape(k+1,cs)
		   |   c   => convert(k+1, Char.toWide(c)::cs)

	    and escape(k,cs) =
		case String.sub(s,k)
		  of #"a"  => convert(k+1, #"\a"::cs)
		   | #"b"  => convert(k+1, #"\b"::cs)
		   | #"t"  => convert(k+1, #"\t"::cs)
		   | #"n"  => convert(k+1, #"\n"::cs)
		   | #"v"  => convert(k+1, #"\v"::cs)
		   | #"f"  => convert(k+1, #"\f"::cs)
		   | #"r"  => convert(k+1, #"\r"::cs)
		   | #"\"" => convert(k+1, #"\""::cs)
		   | #"\\" => convert(k+1, #"\\"::cs)
		   | #"^"  => let val c = String.sub(s,k+1) in
				  convert(k+2, WideChar.chr(Char.ord c -
							    Char.ord #"@")::cs)
			      end

		   | #"u"  => let val s' = String.extract(s, k+1, SOME 4) in
				  convert(k+5, unicode s' :: cs)
			      end

		   |   c   => if Char.isDigit c then
				  let val s' = String.extract(s, k, SOME 3) in
				      convert(k+3, dec s' :: cs)
				  end
			      else if Char.isSpace c then
				   gap(k+1, cs)
			      else raise Crash.Crash "Lexer.toString: \
						     \invalid escape sequence"

	    and gap(k, cs) =
		    if String.sub(s,k) = #"\\" then
			convert(k+1, cs)
		    else
			gap(k+1, cs)

	in
	    convert(1,[])
	end


    fun toChar(s, i) =
	let
	    val s'  = String.substring(s, 1, String.size s - 1)
	    val ss' = toString(s', i)
	in
	    if WideString.size ss' = 1 then
		WideString.sub(ss', 0)
	    else
		error(i, E.CharLengthInvalid ss')
	end

%%


%header	( functor MkLexer(structure Tokens:     Parser_TOKENS
			  structure LexerError: LEXER_ERROR
			  where type token = (Tokens.svalue,int) Tokens.token
			  where type error = ParsingError.error));

%s COMMENT LCOMMENT;

%full


  formatting = [\ \t\n\011\012\013]+;
  letter     = [A-Za-z];
  symbol     = [-!%&$#+/:<=>?@\\~`|*^];
  digit      = [0-9];
  bindigit   = [0-1];
  hexdigit   = [0-9a-fA-F];
  xdigit     = {digit} | "_";
  xbindigit  = {bindigit} | "_";
  xhexdigit  = {hexdigit} | "_";
  digits     = {xdigit}*{digit}{xdigit}*;
  bindigits  = {xbindigit}*{bindigit}{xbindigit}*;
  hexdigits  = {xhexdigit}*{hexdigit}{xhexdigit}*;

  posdecint  = {digit}{xdigit}*;
  posbinint  = "0b"{bindigits};
  poshexint  = "0x"{hexdigits};
  negdecint  = "~"{posdecint};
  negbinint  = "~"{posbinint};
  neghexint  = "~"{poshexint};
  decint     = {posdecint} | {negdecint};
  binint     = {posbinint} | {negbinint};
  hexint     = {poshexint} | {neghexint};
  decword    = "0w"{digits};
  binword    = "0"("wb"|"bw"){bindigits};
  hexword    = "0"("wx"|"xw"){hexdigits};

  int        = {decint} | {binint} | {hexint};
  word       = {decword} | {binword} | {hexword};
  exp        = "e" | "E";
  real       = ({decint}"."{digits} ({exp}{decint})?) | ({decint}{exp}{decint});

  numericlab = [1-9]{digit}*;
  alphanumid = {letter}({letter} | {digit} | [_'])*;
  symbolicid = {symbol}+;
  id         = {alphanumid} | {symbolicid};
  tyvar      = "'"({letter} | {digit} | [_'])*;

  printable  = [^\000-\032"\127\\];
  escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r" |
	       ("\\^"[@-_])  | ("\\"{digit}{3}) | ("\\u"{hexdigit}{4}) |
	       "\\\"" | "\\\\";
  gap        = ("\\"{formatting}"\\");
  stringchar = {printable} | " " | {escape};
  string     = "\""({stringchar} | {gap})*"\"";
  char       = "#"{string};



%%


  <INITIAL>{formatting}	=> ( continue() );

  <INITIAL>"#"		=> ( token(HASH,      yypos, yytext) );
  <INITIAL>"#["		=> ( token(HASHBRACK, yypos, yytext) );
  <INITIAL>"("		=> ( token(LPAR,      yypos, yytext) );
  <INITIAL>")"		=> ( token(RPAR,      yypos, yytext) );
  <INITIAL>"*"		=> ( token(STAR,      yypos, yytext) );
  <INITIAL>","		=> ( token(COMMA,     yypos, yytext) );
  <INITIAL>"->"		=> ( token(ARROW,     yypos, yytext) );
  <INITIAL>"."		=> ( token(DOT,       yypos, yytext) );
  <INITIAL>"..."	=> ( token(DOTS,      yypos, yytext) );
  <INITIAL>":"		=> ( token(COLON,     yypos, yytext) );
  <INITIAL>":>"		=> ( token(COLONGREATER, yypos, yytext) );
  <INITIAL>";"		=> ( token(SEMICOLON, yypos, yytext) );
  <INITIAL>"="		=> ( token(EQUALS,    yypos, yytext) );
  <INITIAL>"=>"		=> ( token(DARROW,    yypos, yytext) );
  <INITIAL>"["		=> ( token(LBRACK,    yypos, yytext) );
  <INITIAL>"]"		=> ( token(RBRACK,    yypos, yytext) );
  <INITIAL>"_"		=> ( token(UNDERBAR,  yypos, yytext) );
  <INITIAL>"{"		=> ( token(LBRACE,    yypos, yytext) );
  <INITIAL>"|"		=> ( token(BAR,       yypos, yytext) );
  <INITIAL>"}"		=> ( token(RBRACE,    yypos, yytext) );

  <INITIAL>"__eqeqtype"	=> ( token(EQEQTYPE,  yypos, yytext) );
  <INITIAL>"__overload"	=> ( token(OVERLOAD,  yypos, yytext) );
  <INITIAL>"__pervasive"=> ( token(PERVASIVE, yypos, yytext) );
  <INITIAL>"__primitive"=> ( token(PRIMITIVE, yypos, yytext) );
  <INITIAL>"__reftype"	=> ( token(REFTYPE,   yypos, yytext) );
  <INITIAL>"_file_"	=> ( token(FILE,      yypos, yytext) );
  <INITIAL>"_line_"	=> ( token(LINE,      yypos, yytext) );
  <INITIAL>"abstype"	=> ( token(ABSTYPE,   yypos, yytext) );
  <INITIAL>"and"	=> ( token(AND,       yypos, yytext) );
  <INITIAL>"andalso"	=> ( token(ANDALSO,   yypos, yytext) );
  <INITIAL>"any"	=> ( token(ANY,       yypos, yytext) );
  <INITIAL>"as"		=> ( token(AS,        yypos, yytext) );
  <INITIAL>"assert"	=> ( token(ASSERT_ NONE,    yypos, yytext) );
  <INITIAL>"assert0"	=> ( token(ASSERT_(SOME 0), yypos, yytext) );
  <INITIAL>"assert1"	=> ( token(ASSERT_(SOME 1), yypos, yytext) );
  <INITIAL>"assert2"	=> ( token(ASSERT_(SOME 2), yypos, yytext) );
  <INITIAL>"assert3"	=> ( token(ASSERT_(SOME 3), yypos, yytext) );
  <INITIAL>"assert4"	=> ( token(ASSERT_(SOME 4), yypos, yytext) );
  <INITIAL>"assert5"	=> ( token(ASSERT_(SOME 5), yypos, yytext) );
  <INITIAL>"assert6"	=> ( token(ASSERT_(SOME 6), yypos, yytext) );
  <INITIAL>"assert7"	=> ( token(ASSERT_(SOME 7), yypos, yytext) );
  <INITIAL>"assert8"	=> ( token(ASSERT_(SOME 8), yypos, yytext) );
  <INITIAL>"assert9"	=> ( token(ASSERT_(SOME 9), yypos, yytext) );
  <INITIAL>"case"	=> ( token(CASE,      yypos, yytext) );
  <INITIAL>"constructor"=> ( token(CONSTRUCTOR,yypos, yytext) );
  <INITIAL>"datatype"	=> ( token(DATATYPE,  yypos, yytext) );
  <INITIAL>"do"		=> ( token(DO,        yypos, yytext) );
  <INITIAL>"else"	=> ( token(ELSE,      yypos, yytext) );
  <INITIAL>"end"	=> ( token(END,       yypos, yytext) );
  <INITIAL>"eqtype"	=> ( token(EQTYPE,    yypos, yytext) );
  <INITIAL>"exception"	=> ( token(EXCEPTION, yypos, yytext) );
  <INITIAL>"exttype"	=> ( token(EXTTYPE,   yypos, yytext) );
  <INITIAL>"fct"       	=> ( token(FCT,       yypos, yytext) );
  <INITIAL>"finally"	=> ( token(FINALLY,   yypos, yytext) );
  <INITIAL>"fn"		=> ( token(FN,        yypos, yytext) );
  <INITIAL>"from"	=> ( token(FROM,      yypos, yytext) );
  <INITIAL>"fun"	=> ( token(FUN,       yypos, yytext) );
  <INITIAL>"functor"	=> ( token(FUNCTOR,   yypos, yytext) );
  <INITIAL>"handle"	=> ( token(HANDLE,    yypos, yytext) );
  <INITIAL>"if"		=> ( token(IF,        yypos, yytext) );
  <INITIAL>"import"	=> ( token(IMPORT,    yypos, yytext) );
  <INITIAL>"in"		=> ( token(IN,        yypos, yytext) );
  <INITIAL>"include"	=> ( token(INCLUDE,   yypos, yytext) );
  <INITIAL>"infix"	=> ( token(INFIX,     yypos, yytext) );
  <INITIAL>"infixr"	=> ( token(INFIXR,    yypos, yytext) );
  <INITIAL>"lazy"	=> ( token(LAZY,      yypos, yytext) );
  <INITIAL>"let"	=> ( token(LET,       yypos, yytext) );
  <INITIAL>"local"	=> ( token(LOCAL,     yypos, yytext) );
  <INITIAL>"non"	=> ( token(NON,       yypos, yytext) );
  <INITIAL>"nonfix"	=> ( token(NONFIX,    yypos, yytext) );
  <INITIAL>"of"		=> ( token(OF,        yypos, yytext) );
  <INITIAL>"op"		=> ( token(OP,        yypos, yytext) );
  <INITIAL>"open"	=> ( token(OPEN,      yypos, yytext) );
  <INITIAL>"orelse"	=> ( token(ORELSE,    yypos, yytext) );
  <INITIAL>"pack"	=> ( token(PACK,      yypos, yytext) );
  <INITIAL>"raise"	=> ( token(RAISE,     yypos, yytext) );
  <INITIAL>"rec"	=> ( token(REC,       yypos, yytext) );
  <INITIAL>"sharing"	=> ( token(SHARING,   yypos, yytext) );
  <INITIAL>"sig"	=> ( token(SIG,       yypos, yytext) );
  <INITIAL>"signature"	=> ( token(SIGNATURE, yypos, yytext) );
  <INITIAL>"spawn"	=> ( token(SPAWN,     yypos, yytext) );
  <INITIAL>"struct"	=> ( token(STRUCT,    yypos, yytext) );
  <INITIAL>"structure"	=> ( token(STRUCTURE, yypos, yytext) );
  <INITIAL>"then"	=> ( token(THEN,      yypos, yytext) );
  <INITIAL>"type"	=> ( token(TYPE,      yypos, yytext) );
  <INITIAL>"unpack"	=> ( token(UNPACK,    yypos, yytext) );
  <INITIAL>"val"	=> ( token(VAL,       yypos, yytext) );
  <INITIAL>"where"	=> ( token(WHERE,     yypos, yytext) );
  <INITIAL>"while"	=> ( token(WHILE,     yypos, yytext) );
  <INITIAL>"with"	=> ( token(WITH,      yypos, yytext) );
  <INITIAL>"withfun"	=> ( token(WITHFUN,   yypos, yytext) );
  <INITIAL>"withtype"	=> ( token(WITHTYPE,  yypos, yytext) );
  <INITIAL>"withval"	=> ( token(WITHVAL,   yypos, yytext) );

  <INITIAL>"0"		=> ( token  (ZERO,              yypos, yytext) );
  <INITIAL>[1-9]	=> ( tokenOf(DIGIT,   toDigit,  yypos, yytext) );
  <INITIAL>{numericlab}	=> ( tokenOf(NUMERIC, toInt,    yypos, yytext) );
  <INITIAL>{int}	=> ( tokenOf(INT,     toInt,    yypos, yytext) );
  <INITIAL>{word}	=> ( tokenOf(WORD,    toWord,   yypos, yytext) );
  <INITIAL>{real}	=> ( tokenOf(REAL,    toReal,   yypos, yytext) );
  <INITIAL>{string}	=> ( tokenOf(STRING,  toString, yypos, yytext) );
  <INITIAL>{char}	=> ( tokenOf(CHAR,    toChar,   yypos, yytext) );

  <INITIAL>{tyvar}	=> ( tokenOf(TYVAR,   toId,     yypos, yytext) );
  <INITIAL>{id}		=> ( tokenOf(ALPHA,   toId,     yypos, yytext) );


  <INITIAL>"(*)"	=> ( YYBEGIN LCOMMENT ; continue() );
  <INITIAL>"(*"		=> ( nest yypos ; YYBEGIN COMMENT ; continue() );
  <COMMENT>"(*)"	=> ( YYBEGIN LCOMMENT ; continue() );
  <COMMENT>"(*"		=> ( nest yypos ; continue() );
  <COMMENT>"*)"		=> ( if unnest() then YYBEGIN INITIAL else () ;
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );

  <LCOMMENT>.		=> ( continue() );
  <LCOMMENT>"\n"	=> ( YYBEGIN (if nested() then COMMENT else INITIAL) ;
			     continue() );

  <INITIAL>"\""		=> ( error'(yypos, yytext, E.InvalidString) );
  <INITIAL>.		=> ( error'(yypos, yytext,
				    E.InvalidChar(String.sub(yytext,0))) );
