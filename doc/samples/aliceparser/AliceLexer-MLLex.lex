(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *   
 *   With some minor adaptions by Benedikt Grundmann <bgrund@ps.uni-sb.de>
 *   to be able to use it with aliceyacc.  If there are any errors they
 *   are most likely to be mine.
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2004
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

    structure P = AliceParser
    
  (* Types to match structure LEXER.UserDeclaration *)

    type pos           = int * int
    type token         = P.token option * pos * pos
    type lexresult     = token

  (* Source positions 
     BG: This is more or less total nonsense what I'm
     doing here but I did not wanted to adapt the main
     layer :-)
   *)

    fun toLRPos(yypos, yytext) =
	let
	    val yypos = yypos - 2	(* bug in ML-Lex... *)
	    val p     = (1, yypos) 
            val p2    = (1, yypos + String.size yytext)
        in
            (p, p)
	end

    fun error (yypos, yytext) =
        let 
            val p = toLRPos (yypos, yytext)
        in
            raise P.Error p 
        end

  (* Handling nested comments *)

    val nesting = ref [] : int list ref

    fun nest yypos = nesting := yypos :: !nesting
    fun unnest ()  = ( nesting := List.tl(!nesting) ; List.null(!nesting) )

    fun eof () =
	case !nesting
	  of []    => ( NONE, (0, 0), (0, 0) )
	   | i0::_ => raise P.Error ( (1, i0), (0, 0) )


  (* Some helpers to create tokens *)

    fun token(token, yypos, yytext) =
        let val (l, r) = toLRPos (yypos, yytext)
        in
            (SOME token, l, r)
        end

    fun tokenOf(token, toVal, yypos, yytext) =
	let
            val (l, r)     = toLRPos (yypos, yytext)
	in
            (SOME (token (toVal yytext)), l, r)
	end

  (* Convert identifiers and constants *)

    datatype radix = datatype StringCvt.radix

    fun toId s = s

    fun toDigit s = Char.ord(String.sub(s,0)) - Char.ord #"0"

    fun toInt s =
	case String.sub(s,0)
	  of #"~" => toInt'("~", String.extract(s,1,NONE))
	   |   _  => toInt'("", s)
	(* Be careful to handle minInt correctly - let scan do the trick... *)

    and toInt'(s0,s) =
	case String.sub(s,0)
	  of #"0" => (if String.size s = 1 then LargeInt.fromInt 0 else
		      case String.sub(s,1)
			of #"b" => toInt''(s0^String.extract(s,2,NONE), BIN)
			 | #"x" => toInt''(s0^String.extract(s,2,NONE), HEX)
			 |   _  => toInt''(s0^s, DEC)
		     )
	   |   _  => toInt''(s0^s, DEC)

    and toInt''(s,b) = Option.valOf(StringCvt.scanString (LargeInt.scan b) s)
			 handle Overflow => raise Fail "overflow in integer literal" 

    fun toWord(s) =
	case (String.sub(s,1), String.sub(s,2))
	  of ( (#"b",_) | (_,#"b") ) => toWord'(String.extract(s,3,NONE), BIN)
	   | ( (#"x",_) | (_,#"x") ) => toWord'(String.extract(s,3,NONE), HEX)
	   |            _            => toWord'(String.extract(s,2,NONE), DEC)

    and toWord'(s,b) = Option.valOf(StringCvt.scanString (LargeWord.scan b) s)
			 handle Overflow => raise Fail "overflow in word literal" 

    fun toReal(s)    = Option.valOf(StringCvt.scanString LargeReal.scan s)
			 handle Overflow => raise Fail "overflow in real literal"


    fun toString(s) =
	let
            fun base(s,b,m) =
		WideChar.chr(Option.valOf(StringCvt.scanString (Int.scan b) s))
		handle (Chr | Overflow) =>
                         raise Fail "error in string"

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
			      else raise Fail "invalid escape sequence in string"

	    and gap(k, cs) =
		    if String.sub(s,k) = #"\\" then
			convert(k+1, cs)
		    else
			gap(k+1, cs)

	in
	    convert(1,[])
	end


    fun toChar s =
	let
	    val s'  = String.substring(s, 1, String.size s - 1)
	    val ss' = toString(s')
	in
	    if WideString.size ss' = 1 then
		WideString.sub(ss', 0)
	    else
                raise Fail "invalid character"
	end

%%


%header	( structure AliceLexerMLLex );

%s COMMENT;

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

  <INITIAL>"#"		=> ( token(P.HASH,      yypos, yytext) );
  <INITIAL>"#["		=> ( token(P.HASHBRACK, yypos, yytext) );
  <INITIAL>"("		=> ( token(P.LPAR,      yypos, yytext) );
  <INITIAL>")"		=> ( token(P.RPAR,      yypos, yytext) );
  <INITIAL>"*"		=> ( token(P.STAR,      yypos, yytext) );
  <INITIAL>","		=> ( token(P.COMMA,     yypos, yytext) );
  <INITIAL>"->"		=> ( token(P.ARROW,     yypos, yytext) );
  <INITIAL>"."		=> ( token(P.DOT,       yypos, yytext) );
  <INITIAL>"..."	=> ( token(P.DOTS,      yypos, yytext) );
  <INITIAL>":"		=> ( token(P.COLON,     yypos, yytext) );
  <INITIAL>":>"		=> ( token(P.COLONGREATER, yypos, yytext) );
  <INITIAL>";"		=> ( token(P.SEMICOLON, yypos, yytext) );
  <INITIAL>"="		=> ( token(P.EQUALS,    yypos, yytext) );
  <INITIAL>"=>"		=> ( token(P.DARROW,    yypos, yytext) );
  <INITIAL>"["		=> ( token(P.LBRACK,    yypos, yytext) );
  <INITIAL>"]"		=> ( token(P.RBRACK,    yypos, yytext) );
  <INITIAL>"_"		=> ( token(P.UNDERBAR,  yypos, yytext) );
  <INITIAL>"{"		=> ( token(P.LBRACE,    yypos, yytext) );
  <INITIAL>"|"		=> ( token(P.BAR,       yypos, yytext) );
  <INITIAL>"}"		=> ( token(P.RBRACE,    yypos, yytext) );

  <INITIAL>"__eqeqtype"	=> ( token(P.EQEQTYPE,  yypos, yytext) );
  <INITIAL>"__overload"	=> ( token(P.OVERLOAD,  yypos, yytext) );
  <INITIAL>"__pervasive"=> ( token(P.PERVASIVE, yypos, yytext) );
  <INITIAL>"__primitive"=> ( token(P.PRIMITIVE, yypos, yytext) );
  <INITIAL>"__reftype"	=> ( token(P.REFTYPE,   yypos, yytext) );
  <INITIAL>"_file_"	=> ( token(P.FILE,      yypos, yytext) );
  <INITIAL>"_line_"	=> ( token(P.LINE,      yypos, yytext) );
  <INITIAL>"abstype"	=> ( token(P.ABSTYPE,   yypos, yytext) );
  <INITIAL>"and"	=> ( token(P.AND,       yypos, yytext) );
  <INITIAL>"andalso"	=> ( token(P.ANDALSO,   yypos, yytext) );
  <INITIAL>"any"	=> ( token(P.ANY,       yypos, yytext) );
  <INITIAL>"as"		=> ( token(P.AS,        yypos, yytext) );
  <INITIAL>"assert"	=> ( token(P.ASSERT NONE,    yypos, yytext) );
  <INITIAL>"assert0"	=> ( token(P.ASSERT(SOME 0), yypos, yytext) );
  <INITIAL>"assert1"	=> ( token(P.ASSERT(SOME 1), yypos, yytext) );
  <INITIAL>"assert2"	=> ( token(P.ASSERT(SOME 2), yypos, yytext) );
  <INITIAL>"assert3"	=> ( token(P.ASSERT(SOME 3), yypos, yytext) );
  <INITIAL>"assert4"	=> ( token(P.ASSERT(SOME 4), yypos, yytext) );
  <INITIAL>"assert5"	=> ( token(P.ASSERT(SOME 5), yypos, yytext) );
  <INITIAL>"assert6"	=> ( token(P.ASSERT(SOME 6), yypos, yytext) );
  <INITIAL>"assert7"	=> ( token(P.ASSERT(SOME 7), yypos, yytext) );
  <INITIAL>"assert8"	=> ( token(P.ASSERT(SOME 8), yypos, yytext) );
  <INITIAL>"assert9"	=> ( token(P.ASSERT(SOME 9), yypos, yytext) );
  <INITIAL>"case"	=> ( token(P.CASE,      yypos, yytext) );
  <INITIAL>"constructor"=> ( token(P.CONSTRUCTOR,yypos, yytext) );
  <INITIAL>"datatype"	=> ( token(P.DATATYPE,  yypos, yytext) );
  <INITIAL>"do"		=> ( token(P.DO,        yypos, yytext) );
  <INITIAL>"else"	=> ( token(P.ELSE,      yypos, yytext) );
  <INITIAL>"end"	=> ( token(P.END,       yypos, yytext) );
  <INITIAL>"eqtype"	=> ( token(P.EQTYPE,    yypos, yytext) );
  <INITIAL>"exception"	=> ( token(P.EXCEPTION, yypos, yytext) );
  <INITIAL>"exttype"	=> ( token(P.EXTTYPE,   yypos, yytext) );
  <INITIAL>"fct"       	=> ( token(P.FCT,       yypos, yytext) );
  <INITIAL>"finally"	=> ( token(P.FINALLY,   yypos, yytext) );
  <INITIAL>"fn"		=> ( token(P.FN,        yypos, yytext) );
  <INITIAL>"from"	=> ( token(P.FROM,      yypos, yytext) );
  <INITIAL>"fun"	=> ( token(P.FUN,       yypos, yytext) );
  <INITIAL>"functor"	=> ( token(P.FUNCTOR,   yypos, yytext) );
  <INITIAL>"handle"	=> ( token(P.HANDLE,    yypos, yytext) );
  <INITIAL>"if"		=> ( token(P.IF,        yypos, yytext) );
  <INITIAL>"import"	=> ( token(P.IMPORT,    yypos, yytext) );
  <INITIAL>"in"		=> ( token(P.IN,        yypos, yytext) );
  <INITIAL>"include"	=> ( token(P.INCLUDE,   yypos, yytext) );
  <INITIAL>"infix"	=> ( token(P.INFIX,     yypos, yytext) );
  <INITIAL>"infixr"	=> ( token(P.INFIXR,    yypos, yytext) );
  <INITIAL>"lazy"	=> ( token(P.LAZY,      yypos, yytext) );
  <INITIAL>"let"	=> ( token(P.LET,       yypos, yytext) );
  <INITIAL>"local"	=> ( token(P.LOCAL,     yypos, yytext) );
  <INITIAL>"non"	=> ( token(P.NON,       yypos, yytext) );
  <INITIAL>"nonfix"	=> ( token(P.NONFIX,    yypos, yytext) );
  <INITIAL>"of"		=> ( token(P.OF,        yypos, yytext) );
  <INITIAL>"op"		=> ( token(P.OP,        yypos, yytext) );
  <INITIAL>"open"	=> ( token(P.OPEN,      yypos, yytext) );
  <INITIAL>"orelse"	=> ( token(P.ORELSE,    yypos, yytext) );
  <INITIAL>"pack"	=> ( token(P.PACK,      yypos, yytext) );
  <INITIAL>"raise"	=> ( token(P.RAISE,     yypos, yytext) );
  <INITIAL>"rec"	=> ( token(P.REC,       yypos, yytext) );
  <INITIAL>"sharing"	=> ( token(P.SHARING,   yypos, yytext) );
  <INITIAL>"sig"	=> ( token(P.SIG,       yypos, yytext) );
  <INITIAL>"signature"	=> ( token(P.SIGNATURE, yypos, yytext) );
  <INITIAL>"spawn"	=> ( token(P.SPAWN,     yypos, yytext) );
  <INITIAL>"struct"	=> ( token(P.STRUCT,    yypos, yytext) );
  <INITIAL>"structure"	=> ( token(P.STRUCTURE, yypos, yytext) );
  <INITIAL>"then"	=> ( token(P.THEN,      yypos, yytext) );
  <INITIAL>"type"	=> ( token(P.TYPE,      yypos, yytext) );
  <INITIAL>"unpack"	=> ( token(P.UNPACK,    yypos, yytext) );
  <INITIAL>"val"	=> ( token(P.VAL,       yypos, yytext) );
  <INITIAL>"where"	=> ( token(P.WHERE,     yypos, yytext) );
  <INITIAL>"while"	=> ( token(P.WHILE,     yypos, yytext) );
  <INITIAL>"with"	=> ( token(P.WITH,      yypos, yytext) );
  <INITIAL>"withfun"	=> ( token(P.WITHFUN,   yypos, yytext) );
  <INITIAL>"withtype"	=> ( token(P.WITHTYPE,  yypos, yytext) );
  <INITIAL>"withval"	=> ( token(P.WITHVAL,   yypos, yytext) );

  <INITIAL>"0"		=> ( token  (P.ZERO,            yypos, yytext) );
  <INITIAL>[1-9]	=> ( tokenOf(P.DIGIT,   toDigit,  yypos, yytext) );
  <INITIAL>{numericlab}	=> ( tokenOf(P.NUMERIC, toInt,    yypos, yytext) );
  <INITIAL>{int}	=> ( tokenOf(P.INT,     toInt,    yypos, yytext) );
  <INITIAL>{word}	=> ( tokenOf(P.WORD,    toWord,   yypos, yytext) );
  <INITIAL>{real}	=> ( tokenOf(P.REAL,    toReal,   yypos, yytext) );
  <INITIAL>{string}	=> ( tokenOf(P.STRING,  toString, yypos, yytext) );
  <INITIAL>{char}	=> ( tokenOf(P.CHAR,    toChar,   yypos, yytext) );

  <INITIAL>{tyvar}	=> ( tokenOf(P.TYVAR,   toId,     yypos, yytext) );
  <INITIAL>{id}		=> ( tokenOf(P.ALPHA,   toId,     yypos, yytext) );


  <INITIAL>"(*"		=> ( nest(yypos-2) ; YYBEGIN COMMENT ; continue() );
  <COMMENT>"(*"		=> ( nest(yypos-2) ; continue() );
  <COMMENT>"*)"		=> ( if unnest() then YYBEGIN INITIAL else () ;
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );


  <INITIAL>"\""		=> ( error(yypos, yytext) );
  <INITIAL>.		=> ( error(yypos, yytext) );
