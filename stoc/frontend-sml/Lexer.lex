(*
 * Standard ML lexical analysis
 *
 * Definition, sections 2.1-2.5, 3.1
 *
 * Extensions and modifications:
 *   - more liberal constant prefixes (allow 0xw)
 *   - binary int and word constants (0b010, 0wb010)
 *   - longids have been moved to the context-free grammar,
 *     so the LONGID token is substituted by a DOT token
 *   - #[ keyword for vector expressions
 *   - CON keyword for constructor declarations
 *   - WHEN keyword for guarded patterns
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


    open Tokens


    (* Types to match structure LEXER.UserDeclaration *)

    type ('a,'b) token = ('a,'b) Tokens.token
    type pos           = Source.pos
    type svalue        = Tokens.svalue
    type lexresult     = (svalue, pos) token



    (* Handling nested comments *)

    val nesting = ref 0		(* non-reentrant side-effect way :-P *)


    fun eof() =
	if !nesting = 0 then
	    Tokens.EOF(0, 0)
	else
	    Error.error((0,0), "unclosed comment")



    (* Some helpers to create tokens *)

    open Tokens


    fun toLRPos(yypos, yytext) =
	let
	    val yypos = yypos - 2	(* bug in ML-Lex... *)
	in
	    (yypos, yypos + String.size yytext)
	end

    fun token(TOKEN, yypos, yytext) =
        TOKEN(toLRPos(yypos, yytext))

    fun tokenOf(TOKEN, toVal, yypos, yytext) =
	let
	    val i as (l,r) = toLRPos(yypos, yytext)
	in
	    TOKEN(toVal(yytext,i), l, r)
	end

    fun error(yypos, yytext, s) =
	    Error.error(toLRPos(yypos, yytext), s)

    fun invalid(yypos, yytext) =
	let
	    val s = "invalid character `" ^ String.toCString yytext ^ "'"
	in
	    error(yypos, yytext, s)
	end



    (* Convert identifiers and constants *)

    local open StringCvt in

    fun toId(s,i) = s

    fun toInt(s,i) =
	case String.sub(s,0)
	  of #"~" => ~(toInt(String.extract(s,1,NONE), i))
	   | #"0" => (if String.size s = 1 then 0 else
		      case String.sub(s,1)
			of #"b" => toInt'(String.extract(s,2,NONE), BIN, i)
			 | #"x" => toInt'(String.extract(s,2,NONE), HEX, i)
			 |   _  => toInt'(s, HEX, i)
		     )
	   |   _  => toInt'(s, HEX, i)

    and toInt'(s,b,i) = valOf(scanString (Int.scan b) s)
			handle Overflow =>
			       Error.error(i, "integer constant too big")

    fun toWord(s,i) =
	case (String.sub(s,1), String.sub(s,2))
	  of ( (#"b",_) | (_,#"b") ) => toWord'(String.extract(s,3,NONE), BIN,i)
	   | ( (#"x",_) | (_,#"x") ) => toWord'(String.extract(s,3,NONE), HEX,i)
	   |            _            => toWord'(String.extract(s,2,NONE), DEC,i)

    and toWord'(s,b,i) = valOf(scanString (Word.scan b) s)
			 handle Overflow =>
				Error.error(i, "word constant too big")

    fun toReal(s,i) = s


    fun toString(s,i) =
	let
            fun base(s,b,m) =
		Char.chr(valOf(scanString (Int.scan b) s))
		handle (Chr | Overflow) =>
			 Error.error(i, m ^ " escape character too big")

            fun dec s     = base(s, DEC, "ASCII")
	    fun unicode s = base(s, HEX, "unicode")

	    fun convert(k,cs) =
		case String.sub(s,k)
		  of #"\"" => String.implode(List.rev cs)
		   | #"\\" => escape(k+1,cs)
		   |   c   => convert(k+1, c::cs)

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
				  convert(k+2, Char.chr(Char.ord c - 64)::cs)
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
			      else Crash.crash "Lexer.toString: \
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
	    if String.size ss' = 1 then
		String.sub(ss', 0)
	    else if ss' = "" then
		Error.error(i, "empty character constant")
	    else
		Error.error(i, "character constant too long")
	end

    end (* local *)


%%


%header	( functor Lexer(structure Tokens: Parser_TOKENS) );

%s COMMENT;

%full


  formatting = [\ \t\n\011\012\013]+;
  letter     = [A-Za-z];
  symbol     = [-!%&$#+/:<=>?@\\~`|*^];
  digit      = [0-9];
  bindigit   = [0-1];
  hexdigit   = [0-9a-fA-F];

  posdecint  = {digit}+;
  posbinint  = "0b"{bindigit}+;
  poshexint  = "0x"{hexdigit}+;
  negdecint  = "~"{posdecint};
  negbinint  = "~"{posbinint};
  neghexint  = "~"{poshexint};
  decint     = {posdecint} | {negdecint};
  binint     = {posbinint} | {negbinint};
  hexint     = {poshexint} | {neghexint};
  decword    = "0w"{digit}+;
  binword    = "0"("wb"|"bw"){bindigit}+;
  hexword    = "0"("wx"|"xw"){hexdigit}+;

  int        = {decint} | {binint} | {hexint};
  word       = {decword} | {binword} | {hexword};
  exp        = "e" | "E";
  real       = ({decint}"."{digit}+ ({exp}{decint})?) | ({decint}{exp}{decint});

  numericlab = [1-9]{digit}*;
  alphanumid = {letter}({letter} | {digit} | [_'])*;
  symbolicid = {symbol}+;
  id         = {alphanumid} | {symbolicid};
  tyvar      = "'"({letter} | {digit} | [_'])*;

  printable  = [^\000-\032"\127\\];
  escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r" |
	       ("\\^"[@-_])  | ("\\"{digit}{3}) | ("\\x"{hexdigit}{2}) |
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

  <INITIAL>"abstype"	=> ( token(ABSTYPE,   yypos, yytext) );
  <INITIAL>"and"	=> ( token(AND,       yypos, yytext) );
  <INITIAL>"andalso"	=> ( token(ANDALSO,   yypos, yytext) );
  <INITIAL>"as"		=> ( token(AS,        yypos, yytext) );
  <INITIAL>"case"	=> ( token(CASE,      yypos, yytext) );
  <INITIAL>"con"	=> ( token(CON,       yypos, yytext) );
  <INITIAL>"datatype"	=> ( token(DATATYPE,  yypos, yytext) );
  <INITIAL>"do"		=> ( token(DO,        yypos, yytext) );
  <INITIAL>"else"	=> ( token(ELSE,      yypos, yytext) );
  <INITIAL>"end"	=> ( token(END,       yypos, yytext) );
  <INITIAL>"eqtype"	=> ( token(EQTYPE,    yypos, yytext) );
  <INITIAL>"exception"	=> ( token(EXCEPTION, yypos, yytext) );
  <INITIAL>"fn"		=> ( token(FN,        yypos, yytext) );
  <INITIAL>"fun"	=> ( token(FUN,       yypos, yytext) );
  <INITIAL>"functor"	=> ( token(FUNCTOR,   yypos, yytext) );
  <INITIAL>"handle"	=> ( token(HANDLE,    yypos, yytext) );
  <INITIAL>"if"		=> ( token(IF,        yypos, yytext) );
  <INITIAL>"in"		=> ( token(IN,        yypos, yytext) );
  <INITIAL>"include"	=> ( token(INCLUDE,   yypos, yytext) );
  <INITIAL>"infix"	=> ( token(INFIX,     yypos, yytext) );
  <INITIAL>"infixr"	=> ( token(INFIXR,    yypos, yytext) );
  <INITIAL>"let"	=> ( token(LET,       yypos, yytext) );
  <INITIAL>"local"	=> ( token(LOCAL,     yypos, yytext) );
  <INITIAL>"nonfix"	=> ( token(NONFIX,    yypos, yytext) );
  <INITIAL>"of"		=> ( token(OF,        yypos, yytext) );
  <INITIAL>"op"		=> ( token(OP,        yypos, yytext) );
  <INITIAL>"open"	=> ( token(OPEN,      yypos, yytext) );
  <INITIAL>"orelse"	=> ( token(ORELSE,    yypos, yytext) );
  <INITIAL>"raise"	=> ( token(RAISE,     yypos, yytext) );
  <INITIAL>"rec"	=> ( token(REC,       yypos, yytext) );
  <INITIAL>"sharing"	=> ( token(SHARING,   yypos, yytext) );
  <INITIAL>"sig"	=> ( token(SIG,       yypos, yytext) );
  <INITIAL>"signature"	=> ( token(SIGNATURE, yypos, yytext) );
  <INITIAL>"struct"	=> ( token(STRUCT,    yypos, yytext) );
  <INITIAL>"structure"	=> ( token(STRUCTURE, yypos, yytext) );
  <INITIAL>"then"	=> ( token(THEN,      yypos, yytext) );
  <INITIAL>"type"	=> ( token(TYPE,      yypos, yytext) );
  <INITIAL>"val"	=> ( token(VAL,       yypos, yytext) );
  <INITIAL>"when"	=> ( token(WHEN,      yypos, yytext) );
  <INITIAL>"where"	=> ( token(WHERE,     yypos, yytext) );
  <INITIAL>"while"	=> ( token(WHILE,     yypos, yytext) );
  <INITIAL>"with"	=> ( token(WITH,      yypos, yytext) );
  <INITIAL>"withtype"	=> ( token(WITHTYPE,  yypos, yytext) );

  <INITIAL>"0"		=> ( token  (ZERO,              yypos, yytext) );
  <INITIAL>[1-9]	=> ( tokenOf(DIGIT,   toInt,    yypos, yytext) );
  <INITIAL>{numericlab}	=> ( tokenOf(NUMERIC, toInt,    yypos, yytext) );
  <INITIAL>{int}	=> ( tokenOf(INT,     toInt,    yypos, yytext) );
  <INITIAL>{word}	=> ( tokenOf(WORD,    toWord,   yypos, yytext) );
  <INITIAL>{real}	=> ( tokenOf(REAL,    toReal,   yypos, yytext) );
  <INITIAL>{string}	=> ( tokenOf(STRING,  toString, yypos, yytext) );
  <INITIAL>{char}	=> ( tokenOf(CHAR,    toChar,   yypos, yytext) );

  <INITIAL>{tyvar}	=> ( tokenOf(TYVAR,   toId,     yypos, yytext) );
  <INITIAL>{alphanumid}	=> ( tokenOf(ALPHA,   toId,     yypos, yytext) );
  <INITIAL>{symbolicid}	=> ( tokenOf(SYMBOL,  toId,     yypos, yytext) );


  <INITIAL>"(*"		=> ( nesting := 1 ; YYBEGIN COMMENT ; continue() );
  <COMMENT>"(*"		=> ( nesting := !nesting+1 ; continue() );
  <COMMENT>"*)"		=> ( nesting := !nesting-1 ;
			     if !nesting = 0 then YYBEGIN INITIAL else () ;
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );


  <INITIAL>"\""		=> ( error(yypos, yytext, "invalid string") );
  <INITIAL>.		=> ( invalid(yypos, yytext) );
