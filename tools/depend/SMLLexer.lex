(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

    open Tokens

    structure E = Error

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

    fun nest yypos = nesting := yypos :: !nesting
    fun unnest ()  = ( nesting := List.tl(!nesting) ; List.null(!nesting) )

    fun eof() =
	case !nesting
	  of []    => raise LexerError.EOF(fn i => Tokens.EOF i)
	   | i0::_ =>
		raise LexerError.EOF(fn(i1,i2) =>
					error((i0,i2), E.UnclosedComment))



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

    fun error'(yypos, yytext, e) = error(toLRPos(yypos, yytext), e)


    datatype radix = datatype StringCvt.radix

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
			      else raise Assert ("", 0) 

	    and gap(k, cs) =
		    if String.sub(s,k) = #"\\" then
			convert(k+1, cs)
		    else
			gap(k+1, cs)

	in
	    convert(1,[])
	end

%%


%header	( functor MkLexer(structure Tokens:     Parser_TOKENS
			  structure LexerError: LEXER_ERROR
			  where type token = (Tokens.svalue,int) Tokens.token
			  where type error = Error.error));

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
	       ("\\^"[@-_])  | ("\\"{digit}{3}) | ("\\u"{hexdigit}{4}) |
	       "\\\"" | "\\\\";
  gap        = ("\\"{formatting}"\\");
  stringchar = {printable} | " " | {escape};
  string     = "\""({stringchar} | {gap})*"\"";
  char       = "#"{string};

  interpunct = [(),.;] | "[" | "]" | "{" | "}";
  token      = {int} | {word} | {real} |
	       {tyvar} | {alphanumid} | {symbolicid} | "_" | {interpunct};

%%

  <INITIAL>{formatting}	=> ( continue() );
  <INITIAL>"import"	=> ( token(IMPORT, yypos, yytext) );
  <INITIAL>{token}	=> ( token(OTHER,  yypos, yytext) );
  <INITIAL>{string}	=> ( tokenOf(STRING, toString, yypos, yytext) );

  <INITIAL>"(*"		=> ( nest(yypos-2) ; YYBEGIN COMMENT ; continue() );
  <COMMENT>"(*"		=> ( nest(yypos-2) ; continue() );
  <COMMENT>"*)"		=> ( if unnest() then YYBEGIN INITIAL else () ;
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );

  <INITIAL>"\""		=> ( error'(yypos, yytext, E.InvalidString) );
  <INITIAL>.		=> ( error'(yypos, yytext,
				    E.InvalidChar(String.sub(yytext,0))) );
