open Tokens

type ('a,'b) token = ('a,'b) Tokens.token
type pos           = int
type svalue        = Tokens.svalue
type lexresult     = (svalue, pos) token

exception Error of int * string

fun eof() = EOF(~1,~1)

fun toLRPos(yypos, yytext) =
    let
	val yypos = yypos - 2	(* bug in ML-Lex... *)
    in
	(yypos, yypos + String.size yytext)
    end

fun token(yypos, yytext, TOKEN) =
	TOKEN (toLRPos(yypos, yytext))

fun tokenOf(yypos, yytext, TOKEN, arg) =
    let
	val (l,r) = toLRPos(yypos, yytext)
    in
	TOKEN(arg, l, r)
    end

%%

%header	(functor Lexer(structure Tokens : Parser_TOKENS));

whitechar  = [\ \t\n\011\012\013];
comment    = "%" [^\n\011]*;
whitespace = ({whitechar} | {comment})+;

letter     = [A-Za-z];
digit      = [0-9];

identifier = {letter} ({letter} | {digit} | "-" | "_")*;
binder     = "/" {identifier};

decnumber  = {digit}+;
integer    = "-"? {decnumber};
exponent   = "e" "-"? {decnumber} | "E" "-"? {decnumber};
real       = ("-"? {decnumber} "." {decnumber} {exponent}?)
           | ("-"? {decnumber} {exponent});
number     = {integer} | {real};

printable  = [^\000-\031"\127\\];
string     = "\"" {printable}* "\"";

%%

<INITIAL> {whitespace}	=> ( continue() );

<INITIAL> "["		=> ( token(yypos, yytext, LBRACK) );
<INITIAL> "]"		=> ( token(yypos, yytext, RBRACK) );
<INITIAL> "{"		=> ( token(yypos, yytext, LBRACE) );
<INITIAL> "}"		=> ( token(yypos, yytext, RBRACE) );

<INITIAL> {identifier}	=> ( tokenOf(yypos, yytext, IDENTIFIER, yytext) );
<INITIAL> {binder}	=> ( tokenOf(yypos, yytext, BINDER,
				     String.extract(yytext, 1, NONE)) );

<INITIAL> {integer}	=> ( tokenOf(yypos, yytext, INTEGER,
				     valOf(Int.fromString yytext)) );
<INITIAL> {real}	=> ( tokenOf(yypos, yytext, REAL,
				     valOf(Real.fromString yytext)) );
<INITIAL> {string}	=> ( tokenOf(yypos, yytext, STRING,
				     String.extract(yytext, 1,
				     	SOME(String.size yytext - 2))) );

<INITIAL> "\""		=> ( raise Error(yypos, "invalid string") );
<INITIAL> .		=> ( raise Error(yypos, "illegal character") );
