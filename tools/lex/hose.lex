
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

(* handling of nested comments *)
val nesting = ref 0

val pos = ref 0

fun ++ x = ( x := !x + 1; !x)

fun error (e, pos) =
    raise AbsSyn.Error ("Lex Error in line " ^ (Int.toString pos) ^ ": "
			^ e ^ "\n")

val eof = fn () => if !nesting = 0 then Tokens.EOF(!pos,!pos)
		   else error ("unclosed comment", !pos)

(* used by: hose.lex
 * countlines : string -> unit
 * counts the new lines in the string and changes pos
 *)
fun countlines s =
    let
	val cs = explode s
	fun countNext nil         = ()
	  | countNext (#"\n"::cs) = (++ pos; countNext cs)
	  | countNext ( _   ::cs) = countNext cs
    in
	countNext cs
    end


%%


%header (functor MkLexer(structure Tokens: Parser_TOKENS));

%s COMMENT;


formatting'= [\ \t\011\012\013];
formatting = {formatting'} | "\n";
symbol     = [-!%&$#+/:<=>?@\\~`|*^];
digit      = [0-9];
hexdigit   = [0-9a-fA-F];
posdecint  = {digit}+;
negdecint  = "~"{posdecint};
decint     = {posdecint} | {negdecint};
exp        = "E" | "e";
real       = ({decint}"."{digit}+ ({exp}{decint})?) | ({decint}{exp}{decint});
printable  = [^\000-\032"\127\\];
escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r"
           | ("\\^"[@-_])  | ("\\"{digit}{3})  | ("\\u"{hexdigit}{4})
           | "\\\"" | "\\\\";
gap        = "\\" {formatting}+ "\\";
stringchar = {printable} | " " | {escape};
string     = "\""({stringchar} | {gap})*"\"";
char       = "#\""{gap}*{stringchar}{gap}*"\"";
alpha      = [A-Za-z];
alphaNum   = [A-Za-z0-9_'];
mlId       = "'"?[A-Za-z][A-Za-z0-9_'.]*;
 
%%

<INITIAL>"\n"                => (++ pos; lex());
<INITIAL>{formatting'}+      => (lex());
<INITIAL>"regexp"            => (Tokens.REGEXP(!pos,!pos));
<INITIAL>"lexer"             => (Tokens.LEXER(!pos,!pos));
<INITIAL>"and"               => (Tokens.AND(!pos,!pos));
<INITIAL>"regcase"           => (Tokens.REGCASE(!pos,!pos));
<INITIAL>"of"                => (Tokens.OF(!pos,!pos));
<INITIAL>"constructor"       => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"datatype"          => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"end"               => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"eqtype"            => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"exception"         => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"exttype"           => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"from"              => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"fun"               => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"functor"           => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"in"                => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"include"           => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"infix"             => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"infixr"            => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"import"            => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"local"             => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"nonfix"            => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"open"              => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"sharing"           => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"signature"         => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"structure"         => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"type"              => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"val"               => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"withtype"          => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>";"                 => (Tokens.MLKEY(yytext,!pos,!pos));
<INITIAL>"="                 => (Tokens.EQ(!pos,!pos));
<INITIAL>"("                 => (Tokens.LPAR(!pos,!pos));
<INITIAL>")"                 => (Tokens.RPAR(!pos,!pos));
<INITIAL>"*"                 => (Tokens.TIMES(!pos,!pos));
<INITIAL>"+"                 => (Tokens.PLUS(!pos,!pos));
<INITIAL>"?"                 => (Tokens.QMARK(!pos,!pos));
<INITIAL>"=>"                => (Tokens.DRARROW(!pos,!pos));
<INITIAL>"|"                 => (Tokens.BAR(!pos,!pos));
<INITIAL>"^"                 => (Tokens.CARAT(!pos,!pos));
<INITIAL>"["                 => (Tokens.LBRACK(!pos,!pos));
<INITIAL>"]"                 => (Tokens.RBRACK(!pos,!pos));
<INITIAL>","                 => (Tokens.COMMA(!pos,!pos));
<INITIAL>"{"                 => (Tokens.LBRACE(!pos,!pos));
<INITIAL>"}"                 => (Tokens.RBRACE(!pos,!pos));
<INITIAL>"_"                 => (Tokens.WILDCARD(!pos,!pos));
<INITIAL>{symbol}+           => (Tokens.MLOP(yytext,!pos,!pos));
<INITIAL>{decint}            => (Tokens.NUM(valOf(Int.fromString(yytext)),
					    !pos,!pos));
<INITIAL>{real}              => (Tokens.REAL(valOf(Real.fromString(yytext)),
					     !pos,!pos));
<INITIAL>{string}            => (countlines(yytext);
				 Tokens.STRING(yytext, !pos,!pos));
<INITIAL>{alpha}{alphaNum}*  => (Tokens.ID(yytext,!pos,!pos));
<INITIAL>{char}              => (countlines(yytext);
				 Tokens.MLTOK(yytext, !pos,!pos));
<INITIAL>{mlId}              => (Tokens.MLTOK(yytext, !pos,!pos));
<INITIAL>"(*"                => (nesting := 1;YYBEGIN COMMENT; lex());
<COMMENT>"(*"                => (nesting := !nesting+1; lex());
<COMMENT>"*)"                => (nesting := !nesting-1;
				 if !nesting=0 then YYBEGIN INITIAL else ();
				     lex());
<COMMENT>"\n"                => (++ pos; lex());
<COMMENT>.                   => (lex());
<INITIAL>.                   => (error ("bad character '"
					^ yytext ^ "'" , !pos));
