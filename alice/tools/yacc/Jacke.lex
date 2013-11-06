
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

(* handling of nested comments *)
val nesting = ref 0

val pos = ref 0

fun ++ x = x := !x + 1

fun -- x = x := !x - 1 

fun eof() = let 
		val pos = hd(!linePos) 
	    in (if !nesting >0 then ErrorMsg.error pos "unclosed comment"
		else ();
		Tokens.EOF(pos,pos)) 
	    end

(* countlines : string -> unit
 * counts the new lines in the string and changes pos
 *)
fun countlines (s, yypos) =
    let
	val cs = explode s
	fun countNext nil         = ()
	  | countNext (#"\n"::cs) =
	    (++ lineNum; linePos := yypos :: !linePos; countNext cs)
	  | countNext ( _   ::cs) = countNext cs
    in
	countNext cs
    end



%%


%header (functor MkLexer(structure Tokens: Parser_TOKENS));

%s COMMENT;


formatting'= [\ \t\011\012\013];
formatting = {formatting'} | "\n";
symbol     = [-!%&$#+/:<=>?@\\~`|*^_;];
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
mlId       = "'"?[A-Za-z][A-Za-z0-9_']*;
 
%%

<INITIAL>"\n"                => (++ lineNum;
				 linePos := yypos :: !linePos;
				 lex());
<INITIAL>{formatting'}+      => (lex());

<INITIAL>"token"    => (Tokens.TOKEN(yypos,yypos+5));
<INITIAL>"assocl"   => (Tokens.ASSOCL(yypos,yypos+6));
<INITIAL>"assocr"   => (Tokens.ASSOCR(yypos,yypos+6));
<INITIAL>"nonassoc" => (Tokens.NONASSOC(yypos,yypos+8));
<INITIAL>"rule"     => (Tokens.RULE(yypos,yypos+4));
<INITIAL>"parser"   => (Tokens.PARSER(yypos,yypos+6));
<INITIAL>"prec"     => (Tokens.PREC(yypos,yypos+4));
<INITIAL>"skip"     => (Tokens.SKIP(yypos,yypos+4));

<INITIAL>"constructor"       => (Tokens.MLKEY(yytext,yypos,yypos+11));
<INITIAL>"datatype"          => (Tokens.MLKEY(yytext,yypos,yypos+8));
<INITIAL>"end"               => (Tokens.MLKEY(yytext,yypos,yypos+3));
<INITIAL>"eqtype"            => (Tokens.MLKEY(yytext,yypos,yypos+6));
<INITIAL>"exception"         => (Tokens.MLKEY(yytext,yypos,yypos+9));
<INITIAL>"exttype"           => (Tokens.MLKEY(yytext,yypos,yypos+7));
<INITIAL>"from"              => (Tokens.MLKEY(yytext,yypos,yypos+4));
<INITIAL>"fun"               => (Tokens.MLKEY(yytext,yypos,yypos+3));
<INITIAL>"functor"           => (Tokens.MLKEY(yytext,yypos,yypos+7));
<INITIAL>"in"                => (Tokens.MLKEY(yytext,yypos,yypos+2));
<INITIAL>"include"           => (Tokens.MLKEY(yytext,yypos,yypos+7));
<INITIAL>"infix"             => (Tokens.MLKEY(yytext,yypos,yypos+5));
<INITIAL>"infixr"            => (Tokens.MLKEY(yytext,yypos,yypos+6));
<INITIAL>"import"            => (Tokens.MLKEY(yytext,yypos,yypos+6));
<INITIAL>"local"             => (Tokens.MLKEY(yytext,yypos,yypos+5));
<INITIAL>"nonfix"            => (Tokens.MLKEY(yytext,yypos,yypos+6));
<INITIAL>"open"              => (Tokens.MLKEY(yytext,yypos,yypos+4));
<INITIAL>"sharing"           => (Tokens.MLKEY(yytext,yypos,yypos+7));
<INITIAL>"signature"         => (Tokens.MLKEY(yytext,yypos,yypos+9));
<INITIAL>"structure"         => (Tokens.MLKEY(yytext,yypos,yypos+9));
<INITIAL>"type"              => (Tokens.MLKEY(yytext,yypos,yypos+4));
<INITIAL>"val"               => (Tokens.MLKEY(yytext,yypos,yypos+3));
<INITIAL>"withtype"          => (Tokens.MLKEY(yytext,yypos,yypos+8));

<INITIAL>"regexp"            => (Tokens.MLKEY(yytext,yypos,yypos+6));
<INITIAL>"lexer"             => (Tokens.MLKEY(yytext,yypos,yypos+5));

<INITIAL>"."      => (Tokens.DOT(yypos,yypos+1));
<INITIAL>","      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"      => (Tokens.COLON(yypos,yypos+1));
<INITIAL>"("      => (Tokens.LPAR(yypos,yypos+1));
<INITIAL>")"      => (Tokens.RPAR(yypos,yypos+1));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"and"    => (Tokens.AND(yypos,yypos+3));
<INITIAL>"of"     => (Tokens.OF(yypos,yypos+2));
<INITIAL>"as"     => (Tokens.AS(yypos,yypos+2));
<INITIAL>"=>"     => (Tokens.DRARROW(yypos,yypos+2));
<INITIAL>"|"      => (Tokens.BAR(yypos,yypos+1));
<INITIAL>"["    => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"    => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"    => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"    => (Tokens.RBRACE(yypos,yypos+1));


<INITIAL>{decint}            => (Tokens.INT(valOf(Int.fromString(yytext)),
					    yypos,yypos+size yytext));
<INITIAL>{real}              => (Tokens.REAL(valOf(Real.fromString(yytext)),
					     yypos,yypos+size yytext));
<INITIAL>{string}            => (countlines(yytext, yypos);
				 Tokens.STRING(yytext, yypos,
					       yypos+size yytext));
<INITIAL>{alpha}{alphaNum}*  => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL>{symbol}+           => (Tokens.MLTOK(yytext,yypos,yypos+size yytext));
<INITIAL>{char}              => (countlines(yytext, yypos);
				 Tokens.MLTOK(yytext, yypos,yypos+size yytext));
<INITIAL>{mlId}              => (Tokens.MLTOK(yytext, yypos,yypos+size yytext));
<INITIAL>"(*"                => (nesting := 1;YYBEGIN COMMENT; lex());
<COMMENT>"(*"                => (++ nesting; lex());
<COMMENT>"*)"                => (-- nesting;
				 if !nesting=0 then YYBEGIN INITIAL else ();
				     lex());
<COMMENT>"\n"                => (++ lineNum;
				 linePos := yypos :: !linePos;
				 lex());
<COMMENT>.                   => (lex());
<INITIAL>.                   => (ErrorMsg.error yypos ("illegal character '"
					^ yytext ^ "'" ); raise ErrorMsg.Error);
