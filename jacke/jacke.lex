structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

(* handling of nested comments *)
val nesting = ref 0

fun eof() = let 
		val pos = hd(!linePos) 
	    in (if !nesting >0 then ErrorMsg.error pos "unclosed comment"
		else ();
		Tokens.EOF(pos,pos)) 
	    end


(* converting fns *)
fun toInt s = 
    foldl (fn (c,acc) => acc*10 + (ord c - ord #"0")) 0 (explode s)


fun toString pos s = 
    let
    fun dec i s = Char.chr(Option.valOf(StringCvt.scanString 
					(Int.scan StringCvt.DEC) s))
	handle (Chr | Overflow) =>
	    (ErrorMsg.error i "ASCII escape character too big"; #"0")
 
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
	  | #"^"  => let val c = String.sub(s,k+1) 
		     in convert(k+2, Char.chr(Char.ord c -
					      Char.ord #"0")::cs)
		     end
	  |   c   => if Char.isDigit c then
		     let val s' = String.extract(s, k, SOME 3) in
			 convert(k+3, dec (pos+k) s' :: cs)
		     end
		     else if Char.isSpace c then
			 gap(k+1, cs)
			  else (ErrorMsg.error pos 
				"invalid escape sequence"; "") 
	and gap(k, cs) =
	    if String.sub(s,k) = #"\\" then
		convert(k+1, cs)
	    else
		gap(k+1, cs)
    in
	convert(1,[])
    end


%%

%header (functor jackeLexFun(structure Tokens: jacke_TOKENS));
%s COMMENT;

alpha=[A-Za-z];
symbol     = [-!%&$#+/:<=>?@\\~`|*^];
digit=[0-9];
alphanum = {alpha} ({alpha} | {digit} | "_" | "'")*;
formatting = [\ \t\011\012\013]+;
printable  = [^\000-\032"\127\\];
escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r" |
             ("\\"{digit}{3}) | "\\\"" | "\\\\";
gap        = ("\\"({formatting}|"\n")*"\\");
stringchar = {printable} | " " | {escape};
string     = "\""({stringchar} | {gap})*"\"";

%%

<INITIAL>\n       => (lineNum := !lineNum+1; 
		      linePos := yypos :: !linePos; continue());
<INITIAL>{formatting} => (continue());

<INITIAL>"token"  => (Tokens.TOKEN(yypos,yypos+5));
<INITIAL>"assocl" => (Tokens.ASSOCL(yypos,yypos+6));
<INITIAL>"assocr" => (Tokens.ASSOCR(yypos,yypos+6));
<INITIAL>"nonassoc" => (Tokens.NONASSOC(yypos,yypos+8));
<INITIAL>"rule"   => (Tokens.RULE(yypos,yypos+4));
<INITIAL>"parser" => (Tokens.PARSER(yypos,yypos+6));
<INITIAL>"prec"   => (Tokens.PREC(yypos,yypos+4));
<INITIAL>"skip"   => (Tokens.SKIP(yypos,yypos+4));

<INITIAL>","      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"      => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("      => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"      => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"let"    => (Tokens.LET(yypos,yypos+3));
<INITIAL>"in"     => (Tokens.IN(yypos,yypos+2));
<INITIAL>"end"    => (Tokens.END(yypos,yypos+3));
<INITIAL>"and"    => (Tokens.AND(yypos,yypos+3));
<INITIAL>"of"     => (Tokens.OF(yypos,yypos+2));
<INITIAL>"as"     => (Tokens.AS(yypos,yypos+2));
<INITIAL>"=>"     => (Tokens.MAPSTO(yypos,yypos+2));
<INITIAL>"|"      => (Tokens.BAR(yypos,yypos+1));
<INITIAL>"abstype" => (Tokens.ABSTYPE(yypos,yypos+7));

<INITIAL>"andalso"  => (Tokens.MLKEY("andalso",yypos,yypos+7));
<INITIAL>"case"  => (Tokens.MLKEY("case",yypos,yypos+4));
<INITIAL>"datatype"  => (Tokens.DECINTRO("datatype",yypos,yypos+8));
<INITIAL>"do"  => (Tokens.MLKEY("do",yypos,yypos+2));
<INITIAL>"else"  => (Tokens.MLKEY("else",yypos,yypos+4));
<INITIAL>"eqtype"  => (Tokens.MLKEY("eqtype",yypos,yypos+6));
<INITIAL>"exception" => (Tokens.DECINTRO("exception",yypos,yypos+9));
<INITIAL>"fn"  => (Tokens.MLKEY("fn",yypos,yypos+2));
<INITIAL>"fun"  => (Tokens.MLKEY("fun",yypos,yypos+3));
<INITIAL>"functor"  => (Tokens.MLKEY("functor",yypos,yypos+7));
<INITIAL>"handle"  => (Tokens.MLKEY("handle",yypos,yypos+6));
<INITIAL>"if"  => (Tokens.MLKEY("if",yypos,yypos+2));
<INITIAL>"include"  => (Tokens.MLKEY("include",yypos,yypos+7));
<INITIAL>"infix"  => (Tokens.DECINTRO("infix",yypos,yypos+5));
<INITIAL>"infixr"  => (Tokens.DECINTRO("infixr",yypos,yypos+6));
<INITIAL>"local"  => (Tokens.LOCAL(yypos,yypos+5));
<INITIAL>"nonfix"  => (Tokens.MLKEY("nonfix",yypos,yypos+6));
<INITIAL>"op"  => (Tokens.MLKEY("op",yypos,yypos+2));
<INITIAL>"open"  => (Tokens.DECINTRO("open",yypos,yypos+4));
<INITIAL>"orelse"  => (Tokens.MLKEY("orelse",yypos,yypos+6));
<INITIAL>"raise"  => (Tokens.MLKEY("raise",yypos,yypos+5));
<INITIAL>"rec"  => (Tokens.MLKEY("rec",yypos,yypos+3));
<INITIAL>"sharing"  => (Tokens.MLKEY("sharing",yypos,yypos+7));
<INITIAL>"sig"  => (Tokens.MLKEY("sig",yypos,yypos+3));
<INITIAL>"signature"  => (Tokens.MLKEY("signature",yypos,yypos+9));
<INITIAL>"struct"  => (Tokens.MLKEY("struct",yypos,yypos+6));
<INITIAL>"structure"  => (Tokens.MLKEY("structure",yypos,yypos+9));
<INITIAL>"then"  => (Tokens.MLKEY("then",yypos,yypos+4));
<INITIAL>"type"  => (Tokens.DECINTRO("type",yypos,yypos+4));
<INITIAL>"val"  => (Tokens.DECINTRO("val",yypos,yypos+3));
<INITIAL>"where"  => (Tokens.MLKEY("where",yypos,yypos+5));
<INITIAL>"while"  => (Tokens.MLKEY("while",yypos,yypos+5));
<INITIAL>"with"  => (Tokens.WITH(yypos,yypos+4));
<INITIAL>"withtype"  => (Tokens.MLKEY("withtype",yypos,yypos+8));

<INITIAL>"["    => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"    => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"    => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"    => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"+"    => (Tokens.MLOP("+",yypos,yypos+1));
<INITIAL>"-"    => (Tokens.MLOP("-",yypos,yypos+1));
<INITIAL>"*"    => (Tokens.MLOP("*",yypos,yypos+1));
<INITIAL>"/"    => (Tokens.MLOP("/",yypos,yypos+1));
<INITIAL>"~"    => (Tokens.UMINUS(yypos,yypos+1));
<INITIAL>"->"   => (Tokens.MLOP("->",yypos,yypos+2));
<INITIAL>"<>"   => (Tokens.MLOP("<>",yypos,yypos+2));
<INITIAL>"<"    => (Tokens.MLOP("<",yypos,yypos+1));
<INITIAL>"<="   => (Tokens.MLOP("<=",yypos,yypos+2));
<INITIAL>">"    => (Tokens.MLOP(">",yypos,yypos+1));
<INITIAL>">="   => (Tokens.MLOP(">=",yypos,yypos+2));
<INITIAL>":="   => (Tokens.MLOP(":=",yypos,yypos+2));
<INITIAL>"!"    => (Tokens.MLOP("!",yypos,yypos+1));
<INITIAL>"@"    => (Tokens.MLOP("@",yypos,yypos+1));
<INITIAL>"^"    => (Tokens.MLOP("=",yypos,yypos+1));


<INITIAL>{alphanum} => (Tokens.ID(yytext,yypos,yypos+
					       (String.size yytext)));
<INITIAL>{string} => (Tokens.STRING(toString yypos yytext,
				    yypos,yypos+size(yytext)));
<INITIAL>{digit}+ => (Tokens.INT(toInt yytext,yypos,yypos+size(yytext)));

<INITIAL>"(*"    => ( nesting := 1 ; YYBEGIN COMMENT ; continue() );
<COMMENT>"(*"    => ( nesting := !nesting+1 ; continue() );
<COMMENT>"*)"    => (nesting := !nesting-1 ;
		     if !nesting = 0 then YYBEGIN INITIAL else () ;
		     continue() );
<COMMENT>"\n"    => (lineNum := !lineNum+1; 
		     linePos := yypos :: !linePos;
		     continue() );
<COMMENT>.       => (continue() );
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character "^yytext);
		      continue());
