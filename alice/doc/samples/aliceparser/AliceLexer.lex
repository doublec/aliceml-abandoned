(* Author:									*)
(*   Benedikt Grundmann <bgrund@ps.uni-sb.de>					*)
(*										*)
(* Copyright:									*)
(*   Benedikt Grundmann, 2005   						*)
(*   Converted from ml-lex grammar by Andreas Rossberg                          *)
(*										*)
(* Last change:									*)
(*   $Date$ by $Author$				*)
(*   $Revision$							*)
import structure AliceParser    from "AliceParser"
import structure Lexer          from "x-alice:/lib/lex/Lexer"

structure AliceLexer =
struct

    structure P = AliceParser

    type pos = int * int

    fun ret (t, yytext, yyline, yycol) = 
        (SOME t, (yyline, yycol), (yyline, yycol+String.size yytext))

    fun retConv (t, conv, yytext, yyline, yycol) =
        (SOME (t (conv yytext)), (yyline, yycol), (yyline, yycol+String.size yytext))


  (* Handling nested comments *)

    val nesting = ref [] : pos list ref

    fun nest pos = nesting := pos :: !nesting
    fun unnest ()  = ( nesting := List.tl(!nesting) ; List.null(!nesting) )

    fun eof (yyline, yycol) =
        case !nesting of
            []          => (NONE, (yyline, yycol), (yyline, yycol) )
          | p :: _      => raise P.Error (p, (yyline, yycol))

  (* Convert identifiers and constants *)

    datatype radix = datatype StringCvt.radix

    fun toId s  = s

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

    fun toWord s =
	case (String.sub(s,1), String.sub(s,2))
	  of ( (#"b",_) | (_,#"b") ) => toWord'(String.extract(s,3,NONE), BIN)
	   | ( (#"x",_) | (_,#"x") ) => toWord'(String.extract(s,3,NONE), HEX)
	   |            _            => toWord'(String.extract(s,2,NONE), DEC)

    and toWord'(s,b) = Option.valOf(StringCvt.scanString (LargeWord.scan b) s)

    fun toReal s     = Option.valOf(StringCvt.scanString LargeReal.scan s)


    fun toString s =
	let
            fun base(s,b,m) =
		WideChar.chr(Option.valOf(StringCvt.scanString (Int.scan b) s))

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
			      else raise Fail "invalid escape sequence"

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
	    val ss' = toString s'
	in
	    if WideString.size ss' = 1 then
		WideString.sub(ss', 0)
	    else
                raise Fail "invalid char length"
	end

    regexp
        formatting = [" \t\n\011\012\013"]+
    and   letter     = ["A-Za-z"]
    and   symbol     = ["-!%&$#+/:<=>?@\\~`|*^"]
    and   digit      = ["0-9"]
    and   bindigit   = ["0-1"]
    and   hexdigit   = ["0-9a-fA-F"]
    and   xdigit     = digit | "_"
    and   xbindigit  = bindigit | "_"
    and   xhexdigit  = hexdigit | "_"
    and   digits     = xdigit* digit xdigit*
    and   bindigits  = xbindigit* bindigit xbindigit*
    and   hexdigits  = xhexdigit* hexdigit xhexdigit*

    and   posdecint   = digit xdigit*
    and   posbinint  = "0b" bindigits
    and   poshexint  = "0x" hexdigits
    and   negdecint  = "~" posdecint
    and   negbinint  = "~" posbinint
    and   neghexint  = "~" poshexint
    and   decint     = posdecint | negdecint
    and   binint     = posbinint | negbinint
    and   hexint     = poshexint | neghexint
    and   decword    = "0w" digits
    and   binword    = "0" ("wb"|"bw") bindigits
    and   hexword    = "0" ("wx"|"xw") hexdigits

    and   int        = decint | binint | hexint
    and   word       = decword | binword | hexword
    and   exp        = "e" | "E"
    and   real       = ( decint "." digits (exp decint)? ) | (decint exp decint)

    and   numericlab = ["1-9"] digit*
    and   alphanumid = letter (letter | digit | ["_'"])*
    and   symbolicid = symbol+
    and   id         = alphanumid | symbolicid
    and   tyvar      = "'"(letter | digit | ["_'"])*

    and   printable  = [^"\000-\032\"\127\\"]
    and   escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r" |
                        ("\\^"["@-_"])  | ("\\" digit digit digit) | 
                        ("\\u" hexdigit hexdigit hexdigit hexdigit) |
                        "\\\"" | "\\\\"
    and   gap        = ("\\" formatting "\\")
    and   stringchar = printable | " " | escape
    and   string     = "\"" (stringchar | gap)* "\""
    and   char       = "#" string

    lexer lex =
        formatting  => ( lex () )
      | "#"         => ( ret (P.HASH, yytext, yyline, yycol) )
      | "#["        => ( ret (P.HASHBRACK, yytext, yyline, yycol) )
      | "("         => ( ret (P.LPAR, yytext, yyline, yycol) )
      | ")"         => ( ret (P.RPAR, yytext, yyline, yycol) )

      | "*"		=> ( ret (P.STAR,      yytext, yyline, yycol) )
      | ","		=> ( ret (P.COMMA,     yytext, yyline, yycol) )
      | "->"		=> ( ret (P.ARROW,     yytext, yyline, yycol) )
      | "."		=> ( ret (P.DOT,       yytext, yyline, yycol) )
      | "..."	        => ( ret (P.DOTS,      yytext, yyline, yycol) )
      | ":"		=> ( ret (P.COLON,     yytext, yyline, yycol) )
      | ":>"		=> ( ret (P.COLONGREATER, yytext, yyline, yycol) )
      | ";"		=> ( ret (P.SEMICOLON, yytext, yyline, yycol) )
      | "="		=> ( ret (P.EQUALS,    yytext, yyline, yycol) )
      | "=>"		=> ( ret (P.DARROW,    yytext, yyline, yycol) )
      | "["		=> ( ret (P.LBRACK,    yytext, yyline, yycol) )
      | "]"		=> ( ret (P.RBRACK,    yytext, yyline, yycol) )
      | "_"		=> ( ret (P.UNDERBAR,  yytext, yyline, yycol) )
      | "{"		=> ( ret (P.LBRACE,    yytext, yyline, yycol) )
      | "|"		=> ( ret (P.BAR,       yytext, yyline, yycol) )
      | "}"		=> ( ret (P.RBRACE,    yytext, yyline, yycol) )

      | "__eqeqtype"	=> ( ret (P.EQEQTYPE,  yytext, yyline, yycol) )
      | "__overload"	=> ( ret (P.OVERLOAD,  yytext, yyline, yycol) )
      | "__pervasive"   => ( ret (P.PERVASIVE, yytext, yyline, yycol) )
      | "__primitive"   => ( ret (P.PRIMITIVE, yytext, yyline, yycol) )
      | "__reftype"	=> ( ret (P.REFTYPE,   yytext, yyline, yycol) )
      | "_file_"	=> ( ret (P.FILE,      yytext, yyline, yycol) )
      | "_line_"	=> ( ret (P.LINE,      yytext, yyline, yycol) )
      | "abstype"	=> ( ret (P.ABSTYPE,   yytext, yyline, yycol) )
      | "and"	        => ( ret (P.AND,       yytext, yyline, yycol) )
      | "andalso"	=> ( ret (P.ANDALSO,   yytext, yyline, yycol) )
      | "any"	        => ( ret (P.ANY,       yytext, yyline, yycol) )
      | "as"		=> ( ret (P.AS,        yytext, yyline, yycol) )
      | "assert"	=> ( ret (P.ASSERT NONE,    yytext, yyline, yycol) )
      | "assert0"	=> ( ret (P.ASSERT(SOME 0), yytext, yyline, yycol) )
      | "assert1"	=> ( ret (P.ASSERT(SOME 1), yytext, yyline, yycol) )
      | "assert2"	=> ( ret (P.ASSERT(SOME 2), yytext, yyline, yycol) )
      | "assert3"	=> ( ret (P.ASSERT(SOME 3), yytext, yyline, yycol) )
      | "assert4"	=> ( ret (P.ASSERT(SOME 4), yytext, yyline, yycol) )
      | "assert5"	=> ( ret (P.ASSERT(SOME 5), yytext, yyline, yycol) )
      | "assert6"	=> ( ret (P.ASSERT(SOME 6), yytext, yyline, yycol) )
      | "assert7"	=> ( ret (P.ASSERT(SOME 7), yytext, yyline, yycol) )
      | "assert8"	=> ( ret (P.ASSERT(SOME 8), yytext, yyline, yycol) )
      | "assert9"	=> ( ret (P.ASSERT(SOME 9), yytext, yyline, yycol) )
      | "case"	        => ( ret (P.CASE,      yytext, yyline, yycol) )
      | "constructor"   => ( ret (P.CONSTRUCTOR,yytext, yyline, yycol) )
      | "datatype"	=> ( ret (P.DATATYPE,  yytext, yyline, yycol) )
      | "do"		=> ( ret (P.DO,        yytext, yyline, yycol) )
      | "else"	        => ( ret (P.ELSE,      yytext, yyline, yycol) )
      | "end"	        => ( ret (P.END,       yytext, yyline, yycol) )
      | "eqtype"	=> ( ret (P.EQTYPE,    yytext, yyline, yycol) )
      | "exception"	=> ( ret (P.EXCEPTION, yytext, yyline, yycol) )
      | "exttype"	=> ( ret (P.EXTTYPE,   yytext, yyline, yycol) )
      | "fct"       	=> ( ret (P.FCT,       yytext, yyline, yycol) )
      | "finally"	=> ( ret (P.FINALLY,   yytext, yyline, yycol) )
      | "fn"		=> ( ret (P.FN,        yytext, yyline, yycol) )
      | "from"	        => ( ret (P.FROM,      yytext, yyline, yycol) )
      | "fun"	        => ( ret (P.FUN,       yytext, yyline, yycol) )
      | "functor"	=> ( ret (P.FUNCTOR,   yytext, yyline, yycol) )
      | "handle"	=> ( ret (P.HANDLE,    yytext, yyline, yycol) )
      | "if"		=> ( ret (P.IF,        yytext, yyline, yycol) )
      | "import"	=> ( ret (P.IMPORT,    yytext, yyline, yycol) )
      | "in"		=> ( ret (P.IN,        yytext, yyline, yycol) )
      | "include"	=> ( ret (P.INCLUDE,   yytext, yyline, yycol) )
      | "infix"	        => ( ret (P.INFIX,     yytext, yyline, yycol) )
      | "infixr"	=> ( ret (P.INFIXR,    yytext, yyline, yycol) )
      | "lazy"	        => ( ret (P.LAZY,      yytext, yyline, yycol) )
      | "let"	        => ( ret (P.LET,       yytext, yyline, yycol) )
      | "local"	        => ( ret (P.LOCAL,     yytext, yyline, yycol) )
      | "non"	        => ( ret (P.NON,       yytext, yyline, yycol) )
      | "nonfix"	=> ( ret (P.NONFIX,    yytext, yyline, yycol) )
      | "of"		=> ( ret (P.OF,        yytext, yyline, yycol) )
      | "op"		=> ( ret (P.OP,        yytext, yyline, yycol) )
      | "open"	        => ( ret (P.OPEN,      yytext, yyline, yycol) )
      | "orelse"	=> ( ret (P.ORELSE,    yytext, yyline, yycol) )
      | "pack"	        => ( ret (P.PACK,      yytext, yyline, yycol) )
      | "raise"	        => ( ret (P.RAISE,     yytext, yyline, yycol) )
      | "rec"	        => ( ret (P.REC,       yytext, yyline, yycol) )
      | "sharing"	=> ( ret (P.SHARING,   yytext, yyline, yycol) )
      | "sig"	        => ( ret (P.SIG,       yytext, yyline, yycol) )
      | "signature"	=> ( ret (P.SIGNATURE, yytext, yyline, yycol) )
      | "spawn"	        => ( ret (P.SPAWN,     yytext, yyline, yycol) )
      | "struct"	=> ( ret (P.STRUCT,    yytext, yyline, yycol) )
      | "structure"	=> ( ret (P.STRUCTURE, yytext, yyline, yycol) )
      | "then"	        => ( ret (P.THEN,      yytext, yyline, yycol) )
      | "type"	        => ( ret (P.TYPE,      yytext, yyline, yycol) )
      | "unpack"	=> ( ret (P.UNPACK,    yytext, yyline, yycol) )
      | "val"	        => ( ret (P.VAL,       yytext, yyline, yycol) )
      | "where"	        => ( ret (P.WHERE,     yytext, yyline, yycol) )
      | "while"	        => ( ret (P.WHILE,     yytext, yyline, yycol) )
      | "with"	        => ( ret (P.WITH,      yytext, yyline, yycol) )
      | "withfun"	=> ( ret (P.WITHFUN,   yytext, yyline, yycol) )
      | "withtype"	=> ( ret (P.WITHTYPE,  yytext, yyline, yycol) )
      | "withval"	=> ( ret (P.WITHVAL,   yytext, yyline, yycol) )

      | "0"		=> ( ret (P.ZERO,      yytext, yyline, yycol) )
      | ["1-9"]	        => ( retConv (P.DIGIT,   toDigit,  yytext, yyline, yycol) )
      | numericlab	=> ( retConv (P.NUMERIC, toInt,    yytext, yyline, yycol) )
      | int	        => ( retConv (P.INT,     toInt,    yytext, yyline, yycol) )
      | word	        => ( retConv (P.WORD,    toWord,   yytext, yyline, yycol) )
      | real    	=> ( retConv (P.REAL,    toReal,   yytext, yyline, yycol) )
      | string  	=> ( retConv (P.STRING,  toString, yytext, yyline, yycol) )
      | char	        => ( retConv (P.CHAR,    toChar,   yytext, yyline, yycol) )

      | tyvar	        => ( retConv (P.TYVAR,   toId,     yytext, yyline, yycol) )
      | id		=> ( retConv (P.ALPHA,   toId,     yytext, yyline, yycol) )


      | "(*"		=> ( (nest(yyline, yycol); comment ()) )
      | eof             => ( eof (yyline, yycol) )
    
    and comment =
        "(*"            => ( (nest (yyline, yycol); comment ()) )
      | "*)"            => ( if unnest () then lex () else comment () )
      | _               => ( comment () )

end
