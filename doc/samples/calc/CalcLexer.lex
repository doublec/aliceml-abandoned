(* Calc -- A alicelex / aliceyacc sample 
 *
 *  Authors: Benedikt Grundmann (bgrund@ps.uni-sb.de) 
 *
 *  $Revision$
 *
 *  Last updated: $Date$ by $Author$
 * 
 *)

import structure Lexer          from "x-alice:/lib/lex/Lexer"
import structure CalcParser     from "CalcParser"

structure CalcLexer =
struct 
    structure P = CalcParser

    (* comment nesting level *)
    val nesting = ref 0

    fun ret (t, yyline, yycol) = let val p = (yyline, yycol) in (SOME t, p, p) end

    regexp 
        digit   = ["0-9"]
    and space   = [" \t\n"]+ 
    and number  = digit+

    lexer lex =
        number      => ( ret (P.NUMBER (valOf (LargeInt.fromString yytext)), yyline, yycol) )
      | "+"         => ( ret (P.PLUS, yyline, yycol) )
      | "-"         => ( ret (P.MINUS, yyline, yycol) )
      | "*"         => ( ret (P.TIMES, yyline, yycol) )
      | "/"         => ( ret (P.DIVIDE, yyline, yycol) )
      | "("         => ( ret (P.LPAR, yyline, yycol) )
      | ")"         => ( ret (P.RPAR, yyline, yycol) )
      | "(*"        => ( comment () )
      | space       => ( lex () )
      | eof         => ( (NONE, (yyline, yycol), (yyline, yycol)) )

    and comment =
        "(*"        => (nesting := !nesting + 1; comment ())
      | "*)"        => (nesting := !nesting - 1;
                        if !nesting = 0 then lex () else comment () )
      | _           => ( comment () )

end 
