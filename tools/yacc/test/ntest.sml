
(* parser *)

token LPAR | RPAR

datatype T =
    Lf of int
  | Br of T * int
 
rule exp =
    A as skip        => (Lf (Aright)      ) 
  | LPAR, exp, RPAR  => (Br (exp,RPARleft))

parser eval = exp




(* lexer *)

regexp
    formatting = [" "]+

lexer brackets =
    formatting  => ( brackets () )
  | "("         => ( (SOME LPAR, yyline, yycol) )
  | ")"         => ( (SOME RPAR, yyline, yycol) )
  | eof         => ( (NONE, yyline, yycol) )

fun getlex s =
    let
	val lex = brackets (Lexer.fromString s)
    in
	fn x => lex x
    end

fun parse s =
    eval (getlex s)
    handle Lexer.RegMatch s => (print s; raise Match)

