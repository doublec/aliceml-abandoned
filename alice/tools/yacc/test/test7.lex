structure P = Parse
type lexresult = P.token option * int * int
val linenum = ref 1
val error = fn x => TextIO.print(x ^ "\n")
val eof = fn () => (NONE, !linenum, !linenum)
%%
%structure Lexer
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (linenum:=(!linenum+1); lex());
{ws}+    => (lex());
"\\"     => (SOME P.LAM,!linenum,!linenum);
{alpha}+ => (SOME (P.VAR yytext), !linenum,!linenum);
"("      => (SOME P.LPAR, !linenum, !linenum);
")"      => (SOME P.RPAR, !linenum, !linenum);
"."      => (SOME P.DOT, !linenum, !linenum);
.        => (error ("lexer: ignoring bad character "^yytext); lex());
