%{
#include "Parser.hh"
#include "scheme.bison.tab.h"

static int StringToInt(char *s) {
  int i;

  sscanf(s, "%d", &i);
  return i;
}

static char *TransformString(char *s) {
  s[strlen(s) - 1] = 0x00;
  return s + 1;
}

%}

%option noyywrap
%option always-interactive

%x init

%%
			BEGIN(init);

<init>"define"		{ yylval = NULL; return TK_DEFINE; }
<init>"let"		{ yylval = NULL; return TK_LET; }
<init>"lambda"		{ yylval = NULL; return TK_LAMBDA; }
<init>"if"		{ yylval = NULL; return TK_IF; }
<init>"("		{ yylval = NULL; return TK_OPARENT; }
<init>")"		{ yylval = NULL; return TK_CPARENT; }
<init>"()"		{
				yylval = NilNode::New()->ToWord();
				return TK_NIL;
			}
<init>("-")*([0-9])+	{
				yylval = IntNode::New(StringToInt(yytext))->ToWord();
				return TK_INT;
			}
<init>"\""[^'\n]*"\""	{
				yylval = StringNode::New(TransformString(yytext))->ToWord();
				return TK_STRING;
			}
<init>([a-zA-Z0-9_?])+	{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"+"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"-"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"*"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"/"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"<"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>">"		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}
<init>"="		{
				yylval = IdNode::New(yytext)->ToWord();
				return TK_ID;
			}


<init>[ \t]+		// Consume whitespaces
<init>[\n]		{ line_num++; }
<init>";"		{ yyterminate(); }
<init>.			{ std::printf("flex: unrecognized character `%s'", yytext); std::exit(0); }

%%
