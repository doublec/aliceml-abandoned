%{
#include <cstdlib>
#include "Parser.hh"
%}

%token TK_DEFINE
%token TK_BEGIN
%token TK_LET
%token TK_LAMBDA
%token TK_IF
%token TK_OPARENT
%token TK_CPARENT
%token TK_INT
%token TK_ID
%token TK_STRING
%token TK_NIL
%token TK_TAIL
%token TK_SETQ

%%

start:		decls
		{
			$$ = Parser::tree = (ConsCell::FromWord($1)->ToArray(T_DECLARR))->ToWord();
		}
;

declexp:	decl
		{
			$$ = $1;
		}
	|	expr
		{
			$$ = $1;
		}
;

decls:		declexp edecls
		{
			$$ = ConsCell::New($1, $2)->ToWord();
		}
;

edecls:
		{
			$$ = Store::IntToWord(0);
		}
	|	decl edecls
		{
			$$ = ConsCell::New($1, $2)->ToWord();
		}
;

decl:		TK_OPARENT TK_DEFINE TK_ID expr TK_CPARENT
		{
			$$ = DefineNode::New($3, $4)->ToWord();
		}
;

expr:		TK_ID
		{
			$$ = $1;
		}
	|	TK_INT
		{
			$$ = $1;
		}
	|	TK_STRING
		{
			$$ = $1;
		}
	|	TK_NIL
		{
			$$ = $1;
		}
	|	TK_OPARENT TK_IF expr expr expr TK_CPARENT
		{
			$$ = IfNode::New($3, SelectionNode::New($4, $5)->ToWord())->ToWord();
		}
	|	TK_OPARENT TK_LAMBDA idlist expr TK_CPARENT
		{
			word idarr = ConsCell::FromWord($3)->ToArray(T_IDARR)->ToWord();

			$$ = LambdaNode::New(idarr, $4)->ToWord();
		}
	| TK_OPARENT TK_LET TK_OPARENT TK_OPARENT TK_ID expr TK_CPARENT TK_CPARENT expr TK_CPARENT
		{
			$$ = LetNode::New($5, $6, $9)->ToWord();
		}
	|	TK_OPARENT exprlist TK_CPARENT
		{
			word exarr = ConsCell::FromWord($2)->ToArray(T_EXPRARR)->ToWord();

			$$ = ApplicationNode::New(exarr, 0)->ToWord();
		}
	|	TK_OPARENT TK_TAIL exprlist TK_CPARENT
		{
			word exarr = ConsCell::FromWord($3)->ToArray(T_EXPRARR)->ToWord();

			$$ = ApplicationNode::New(exarr, 1)->ToWord();
		}
	|	TK_OPARENT TK_SETQ TK_ID expr TK_CPARENT
		{
			$$ = SetQNode::New($3, $4)->ToWord();
		}
	|	TK_OPARENT TK_BEGIN exprlist TK_CPARENT
		{
			word exarr = ConsCell::FromWord($3)->ToArray(T_EXPRARR)->ToWord();

			$$ = BeginNode::New(exarr)->ToWord();
		}
;

idlist:		TK_NIL
		{
			$$ = Store::IntToWord(0);
		}
	|	TK_OPARENT ridlist TK_CPARENT
		{
			$$ = $2;
		}
;

ridlist:	TK_ID
		{
			$$ = ConsCell::New($1, Store::IntToWord(0))->ToWord();
		}
	|	TK_ID ridlist
		{
			$$ = ConsCell::New($1, $2)->ToWord();
		}
;

exprlist:	expr
		{
			$$ = ConsCell::New($1, Store::IntToWord(0))->ToWord();
		}
	|	expr exprlist
		{
			$$ = ConsCell::New($1, $2)->ToWord();
		}
;
