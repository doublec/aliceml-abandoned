//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __TEST__STORE__SCHEME__PARSER_HH__
#define __TEST__STORE__SCHEME__PARSER_HH__

#include <cstdio>
#include "Nodes.hh"

// export node type to BISON and FLEX
#define YYSTYPE word

// ROOT variable containing the complete parse tree
extern YYSTYPE anchor;

// expose current line for error reporting
extern int line_num;

// BISON and FLEX stuff
extern FILE *yyin;
extern int yylex(void);
extern int yyparse(void);
extern void yyerror(char *s);

/* Parser Interface */

extern word parse(FILE *file_name);

#endif __TEST__STORE__SCHEME__PARSER_HH__
