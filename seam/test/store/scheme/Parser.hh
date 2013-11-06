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
#include <setjmp.h>
#include "Nodes.hh"

// export node type to BISON and FLEX
#define YYSTYPE word

// BISON and FLEX stuff
extern FILE *yyin;
extern int yylex(void);
extern int yyparse(void);
extern void yyerror(char *s);
extern void flexerror(char *s);

// Parser Interface
class Parser {
public:
  static word tree;
  static int line;
  static int interactive;
  static jmp_buf buf;

  static void Parse(FILE *file);
};

#endif __TEST__STORE__SCHEME__PARSER_HH__
