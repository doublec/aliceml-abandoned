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
#include "Nodes.hh"
#include "Parser.hh"

// Parser Error Handler
void yyerror(char *s) {
  std::fprintf(stderr, "line %d: %s\n", Parser::line, s);
  std::exit(0);
}

// Public Parser Interface
word Parser::tree;
int Parser::line;

void Parser::Parse(FILE *file) {
  line = 1;
  yyin = file;
  yyparse();
}
