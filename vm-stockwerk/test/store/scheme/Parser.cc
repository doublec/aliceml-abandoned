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
#include "Parser.hh"

word anchor  = (word) 1;
int line_num = 1;

void yyerror(char *s) {
  std::fprintf(stderr, "%s: line %d: %s\n", "stdout", line_num, s);
  std::exit(0);
}

word parse(FILE *file_name) {
  yyin = file_name;
  yyparse();

  return anchor;
}
