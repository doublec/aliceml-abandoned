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
  std::fprintf(stderr, "\nline %d: %s\n", Parser::line, s);
  if (Parser::interactive) {
    longjmp(Parser::buf, 0);
  }
  else {
    std::exit(0);
  }
}

// Flex Error Handler
void flexerror(char *s) {
  std::fprintf(stderr, "\nline %d: unknown character: %s; skipping input\n", Parser::line, s);
  if (Parser::interactive) {
    longjmp(Parser::buf, 0);
  }
  else {
    std::exit(0);
  }
}
// Public Parser Interface
word Parser::tree;
int Parser::line;
int Parser::interactive;
jmp_buf Parser::buf;

void Parser::Parse(FILE *file) {
  line = 1;
  yyin = file;
  yyparse();
}
