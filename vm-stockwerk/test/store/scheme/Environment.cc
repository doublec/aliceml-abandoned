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
#include <cstdio>
#include "Nodes.hh"
#include "Parser.hh"
#include "Environment.hh"

// Environment Class Variables
word Environment::closures[MAX_CLOSURE_DEPTH];
word Environment::semi_locals[MAX_CLOSURE_DEPTH];
int Environment::top;

// Environment Internal Methods
u_int Environment::Length(word l) {
  u_int len = 0;

  while (l != Store::IntToWord(0)) {
    len++;
    l = ConsCell::FromWord(l)->Cdr();
  }
  return len;
}

int Environment::SearchSemi(u_int name) {
  int l;
  
  if ((l = SearchFrame(semi_locals[top], name)) >= 0) {
    return l;
  }
  else {
    int cur_f = (top - 1);

    while (cur_f >= 0) {
      if ((l = SearchFrame(closures[cur_f], name)) >= 0) {
	Block *node = IdNode::New(name, l, (top - cur_f));

	semi_locals[top] = ConsCell::New(node->ToWord(), semi_locals[top])->ToWord();
	return (Length(semi_locals[top]) - 1);
      }
      cur_f--;
    }
    return -1;
  }
}

// Environment Public Methods
void Environment::Reset() {
  top            = 0;
  closures[0]    = Store::IntToWord(0);
  semi_locals[0] = Store::IntToWord(0);
}

int Environment::SearchFrame(word frame, u_int name) {
  int l = (Length(frame) - 1);
  int i = 0;

  while (frame != Store::IntToWord(0)) {
    ConsCell *cell = ConsCell::FromWord(frame);
    IdNode *node   = IdNode::FromWord(cell->Car());
 
    if (node->GetName() == name) {
      return (l - i);
    }
    i++;
    frame = cell->Cdr();
  }
  return -1;
}

void Environment::SearchId(u_int name, VarType *type, u_int *id) {
  int l;
  
  if ((top >= 0) && ((l = SearchFrame(closures[top], name)) >= 0)) {
    *type = T_LOCAL_VAR;
    *id   = l;
  }
  else if ((l = SearchSemi(name)) >= 0) {
    *type = T_SEMI_LOCAL_VAR;
    *id   = l;
  }
  else if ((l = SearchGlobal(name)) >= 0) {
    *type = T_GLOBAL_VAR;
    *id   = SearchGlobal(name);
  }
  else {
    std::fprintf(stderr, "line %d: unbound variable `%s'\n", Parser::line, AtomToString(name));
    if (Parser::interactive) {
      longjmp(Parser::buf, 0);
    }
    else {
      std::exit(0);
    }
  }
}
