//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//// Copyright:
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
#include "Decorator.hh"
#include "Interpreter.hh"

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

// Internal Helper Functions
static inline void Show(char *s) {
  std::printf(s);
  std::fflush(stdout);
}

static inline void InitStore() {
  u_int limits[STORE_GENERATION_NUM];
  
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    limits[i] = (i + 1);
  }
  Store::InitStore(limits);
}

// Public Functions
u_int RegisterAtom(char *s) {
  return Interpreter::RegisterAtom(s);
}

char *AtomToString(u_int name) {
  return Interpreter::AtomToString(name);
}

u_int GlobalAlloc(u_int name) {
  return Interpreter::GlobalAlloc(name);
}

int SearchGlobal(u_int name) {
  return Interpreter::SearchGlobal(name);
}

int main(void) {
  char *fn = INVALID_POINTER;
  FILE *fs = INVALID_POINTER;
  
  InitStore();
  Interpreter::Init();
  Show("Store based Scheme Interpreter started\n");

  while (1) {
    
    if (fn != INVALID_POINTER) {
      if ((fs = std::fopen(fn, "r")) == INVALID_POINTER) {
	std::fprintf(stderr, "Interpreter::Main: unable to open file `%s'.\n", fn);
	std::exit(0);
      }
      Parser::Parse(fs);
      std::fclose(fs);
    }
    else {
      Show("> ");
      Parser::Parse(stdin);
    }
    Environment::Reset();
    Decorator::Decorate(Parser::tree);
    fn = Interpreter::Interpret(Parser::tree);
  }
}
