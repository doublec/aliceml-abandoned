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
  char *p = INVALID_POINTER;
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
  *p = (char) 0;
}
#endif

// Internal Helper Functions
static inline void Show(const char *s) {
  std::printf(s);
  std::fflush(stdout);
}

static inline void InitStore() {
  u_int limits[STORE_GENERATION_NUM];

  limits[0]             = 1 * STORE_MEMCHUNK_SIZE;
  limits[1]             = 2 * STORE_MEMCHUNK_SIZE;
  limits[2] = limits[3] = 5 * STORE_MEMCHUNK_SIZE;
  Store::InitStore(limits, 75, 20);
}

// Public Functions
u_int RegisterAtom(const char *s) {
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

  Parser::interactive = 1;

  setjmp(Parser::buf);
  fn = INVALID_POINTER;

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
