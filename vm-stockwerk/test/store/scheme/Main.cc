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
#include "Interpreter.hh"

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  char *fn = NULL;

  printf("Store based Scheme Interpreter started\n");
  fflush(stdout);

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1);
  }

  Store::InitStore(memLimits);
  Interpreter::Init();

  while (1) {
    FILE *fs = NULL;

    std::printf("> ");
    std::fflush(stdout);
    
    if (fn != NULL) {
      if ((fs = std::fopen(fn, "r")) == NULL) {
	std::fprintf(stderr, "Interpreter::Main: unable to open file `%s'.\n", fn);
	std::exit(0);
      }
      anchor = parse(fs);
      std::fclose(fs);
    }
    else {
      anchor = parse(stdin);
    }
    fn = Interpreter::Interpret(anchor);
    if (fn == NULL) {
#if defined(DEBUG_CHECK)
      Store::MemStat();
#endif
    }
  }
}
