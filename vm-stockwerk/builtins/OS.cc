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
#include <cstdlib>

#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"
#include "OS.hh"

namespace Builtins {
  namespace OS {
    namespace Process {
      word system(word a) {
	String *s = (String *) Store::WordToBlock(CommonOp::Sync(a));
	char *buf = s->GetValue();
	buf[s->GetLength()] = 0x00;
	return Store::IntToWord(std::system(buf));
      }
      word exit(word a) {
	std::exit(Store::WordToInt(CommonOp::Sync(a)));
	return Store::IntToWord(0); // to be determined
      }
      word getEnv(word a) {
	String *s = (String *) Store::WordToBlock(CommonOp::Sync(a));
	char *buf = s->GetValue();
	buf[s->GetLength()] = 0x00;
	return String::New(std::getenv(buf))->ToWord();
      }
    }
  }
}
