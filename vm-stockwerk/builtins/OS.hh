#ifndef __OS_HH__
#define __OS_HH__

namespace Builtins {
  namespace OS {
    namespace Process {
      word system(word a);
      word exit(word a);
      word getEnv(word a);
    }
  }
}

#endif
