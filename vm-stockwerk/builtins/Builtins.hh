#ifndef __BUILTINS_HH__
#define __BUILTINS_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "Array.hh"
#include "Char.hh"
#include "CommandLine.hh"
#include "Future.hh"
#include "General.hh"
#include "Hole.hh"
#include "Int.hh"
#include "Math.hh"
#include "OS.hh"
#include "Real.hh"
#include "String.hh"
#include "TextIO.hh"
#include "Thread.hh"
#include "Unsafe.hh"
#include "Vector.hh"
#include "Word.hh"

namespace Builtins {
  word opeq(word a, word b);
  word opnoteq(word a, word b);
}

#include "Prebound.hh"

#endif
