//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __UNPICKLER_HH__
#define __UNPICKLER_HH__

#if defined(INTERFACE)
#pragma interface "Unpickler.hh"
#endif

#include "adt/Stack.hh"
#include "scheduler/Interpreter.hh"

class HashTable;

typedef word (*transformer)(word);

class TransformerInterpreter: public Interpreter {
private:
  transformer function;
public:
  TransformerInterpreter(transformer f): function(f) {}

  virtual ConcreteCode *Prepare(word abstractCode);
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PopFrame(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

typedef unsigned char byte;

class PickleInputStream {
public:
  virtual byte ReadByte() = 0;
};

class Unpickler: private Stack {
private:
  enum Tag {
    POSINT    = 0,
    NEGINT    = 1,
    CHUNK     = 2,
    BLOCK     = 3,
    CLOSURE   = 4,
    REF       = 5,
    TRANSFORM = 6
  };

  PickleInputStream *inputStream;
  HashTable *transformers;
  Stack *refs;

  u_int ReadInt();
public:
  Unpickler(PickleInputStream *stream, HashTable *table) {
    inputStream = stream;
    transformers = table;
    refs = Stack::New(8);
  }

  word Read();
};

#endif __UNPICKLER_HH__
