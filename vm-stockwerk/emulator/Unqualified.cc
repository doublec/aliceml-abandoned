//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "emulator/Authoring.hh"

 // NON-ABSTRACT TASK STACK USE
static int Compare(word x0, word x1) {
  Block *a = Store::WordToBlock(x0);
  Block *b = Store::WordToBlock(x1);
  if (a == INVALID_POINTER) { // a is Transient or int
    int i = Store::WordToInt(x0);
    if (i == INVALID_INT) {
      Scheduler::currentData = x0;
      return -1;
    }
    if (b != INVALID_POINTER) { // b is TagVal
      return 0;
    }
    int j = Store::WordToInt(x1);
    if (j == INVALID_INT) {
      Scheduler::currentData = x1;
      return -1;
    }
    return (i == j);
  }
  if (b == INVALID_POINTER) { // b is Transient or int, but a is a block
    int j = Store::WordToInt(x1);
    if (j == INVALID_INT) {
      Scheduler::currentData = x1;
      return -1;
    }
    return 0;
  }
  // from here, both a and b are blocks
  BlockLabel label = a->GetLabel();
  switch (label) {
  case TUPLE_LABEL:
  case Alice::ConVal:
  case Alice::Vector:
    {
      u_int size = a->GetSize();
      if (b->GetSize() != size)
	return 0;
      for (u_int i = 1; i <= size; i++) {
	int result = Compare(a->GetArg(i), b->GetArg(i));
	if (result != 1) {
	  return result;
	}
      }
    }
    return 1;
  case CHUNK_LABEL:
    {
      u_int size = a->GetSize();
      if (b->GetSize() != size)
	return 0;
      for (u_int i = 1; i <= size; i++)
	if (a->GetArg(i) != b->GetArg(i))
	  return 0;
      return 1;
    }
  default:
    if (label == b->GetLabel()) {
      return (a == b);
    }
  }
  return 0;
}

DEFINE2(opeq) {
  int result = Compare(x0, x1);
  if (result == -1) {
    return Interpreter::REQUEST;
  }
  else {
    RETURN_BOOL(result);
  }
} END

DEFINE2(opnoteq) {
  int result = Compare(x0, x1);
  if (result == -1) {
    return Interpreter::REQUEST;
  }
  else {
    RETURN_BOOL(!result);
  }
} END

void PrimitiveTable::RegisterUnqualified() {
  Register("=", opeq, 2);
  Register("<>", opnoteq, 2);
}
