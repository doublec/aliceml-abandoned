//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

static int Compare(word x0, word x1) {
  Block *a = Store::WordToBlock(x0);
  Block *b = Store::WordToBlock(x1);
  if (a == INVALID_POINTER) { // x0 is Transient or int
    s_int i = Store::WordToInt(x0);
    if (i == INVALID_INT) { // x0 is transient
      Scheduler::currentData = x0;
      return -1;
    }
    // x0 is int
    if (b != INVALID_POINTER) // x1 is Block
      return 0;
    // x1 is Transient or int
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::currentData = x1;
      return -1;
    }
    return (i == j);
  }
  if (b == INVALID_POINTER) { // x1 is Transient or int, but x0 is a block
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::currentData = x1;
      return -1;
    }
    return 0;
  }
  // from here, both x0 and x1 are blocks
  BlockLabel label = a->GetLabel();
  if (label != b->GetLabel())
    return 0;
  switch (label) {
  case TUPLE_LABEL:
  case Alice::ConVal:
  case Alice::Vector:
    {
    structural_equality:
      u_int size = a->GetSize();
      if (b->GetSize() != size)
	return 0;
      for (u_int i = 0; i < size; i++) { // request left-to-right
	int result = Compare(a->GetArg(i), b->GetArg(i));
	if (result != 1)
	  return result;
      }
      return 1;
    }
  case CHUNK_LABEL:
    {
      Chunk *ac = static_cast<Chunk *>(a);
      Chunk *bc = static_cast<Chunk *>(b);
      u_int size = ac->GetSize();
      return bc->GetSize() == size &&
	!std::memcmp(ac->GetBase(), bc->GetBase(), size);
    }
  default:
    if (Alice::IsTag(label)) {
      goto structural_equality;
    } else // fall back to identity-based equality
      return (a == b);
  }
}

DEFINE2(opeq) {
  int result = Compare(x0, x1);
  if (result == -1) {
    PUSH_PRIM_SELF();
    return Worker::REQUEST;
  } else {
    RETURN_BOOL(result);
  }
} END

DEFINE2(opnoteq) {
  int result = Compare(x0, x1);
  if (result == -1) {
    PUSH_PRIM_SELF();
    return Worker::REQUEST;
  } else {
    RETURN_BOOL(!result);
  }
} END

void PrimitiveTable::RegisterUnqualified() {
  Register("=", opeq, 2);
  Register("<>", opnoteq, 2);
}
