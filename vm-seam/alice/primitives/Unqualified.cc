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
      Scheduler::SetCurrentData(x0);
      return -1;
    }
    // x0 is int
    if (b != INVALID_POINTER) // x1 is Block
      {
        // test if x1 is a bigInt
        if (b->GetLabel()==CONCRETE_LABEL) {
          ConcreteRepresentation *cr =
            STATIC_CAST(ConcreteRepresentation *, b);
          if (cr->GetHandler()==PrimitiveTable::gmpHandler) {
            return BigInt::FromWordDirect(cr->Get(0))->compare(i)==0;
          }
        }
        return 0;
      }
    // x1 is Transient or int
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::SetCurrentData(x1);
      return -1;
    }
    return (i == j);
  }
  if (b == INVALID_POINTER) { // x1 is Transient or int, but x0 is a block
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::SetCurrentData(x1);
      return -1;
    } // x1 is int
    // test if x0 is a bigInt
    if (a->GetLabel()==CONCRETE_LABEL) {
      ConcreteRepresentation *cr =
        STATIC_CAST(ConcreteRepresentation *, a);
      if (cr->GetHandler()==PrimitiveTable::gmpHandler) {
        return BigInt::FromWordDirect(cr->Get(0))->compare(j)==0;
      }
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
  case CONCRETE_LABEL:
    {
      ConcreteRepresentation *ac = STATIC_CAST(ConcreteRepresentation *, a);
      ConcreteRepresentation *bc = STATIC_CAST(ConcreteRepresentation *, b);
      if (ac->GetHandler()==PrimitiveTable::gmpHandler &&
          ac->GetHandler()==bc->GetHandler()) {
        BigInt *a = BigInt::FromWordDirect(ac->Get(0));
        BigInt *b = BigInt::FromWordDirect(bc->Get(0));
        return (a->compare(b) == 0);
      } else {
        return (ac==bc);
      }
    }
  case CHUNK_LABEL:
    {
      if (a->IsMutable()) {
	if (b->IsMutable()) {
	  return (a == b);
	} else {
	  return 0;
	}
      }
      Chunk *ac = STATIC_CAST(Chunk *, a);
      Chunk *bc = STATIC_CAST(Chunk *, b);
      u_int size = ac->GetSize();
      return bc->GetSize() == size &&
	!std::memcmp(ac->GetBase(), bc->GetBase(), size);
    }
  default:
    if (a->IsMutable()) {
      if (b->IsMutable()) {
	return (a == b);
      } else {
	return 0;
      }
    }
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
  Register("op=", opeq, 2);
  Register("op<>", opnoteq, 2);
}
