//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2002
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#if defined(INTERFACE)
#pragma implementation "adt/DynamicArray.hh"
#endif

#include "DynamicArray.hh"

// Provides an unbounded array with dynamic resizing

word DynamicArray::INVALID_ARRAY_ELEM;

void DynamicArray::Init() {
  INVALID_ARRAY_ELEM = Store::AllocBlock(DYNARRAY_LABEL, 1)->ToWord();
  RootSet::Add(INVALID_ARRAY_ELEM);
}

DynamicArray *DynamicArray::New(int initialSize) {
  Block *p = Store::AllocMutableBlock(DYNARRAY_LABEL, SIZE);
  Block *a = Store::AllocMutableBlock(MIN_DATA_LABEL, initialSize);
  
  p->InitArg(SIZE_POS, initialSize);
  p->InitArg(ARRAY_POS, a->ToWord());
  p->InitArg(INIT_ELEM_POS, INVALID_ARRAY_ELEM);
  return STATIC_CAST(DynamicArray *, p);
}

DynamicArray *DynamicArray::NewInit(int initialSize, word initElem) {
  // Creates a new DynamicArray and initializes it
  // with the initElem
  Block *p = Store::AllocMutableBlock(DYNARRAY_LABEL, SIZE);
  Block *a = Store::AllocMutableBlock(MIN_DATA_LABEL, initialSize);

  p->InitArg(SIZE_POS, initialSize);
  p->InitArg(ARRAY_POS,a->ToWord());
  p->InitArg(INIT_ELEM_POS, initElem);
  
  for (int i=initialSize; i--;) {
    a->InitArg(i, initElem);
  }

  return STATIC_CAST(DynamicArray *, p);
}

DynamicArray *DynamicArray::FromWord(word x) {
  Block *b = Store::WordToBlock(x);
  Assert(b == INVALID_POINTER || b->GetLabel() == DYNARRAY_LABEL);
  return STATIC_CAST(DynamicArray *, b);
}

DynamicArray *DynamicArray::FromWordDirect(word x) {
  Block *b = Store::DirectWordToBlock(x);
  Assert(b->GetLabel() == DYNARRAY_LABEL);
  return STATIC_CAST(DynamicArray *, b);
}

u_int DynamicArray::GetLength() {
  return Store::DirectWordToInt(GetArg(SIZE_POS));
}

void DynamicArray::EnlargeArray(int upTo) {
  // tries to double the array's size
  // if that is not enough (upTo > doubled size),
  // it is resized to upTo+1 (does that make sense?)
  Block *a = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  
  word initElem = GetArg(INIT_ELEM_POS);

  int length = GetLength();
  int newLength = length*2;
  
  if (newLength<=upTo) newLength=upTo+1;

  Block *newA = Store::AllocMutableBlock(MIN_DATA_LABEL, newLength);
  for (int i=length; i--;) {
    newA->InitArg(i, a->GetArg(i));
  }
  if (initElem != INVALID_ARRAY_ELEM) {
    for (int i=length; i<newLength; i++) {
      newA->InitArg(i, initElem);
    }
  }

  ReplaceArg(ARRAY_POS, newA->ToWord());
  ReplaceArg(SIZE_POS, newLength);
}

void DynamicArray::Init(u_int index, word value) {
  u_int length = GetLength();
  if (index>=length) EnlargeArray();
  
  Block *a = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  a->InitArg(index, value);
}

void DynamicArray::Update(u_int index, word value) {
  u_int length = GetLength();    
  if (index>=length) EnlargeArray(index);
  
  Block *a = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  a->ReplaceArg(index, value);
}

word DynamicArray::Sub(u_int index) {
  Block *a = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  u_int length = GetLength();    
  if (index>=length) {
    return INVALID_ARRAY_ELEM;
  } else {
    return a->GetArg(index);
  }
}
