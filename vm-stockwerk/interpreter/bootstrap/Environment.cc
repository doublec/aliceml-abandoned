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

#if defined(INTERFACE)
#pragma implementation "interpreter/bootstrap/Environment.hh"
#endif

#include "interpreter/bootstrap/Environment.hh"

static const u_int ENTRY_SIZE = 3;
static const u_int ID_POS = 0;
static const u_int VALUE_POS = 1;
static const u_int CDR_POS = 2;

void Environment::Add(word id, word value) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetLength();
  Array *newEntry = Array::New(ENTRY_SIZE);
  newEntry->Init(ID_POS, id);
  newEntry->Init(VALUE_POS, value);
  newEntry->Init(CDR_POS, Sub(index));
  Update(index, newEntry->ToWord());
}

word Environment::Lookup(word id) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetLength();
  Array *entry = Array::FromWord(Sub(index));
  while (entry->Sub(ID_POS) != id) {
    entry = Array::FromWord(entry->Sub(CDR_POS));
    Assert(entry != INVALID_POINTER);
  }
  return entry->Sub(VALUE_POS);
}

void Environment::Kill(word id) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetLength();
  Array *entry = Array::FromWord(Sub(index));
  while (entry->Sub(ID_POS) != id) {
    entry = Array::FromWord(entry->Sub(CDR_POS));
    Assert(entry != INVALID_POINTER);
  }
  entry->Update(CDR_POS, entry->Sub(CDR_POS));
}
