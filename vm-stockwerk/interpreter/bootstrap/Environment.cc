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

#include "interpreter/bootstrap/Environment.hh"

#define ENTRY_SIZE 3
#define ID_POS 0
#define VALUE_POS 1
#define CDR_POS 2

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
  while (entry->Sub(ID_POS) != id)
    entry = Array::FromWord(entry->Sub(CDR_POS));
  return entry->Sub(VALUE_POS);
}

void Environment::Kill(word id) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetLength();
  Array *entry = Array::FromWord(Sub(index));
  while (entry->Sub(ID_POS) != id)
    entry = Array::FromWord(entry->Sub(CDR_POS));
  entry->Update(CDR_POS, entry->Sub(CDR_POS));
}
