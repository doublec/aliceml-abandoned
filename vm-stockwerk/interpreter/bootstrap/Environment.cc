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

#include "Environment.hh"

void Environment::Add(word id, word value) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetSize();
  Array *newEntry = Array::New(ENTRY_SIZE);
  newEntry->InitArg(ID_POS, id);
  newEntry->InitArg(VALUE_POS, value);
  newEntry->InitArg(CDR_POS, GetArg(index));
  SetArg(index, newEntry->ToWord());
}

word Environment::Lookup(word id) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetSize();
  Array *entry = Array::FromWord(GetArg(index));
  while (entry->GetArg(ID_POS) != id)
    entry = Array::FromWord(entry->GetArg(CDR_POS));
  return entry->GetArg(VALUE_POS);
}

void Environment::Kill(word id) {
  int stamp = Store::WordToInt(id);
  int index = stamp % GetSize();
  Array *entry = Array::FromWord(GetArg(index));
  Assert(entry != INVALID_POINTER);
  while (entry->GetArg(ID_POS) != id)
    entry = Array::FromWord(entry->GetArg(CDR_POS));
  entry->SetArg(CDR_POS, entry->GetArg(CDR_POS));
}
