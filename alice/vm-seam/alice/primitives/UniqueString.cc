//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE1(UniqueString_hash) {
  DECLARE_UNIQUE_STRING(uniqueString, x0);
  RETURN_INT(uniqueString->Hash());
} END

DEFINE1(UniqueString_string) {
  DECLARE_UNIQUE_STRING(uniqueString, x0);
  RETURN(uniqueString->ToString()->ToWord());
} END

DEFINE1(UniqueString_unique) {
  DECLARE_STRING(string, x0);
  RETURN(UniqueString::New(string)->ToWord());
} END

void PrimitiveTable::RegisterUniqueString() {
  Register("UniqueString.hash", UniqueString_hash, 1);
  Register("UniqueString.string", UniqueString_string, 1);
  Register("UniqueString.unique", UniqueString_unique, 1);
}
