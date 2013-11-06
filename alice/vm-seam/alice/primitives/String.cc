//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstring>
#include "alice/Authoring.hh"

DEFINE2(String_opconcat) {
  DECLARE_STRING(string1, x0);
  DECLARE_STRING(string2, x1);
  u_int length1 = string1->GetSize();
  u_int length2 = string2->GetSize();
  if (length1 + length2 > ALICE_STRING_MAX_SIZE) {
    RAISE(PrimitiveTable::General_Size);
  }
  String *newString = String::New(length1 + length2);
  u_char *base = newString->GetValue();
  std::memcpy(base, string1->GetValue(), length1);
  std::memcpy(base + length1, string2->GetValue(), length2);
  RETURN(newString->ToWord());
} END

static inline int DoCompare(String *string1, String *string2) {
  u_int length1 = string1->GetSize();
  u_int length2 = string2->GetSize();
  int result = std::memcmp(string1->GetValue(), string2->GetValue(),
			   length1 < length2? length1: length2);
  if (result == 0) {
    if (length1 < length2)
      return -1;
    else if (length1 == length2)
      return 0;
    else
      return 1;
  } else
    return result;
}

#define COMPARISON(name, op)				\
  DEFINE2(name) {					\
    DECLARE_STRING(string1, x0);			\
    DECLARE_STRING(string2, x1);			\
    RETURN_BOOL(DoCompare(string1, string2) op 0);	\
  } END

COMPARISON(String_opless, <)
COMPARISON(String_opgreater, >)
COMPARISON(String_oplessEq, <=)
COMPARISON(String_opgreaterEq, >=)
COMPARISON(String_equal, ==);

DEFINE2(String_compare) {
  DECLARE_STRING(string1, x0);
  DECLARE_STRING(string2, x1);
  int result = DoCompare(string1, string2);
  if (result < 0) {
    RETURN_INT(Types::LESS);
  } else if (result == 0) {
    RETURN_INT(Types::EQUAL);
  } else { // result > 0
    RETURN_INT(Types::GREATER);
  }
} END

DEFINE1(String_hash) {
  DECLARE_STRING(string, x0);
  RETURN_INT(string->Hash());
} END

DEFINE1(String_str) {
  DECLARE_INT(i, x0);
  char c = static_cast<char>(i);
  RETURN(String::New(&c, 1)->ToWord());
} END

void PrimitiveTable::RegisterString() {
  Register("String.^", String_opconcat, 2);
  Register("String.<", String_opless, 2);
  Register("String.>", String_opgreater, 2);
  Register("String.<=", String_oplessEq, 2);
  Register("String.>=", String_opgreaterEq, 2);
  Register("String.equal", String_equal, 2);
  Register("String.compare", String_compare, 2);
  Register("String.hash", String_hash, 1);
  Register("String.str", String_str, 1);
}
