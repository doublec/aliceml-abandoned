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

#include <cstdio>
#include "emulator/Authoring.hh"

DEFINE2(GlobalStamp_compare) {
  DECLARE_GLOBAL_STAMP(globalStamp1, x0);
  DECLARE_GLOBAL_STAMP(globalStamp2, x1);
  u_int hashCode1 = globalStamp1->GetHashCode();
  u_int hashCode2 = globalStamp2->GetHashCode();
  if (hashCode1 < hashCode2) {
    RETURN_INT(2);   // LESS
  } else if (hashCode1 == hashCode2) {
    RETURN_INT(0);   // EQUAL
  } else { // hashCode1 > hashCode2
    RETURN_INT(1);   // GREATER
  }
} END

DEFINE1(GlobalStamp_fromString) {
  DECLARE_STRING(name, x0);
  RETURN(GlobalStamp::New(name)->ToWord());
} END

DEFINE1(GlobalStamp_hash) {
  DECLARE_GLOBAL_STAMP(globalStamp, x0);
  RETURN_INT(globalStamp->GetHashCode());
} END

DEFINE0(GlobalStamp_new) {
  RETURN(GlobalStamp::New()->ToWord());
} END

DEFINE1(GlobalStamp_toString) {
  DECLARE_GLOBAL_STAMP(globalStamp, x0);
  String *name = globalStamp->GetName();
  if (name == INVALID_POINTER) {
    //--** not elegant: string is traversed twice
    static char buf[20];
    std::sprintf(buf, "%u", globalStamp->GetHashCode());
    RETURN(String::New(buf)->ToWord());
  } else {
    RETURN(name->ToWord());
  }
} END

void PrimitiveTable::RegisterGlobalStamp() {
  Register("GlobalStamp.compare", GlobalStamp_compare, 2);
  Register("GlobalStamp.fromString", GlobalStamp_fromString, -1);
  Register("GlobalStamp.hash", GlobalStamp_hash, -1);
  Register("GlobalStamp.new", GlobalStamp_new, 0);
  Register("GlobalStamp.toString", GlobalStamp_toString, -1);
}
