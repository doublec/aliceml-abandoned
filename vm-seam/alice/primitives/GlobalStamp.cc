//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "alice/Guid.hh"
#include "alice/Authoring.hh"

static s_int counter = 0;

#define DECLARE_GLOBAL_STAMP(globalStamp, x)				\
  Block *globalStamp = Store::WordToBlock(x);				\
  if (globalStamp == INVALID_POINTER) { REQUEST(x); } else {};		\
  Assert(globalStamp->GetLabel() == CHUNK_LABEL ||			\
	 globalStamp->GetLabel() == TUPLE_LABEL);

DEFINE2(GlobalStamp_compare) {
  DECLARE_GLOBAL_STAMP(globalStamp1, x0);
  DECLARE_GLOBAL_STAMP(globalStamp2, x1);
  if (globalStamp1->GetLabel() == CHUNK_LABEL) {
    if (globalStamp2->GetLabel() == CHUNK_LABEL) { // compare two strings
      String *string1 = STATIC_CAST(String *, globalStamp1);
      String *string2 = STATIC_CAST(String *, globalStamp2);
      u_int length1 = string1->GetSize();
      u_int length2 = string2->GetSize();
      u_int length = length1 < length2? length1: length2;
      int result =
	std::memcmp(string1->GetValue(), string2->GetValue(), length);
      if (result < 0) {
	RETURN_INT(Types::LESS);
      } else if (result > 0) {
	RETURN_INT(Types::GREATER);
      } else if (length1 < length2) {
	RETURN_INT(Types::LESS);
      } else if (length1 > length2) {
	RETURN_INT(Types::GREATER);
      } else {
	RETURN_INT(Types::EQUAL);
      }
    } else {
      RETURN_INT(Types::GREATER);
    }
  } else {
    if (globalStamp2->GetLabel() == CHUNK_LABEL) {
      RETURN_INT(Types::LESS);
    } else {
      Tuple *tuple1 = STATIC_CAST(Tuple *, globalStamp1);
      Tuple *tuple2 = STATIC_CAST(Tuple *, globalStamp2);
      Guid *guid1 = Guid::FromWordDirect(tuple1->Sel(0));
      Guid *guid2 = Guid::FromWordDirect(tuple2->Sel(0));
      int result = Guid::Compare(guid1, guid2);
      if (result < 0) {
	RETURN_INT(Types::LESS);
      } else if (result > 0) {
	RETURN_INT(Types::GREATER);
      }
      u_int counter1 = Store::DirectWordToInt(tuple1->Sel(1));
      u_int counter2 = Store::DirectWordToInt(tuple2->Sel(1));
      if (counter1 < counter2) {
	RETURN_INT(Types::LESS);
      } else if (counter1 > counter2) {
	RETURN_INT(Types::GREATER);
      } else {
	RETURN_INT(Types::EQUAL);
      }
    }
  }
} END

DEFINE1(GlobalStamp_fromString) {
  DECLARE_STRING(name, x0);
  RETURN(x0);
} END

DEFINE1(GlobalStamp_hash) {
  DECLARE_GLOBAL_STAMP(globalStamp, x0);
  if (globalStamp->GetLabel() == CHUNK_LABEL) {
    String *string = STATIC_CAST(String *, globalStamp);
    u_int size = string->GetSize();
    if (size == 0) {
      RETURN_INT(0);
    }
    u_char *value = string->GetValue();
    RETURN_INT(value[0] * value[size - 1]);
  } else {
    Tuple *tuple = STATIC_CAST(Tuple *, globalStamp);
    Guid *guid = Guid::FromWordDirect(tuple->Sel(0));
    s_int counter = Store::DirectWordToInt(tuple->Sel(1));
    RETURN_INT(guid->Hash() ^ counter);
  }
} END

DEFINE0(GlobalStamp_new) {
  //--** handle counter overflow?
  RETURN2(Guid::vmGuid, Store::IntToWord(counter++));
} END

DEFINE1(GlobalStamp_toString) {
  DECLARE_GLOBAL_STAMP(globalStamp, x0);
  if (globalStamp->GetLabel() == CHUNK_LABEL) {
    RETURN(globalStamp->ToWord());
  } else {
    Tuple *tuple = STATIC_CAST(Tuple *, globalStamp);
    static char buf[20];
    std::sprintf(buf, "%u", Store::DirectWordToInt(tuple->Sel(1)));
    RETURN(String::New(buf)->ToWord());
  }
} END

void PrimitiveTable::RegisterGlobalStamp() {
  Register("GlobalStamp.compare", GlobalStamp_compare, 2);
  Register("GlobalStamp.fromString", GlobalStamp_fromString, 1);
  Register("GlobalStamp.hash", GlobalStamp_hash, 1);
  Register("GlobalStamp.new", GlobalStamp_new, 0, 2);
  Register("GlobalStamp.toString", GlobalStamp_toString, 1);
}
