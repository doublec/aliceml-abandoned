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

#include "alice/primitives/Authoring.hh"

DEFINE1(UnsafeAddr_addr) {
  Block *b = Store::WordToBlock(x0);
  if (b != INVALID_POINTER)
    RETURN(Store::UnmanagedPointerToWord(b));
  Transient *t = Store::WordToTransient(x0);
  if (t != INVALID_POINTER)
    RETURN(Store::UnmanagedPointerToWord(t));
  RETURN_INT(Store::WordToInt(x0));
} END

word UnsafeAddr() {
  Record *record = Record::New(1);
  INIT_STRUCTURE(record, "UnsafeAddr", "addr",
		 UnsafeAddr_addr, 1, true);
  RETURN_STRUCTURE("UnsafeAddr$", record);
}
