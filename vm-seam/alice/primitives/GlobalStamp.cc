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

typedef unsigned char u_char;

static int counter = 0;

DEFINE2(GlobalStamp_compare) {
  Block *p = Store::WordToBlock(x0);
  if (p == INVALID_POINTER) {
    REQUEST(x0);
  }
  Block *q = Store::WordToBlock(x1);
  if (q == INVALID_POINTER) {
    REQUEST(x1);
  }
  // Both are Blocks
  if (p->GetLabel() == CHUNK_LABEL) {
    if (q->GetLabel() == CHUNK_LABEL) {
      Chunk *pc = (Chunk *) p;
      Chunk *qc = (Chunk *) q;
      u_int pl  = pc->GetSize();
      u_int ql  = qc->GetSize();
      if (pl < ql) {
	RETURN_INT(2); // LESS
      }
      else if (pl > ql) {
	RETURN_INT(1); // GREATER
      }
      else {
	switch (memcmp(pc->GetBase(), qc->GetBase(), ql)) {
	case -1:
	  RETURN_INT(2); // LESS
	case 0:
	  RETURN_INT(0); // EQUAL
	case 1:
	  RETURN_INT(1); // GREATER
	default:
	  Assert(0);
	  RETURN_INT(0);
	}
      }
    }
    else {
      RETURN_INT(1); // GREATER
    }
  }
  else {
    if (q->GetLabel() == CHUNK_LABEL) {
      RETURN_INT(2); // LESS
    }
    else {
      // GUID comparision: to be done
      Tuple *pt = (Tuple *) p;
      Tuple *qt = (Tuple *) q;
      u_int pl = Store::WordToInt(pt->Sel(1));
      u_int ql = Store::WordToInt(qt->Sel(1));
      if (pl < ql) {
	RETURN_INT(2); // LESS
      }
      else if (pl > ql) {
	RETURN_INT(1); // GREATER
      }
      else {
	RETURN_INT(0); // EQUAL
      }
    }
  }
} END

DEFINE1(GlobalStamp_fromString) {
  DECLARE_STRING(name, x0);
  RETURN(x0);
} END

DEFINE1(GlobalStamp_hash) {
  Block *p = Store::WordToBlock(x0);
  if (p == INVALID_POINTER) {
    REQUEST(x0);
  }
  if (p->GetLabel() == CHUNK_LABEL) {
    Chunk *pc  = (Chunk *) p;
    u_int size = p->GetSize();
    if (size == 0) {
      RETURN_INT(0);
    }
    u_char *pb = (u_char *) pc->GetBase();
    RETURN_INT(pb[0] * pb[size - 1]); 
  }
  // GUID Hashing to be done
  Tuple *pt = (Tuple *) p;
  RETURN_INT(Store::WordToInt(pt->Sel(1)));
} END

DEFINE0(GlobalStamp_new) {
  Tuple *t = Tuple::New(2);
  t->Init(0, Scheduler::vmGUID);
  t->Init(1, Store::IntToWord(counter++));
  RETURN(t->ToWord());
} END

DEFINE1(GlobalStamp_toString) {
  Block *p = Store::WordToBlock(x0);
  if (p == INVALID_POINTER) {
    REQUEST(x0);
  }
  if (p->GetLabel() == CHUNK_LABEL) {
    RETURN(x0);
  }
  Tuple *pt = (Tuple *) p;
  //--** not elegant: string is traversed twice
  static char buf[20];
  std::sprintf(buf, "%u", Store::WordToInt(pt->Sel(1)));
  RETURN(String::New(buf)->ToWord());
} END

void PrimitiveTable::RegisterGlobalStamp() {
  Register("GlobalStamp.compare", GlobalStamp_compare, 2);
  Register("GlobalStamp.fromString", GlobalStamp_fromString, -1);
  Register("GlobalStamp.hash", GlobalStamp_hash, -1);
  Register("GlobalStamp.new", GlobalStamp_new, 0);
  Register("GlobalStamp.toString", GlobalStamp_toString, -1);
}
