//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstring>

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "ListOp.hh"
#include "String.hh"

#define MIN(a, b) (((a) > (b)) ? (b) : (a))

static int DoCompare(word a, word b) {
  String *as = (::String *) Store::WordToBlock(CommonOp::Sync(a));
  String *bs = (::String *) Store::WordToBlock(CommonOp::Sync(b));
  int alen   = as->GetLength();
  int blen   = bs->GetLength();
  int val    = std::memcmp(as->GetValue(), bs->GetValue(), MIN(alen, blen)); 

  if (val == 0) {
    if (alen < blen) {
      return -1;
    }
    else if (alen == blen) {
      return 0;
    }
    else {
      return 1;
    }
  }
  else {
    return val;
  }
}

namespace Builtins {
  namespace String {
    word append(word a, word b) {
      ::String *as = (::String *) Store::WordToBlock(CommonOp::Sync(a));
      ::String *bs = (::String *) Store::WordToBlock(CommonOp::Sync(b));
      int alen     = as->GetLength();
      int blen     = bs->GetLength();
      ::String *ns = ::String::New(alen + blen);
      char *cs     = ns->GetValue();

      std::memcpy(cs, as->GetValue(), alen);
      std::memcpy(cs + alen, bs->GetValue(), blen);
      return ns->ToWord();
    }
    word opless(word a, word b) {
      return Store::IntToWord(DoCompare(a, b) == -1);
    }
    word opgreater(word a, word b) {
      return Store::IntToWord(DoCompare(a, b) == 1);
    }
    word oplessEq(word a, word b) {
      return Store::IntToWord(DoCompare(a, b) <= 0);
    }
    word opgreaterEq(word a, word b) {
      return Store::IntToWord(DoCompare(a, b) >= 0);
    }
    word compare(word a, word b) {
      switch (DoCompare(a, b)) {
      case -1:
	return Store::IntToWord(0); // LESS
      case 1:
	return Store::IntToWord(0); // GREATER
      default:
	return Store::IntToWord(0); // EQUAL
      }
    }
    word explode(word a) {
      ::String *as = (::String *) Store::WordToBlock(CommonOp::Sync(a));
      char *base   = as->GetValue();
      word cell    = Store::IntToWord(1);
      
      for (int i = (as->GetLength() - 1); i >= 0; i--) {
	cell = ListOp::Cons(Store::IntToWord((int) base[i]), cell);
      }
      
      return cell;
    }
    word implode(word a) {
      ::String *s = ::String::New(ListOp::Length(a)); 
      char *base  = s->GetValue();
      int i       = 0;

      while (true) {
	Block *cell = Store::WordToBlock(CommonOp::Sync(a));

	if (cell != INVALID_POINTER) {
	  base[i++] = (char) Store::WordToBlock(cell->GetArg(1));
	  a         = cell->GetArg(2);
	}
	else {
	  return s->ToWord();
	}
      }
    }
    word size(word a) {
      return Store::IntToWord(((::String *) Store::WordToBlock(CommonOp::Sync(a)))->GetLength());
    }
    word sub(word a, word b) {
      ::String *as = (::String *) Store::WordToBlock(CommonOp::Sync(a));
      int i        = Store::WordToInt(CommonOp::Sync(b));
      
      if (i < as->GetLength()) {
	return Store::IntToWord((as->GetValue())[i]);
      }
      else {
	return Store::IntToWord(0); // to be determined
      }
    }
    word substring(word a, word b, word c) {
      ::String *as = (::String *) Store::WordToBlock(CommonOp::Sync(a));
      int si       = Store::WordToInt(CommonOp::Sync(b));
      int ei       = Store::WordToInt(CommonOp::Sync(c));
      int len      = as->GetLength();

      if ((si >= 0) && (si < len) && (ei >= 0) && (ei < len) && (ei >= si)) {
	int nlen     = (ei - si + 1);
	::String *ns = ::String::New(nlen);
	
	std::memcpy(ns->GetValue(), as->GetValue() + si, nlen);
	return ns->ToWord();
      }
      else {
	return Store::IntToWord(0); // to be determined
      }
    }
    word str(word a) {
      static char *buf = " ";
      buf[0] = (char) Store::WordToInt(CommonOp::Sync(a));
      return ::String::New(buf, 1)->ToWord();
    }
  }
}
