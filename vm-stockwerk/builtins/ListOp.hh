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
#ifndef __LISTOP_HH__
#define __LISTOP_HH__

#if defined(INTERFACE)
#pragma interface
#endif

class ListOp {
public:
  static word Car(Block *a) {
    return a->GetArg(1);
  }
  static word Cdr(Block *a) {
    return a->GetArg(2);
  }
  static word Cons(word car, word cdr) {
    static BlockLabel label = Store::MakeLabel(0);
    Block *cell             = Store::AllocBlock(label, 2);

    cell->InitArg(1, car);
    cell->InitArg(2, cdr);
    return cell->ToWord();
  }
  static int Length(word a) {
    int n = 0;
    
    while (true) {
      Block *cell = Store::WordToBlock(CommonOp::Sync(a));

      if (cell != INVALID_POINTER) {
	n++;
	a = cell->GetArg(2);
      }
      else {
	return n;
      }
    }
  }
};

#endif
