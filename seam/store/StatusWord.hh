//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__STATUS_WORD_HH__
#define __STORE__STATUS_WORD_HH__

#if defined(INTERFACE)
#pragma interface "store/StatusWord.hh"
#endif

#include "store/Base.hh"
#include "store/Types.hh"

class SeamDll StatusWord {
protected:
  static u_int status;
public:
  static void Init() {
    status = 0;
  } 
  static void SetStatus(u_int mask) {
    status |= mask;
  }
  static void ClearStatus() {
    status = 0;
  }
  static void ClearStatus(u_int mask) {
    status &= ~mask;
  }
  static u_int GetStatus() {
    return status;
  }
  static u_int GetStatus(u_int mask) {
    return status & mask;
  }
};

#endif
