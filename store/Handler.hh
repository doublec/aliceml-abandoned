//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__HANDLER_HH__
#define __STORE__HANDLER_HH__

class Handler {
public:
  virtual void PrepareForGC(Block *p) = 0;
  virtual void Finalize(word value) = 0;
};

#endif __STORE__HANDLER_HH__
