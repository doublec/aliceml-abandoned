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

#if defined(INTERFACE)
#pragma implementation "generic/pickling/PickleInputStream.hh"
#endif
#include "generic/pickling/PickleInputStream.hh"

class PickleInputStreamHandler: public Handler {
public:
  virtual void PrepareForGC(Block *);
  virtual void Finalize(word);
  virtual Block *GetAbstractRepresentation();
};

void PickleInputStreamHandler::PrepareForGC(Block *) {}

void PickleInputStreamHandler::Finalize(word w) {
  std::fclose(PickleInputStream::FromWordDirect(w)->GetFile());
}

Handler *PickleInputStream::handler = new PickleInputStreamHandler();
