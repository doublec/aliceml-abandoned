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

#ifndef __ENVIRONMENT_HH__
#define __ENVIRONMENT_HH__

#include "datalayer/alicedata.hh"

class Environment: private Array {
private:
  static const u_int size = 19;
public:
  using Array::ToWord;

  static Environment *New() {
    return static_cast<Environment *>(Array::New(size));
  }
  static Environment *FromWord(word w) {
    return static_cast<Environment *>(Array::FromWord(w));
  }

  void Add(word id, word value);
  word Lookup(word id);
  void Kill(word id);
};

#endif __ENVIRONMENT_HH__
