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

#ifndef __INTERPRETER__BOOTSTRAP__ENVIRONMENT_HH__
#define __INTERPRETER__BOOTSTRAP__ENVIRONMENT_HH__

#if defined(INTERFACE)
#pragma interface "interpreter/bootstrap/Environment.hh"
#endif

#include "datalayer/Alice.hh"

class Environment: private Array {
public:
  using Array::ToWord;

  static Environment *New(u_int size) {
    return static_cast<Environment *>(Array::New(size));
  }
  static Environment *FromWord(word w) {
    return static_cast<Environment *>(Array::FromWord(w));
  }

  void Add(word id, word value) {
    Update(Store::WordToInt(id), value);
  }
  word Lookup(word id) {
    return Sub(Store::WordToInt(id));
  }
  void Kill(word id) {
    Update(Store::WordToInt(id), Store::IntToWord(0));
  }
};

#endif __INTERPRETER__BOOTSTRAP__ENVIRONMENT_HH__
