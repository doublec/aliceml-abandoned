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

static const u_int envSize = 19;

class Environment: private Array {
private:
  static const int ID_POS = 1;
  static const int VALUE_POS = 2;
  static const int CDR_POS = 3;
  static const u_int ENTRY_SIZE = 3;
public:
  using Array::ToWord;

  static Environment *New(u_int size) {
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
