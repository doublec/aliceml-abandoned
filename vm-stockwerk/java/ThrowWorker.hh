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

#ifndef __JAVA__THROW_WORKER_HH__
#define __JAVA__THROW_WORKER_HH__

#if defined(INTERFACE)
#pragma interface "java/ThrowWorker.hh"
#endif

#include "generic/Worker.hh"
#include "java/Data.hh"

class ThrowWorker: public Worker {
private:
  ThrowWorker() {}
public:
  static ThrowWorker *self;

  static word NoSuchMethodError;

  static void Init();

  static void PushFrame(word exception, JavaString *detailMessage);

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
