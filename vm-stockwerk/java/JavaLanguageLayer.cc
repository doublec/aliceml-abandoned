//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "java/JavaLanguageLayer.hh"
#endif

#include "java/ThrowWorker.hh"
#include "java/NativeMethodTable.hh"
#include "java/ClassLoader.hh"
#include "java/ByteCodeInterpreter.hh"
#include "java/JavaLanguageLayer.hh"

void JavaLanguageLayer::Init() {
  ClassLoader::Init();
  PrimitiveType::Init();
  Class::Init();
  ClassObject::Init();
  JavaString::Init();
  ThrowWorker::Init();
  NativeMethodTable::Init();
  ByteCodeInterpreter::Init();
}
