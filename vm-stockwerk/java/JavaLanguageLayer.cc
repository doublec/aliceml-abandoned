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

#if defined(INTERFACE)
#pragma implementation "java/JavaLanguageLayer.hh"
#endif

#include "java/ClassLoader.hh"
#include "java/JavaLanguageLayer.hh"
#include "java/ByteCodeInterpreter.hh"

void JavaLanguageLayer::Init() {
  ClassLoader::Init();
  ByteCodeInterpreter::Init();
}
