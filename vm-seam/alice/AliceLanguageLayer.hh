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

#ifndef __ALICE__ALICE_LANGUAGE_LAYER_HH__
#define __ALICE__ALICE_LANGUAGE_LAYER_HH__

#if defined(INTERFACE)
#pragma interface "alice/AliceLanguageLayer.hh"
#endif

#include "alice/Base.hh"

class TagVal;

typedef word (*concrete_constructor)(TagVal *);

class AliceDll AliceLanguageLayer {
public:
  class AliceDll TransformNames {
  public:
    static word primitiveValue;
    static word primitiveFunction;
    static word function;
    static word constructor;
    static word uniqueString;
    static word bigInteger;
    static word unsafeMap;
  };

  static word aliceHome;
  static word rootUrl;
  static word commandLineArguments;
  static word remoteCallback;
  static word undefinedValue;

  static concrete_constructor concreteCodeConstructor;

  static void Init(const char *home, int argc, const char *argv[]);
};

#endif
