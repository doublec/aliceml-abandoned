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

#ifndef __ALICE__ALICE_LANGUAGE_LAYER_HH__
#define __ALICE__ALICE_LANGUAGE_LAYER_HH__

#if defined(INTERFACE)
#pragma interface "alice/AliceLanguageLayer.hh"
#endif

class TagVal;

typedef word (*concrete_constructor)(TagVal *);

class DllExport AliceLanguageLayer {
public:
  class DllExport TransformNames {
  public:
    static word primitiveValue;
    static word primitiveFunction;
    static word function;
    static word constructor;
    static word uniqueString;
  };

  static word remoteCallback;

  static concrete_constructor concreteCodeConstructor;

  static void Init();
};

#endif
