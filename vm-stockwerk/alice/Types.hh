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

#ifndef __ALICE__TYPES_HH__
#define __ALICE__TYPES_HH__

#if defined(INTERFACE)
#pragma interface "alice/Types.hh"
#endif

class Types {
public:
  // Bool.bool
  enum { _false, _true };
  // General.order
  enum { EQUAL, GREATER, LESS };
  // Option.option
  enum { NONE, SOME };
  // List.list
  enum { cons, nil };
  // Future.status
  enum { DETERMINED, FAILED, FUTURE };
  // Component.component
  enum { EVALUATED, UNEVALUATED };
  enum { inf1, mod };                   // EVALUATED labels
  enum { body, imports, inf2 };         // UNEVALUATED labels
  // Config.platform
  enum { Config_UNIX, Config_WIN32 };
  // Label.lab
  enum { ALPHA, NUM };
  // Name.name
  enum { ExId };
};

#endif
