//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PROPERTIES_HH__
#define __GENERIC__PROPERTIES_HH__

#if defined(INTERFACE)
#pragma interface "generic/Properties.hh"
#endif

#include "store/Store.hh"

class DllExport Properties {
public:
  static word aliceHome;
  static word rootUrl;
  static word commandLineArguments;
  static word atExn;

  static void Init(char *home, u_int argc, char *argv[]);
};

#endif
