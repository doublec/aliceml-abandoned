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

#ifndef __EMULATOR__DEBUG_HH__
#define __EMULATOR__DEBUG_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Debug.hh"
#endif

class Debug {
public:
  static u_int maxWidth, maxDepth;
  static void Dump(word x);
};

#endif
