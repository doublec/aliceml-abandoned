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

#ifndef __EMULATOR__IO_HANDLER_HH__
#define __EMULATOR__IO_HANDLER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/IOHandler.hh"
#endif

class IOHandler {
public:
  static void Init();
  static void Poll();
  static void Block();
};

#endif
