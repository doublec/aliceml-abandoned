//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__SIGNAL_HANDLER_HH__
#define __GENERIC__SIGNAL_HANDLER_HH__

#if defined(INTERFACE)
#pragma implementation "generic/SignalHandler.hh"
#endif

#include <signal.h>

#if defined(__MINGW32__) || defined(_MSC_VER)
#define SIGALRM 0
#endif

#define SIGNAL_HANDLER_SIGNAL_ARRIVED_STATUS 2

class SignalHandler {
protected:
  static void PushCall(word closure, int signal);
  static void CheckTimerEvents();
public:
  // SignalHandler Static Constructor
  static void Init();
  // SignalHandler Methods
  static u_int SignalStatus() {
    return (1 << SIGNAL_HANDLER_SIGNAL_ARRIVED_STATUS);
  }
  static void BlockSignals();
  static void UnblockSignals();
  static void Register(int signal, word closure, u_int delay = 0);
  static bool PendingTimerEvents();
  static void HandlePendingSignals();
};

#endif
