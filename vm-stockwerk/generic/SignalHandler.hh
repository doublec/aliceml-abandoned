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

#include <csignal>
#include "store/Store.hh"

//--** avoid magic constant:
#define SIGNAL_HANDLER_SIGNAL_ARRIVED_STATUS 2

class Future;

class DllExport SignalHandler {
public:
  // SignalHandler Static Constructor
  static void Init();
  // SignalHandler Methods
  static u_int SignalStatus() {
    return (1 << SIGNAL_HANDLER_SIGNAL_ARRIVED_STATUS);
  }
  static u_int GetSignalStatus() {
    return StatusWord::GetStatus(SignalHandler::SignalStatus());
  }
  static void RegisterSignal(int signal, word closure);
  static Future *RegisterAlarm(u_int milliseconds);
  static void HandlePendingSignals();
};

#endif
