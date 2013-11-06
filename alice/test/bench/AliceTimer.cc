//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "Alice.hh"

static double startTime;

DEFINE0(Timer_start) {
#if defined(STORE_NOGCBENCH)
  Store::ResetTime();
#endif
  startTime = Time::GetElapsedMicroseconds();
  RETURN_UNIT;
} END

DEFINE0(Timer_check) {
  double curTime = Time::GetElapsedMicroseconds();
#if defined(STORE_NOGCBENCH)
  double result = (curTime - startTime - Store::ReadTime()) / 1000.0;
#else
  double result = (curTime - startTime) / 1000.0;
#endif
  RETURN(Store::IntToWord(static_cast<int>(result)));
} END

word InitComponent() {
  Record *record = Record::New(2);
  INIT_STRUCTURE(record, "AliceTimer", "start",
		 Timer_start, 0);
  INIT_STRUCTURE(record, "AliceTimer", "check",
		 Timer_check, 0);
  RETURN_STRUCTURE("AliceTimer$", record);
}
