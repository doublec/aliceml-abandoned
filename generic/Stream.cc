//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002-2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Stream.hh"
#endif

#include "generic/Stream.hh"
#include "generic/Transients.hh"
#include "generic/Debug.hh"


void Stream::SendEvent(word event) {
  Future *future = Future::New();
  EntryList *entry = EntryList::New(event, future->ToWord());
  Future *old = 
    STATIC_CAST(Future *, Store::DirectWordToTransient(GetArg(STREAM_POS)));
  
  old->ScheduleWaitingThreads();
  old->Become(REF_LABEL, entry->ToWord());
  ReplaceArg(STREAM_POS, future->ToWord());
  
}

word Stream::GetEntryList() {
  return GetArg(STREAM_POS);
}
