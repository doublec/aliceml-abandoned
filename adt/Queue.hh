//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ADT__QUEUE_HH__
#define __ADT__QUEUE_HH__

#pragma interface "scheduler/Queue.hh"

#include "store/store.hh"

class Queue: private Block {
public:
  using Block::ToWord;

  static Queue *New(); //--** implement
  static Queue *FromWord(word w); //--** implement

  void Enqueue(word w); //--** implement
  bool IsEmpty(); //--** implement
  word Dequeue(); //--** implement
};

#endif __ADT__QUEUE_HH__
