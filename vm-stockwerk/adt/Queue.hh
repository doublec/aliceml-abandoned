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

#if defined(INTERFACE)
#pragma interface "adt/Queue.hh"
#endif

#include "store/store.hh"
#include <cstring>

#define QueueLabel Store::MakeLabel(0) //--**
#define QueueArrayLabel Store::MakeLabel(0) //--**

class Queue: private Block {
private:
  static const u_int SIZE = 3;
  static const u_int WRITE_INDEX_POS = 1;
  static const u_int READ_INDEX_POS = 2;
  static const u_int ARRAY_POS = 3;

  void Enlarge(u_int threshold) {
    Block *oldArray = Store::WordToBlock(GetArg(ARRAY_POS));
    u_int oldSize = oldArray->GetSize();
    u_int newSize = oldSize + threshold;
    Block *newArray = Store::AllocBlock(QueueArrayLabel, newSize);
    u_int index = Store::WordToInt(GetArg(READ_INDEX_POS));
    Assert(index == Store::WordToInt(GetArg(WRITE_INDEX_POS)));
    word *oldBase = oldArray->GetBase();
    word *newBase = newArray->GetBase();
    std::memcpy(newBase, oldBase + index, oldSize - index);
    std::memcpy(newBase + index, oldBase, index);
    ReplaceArg(WRITE_INDEX_POS, Store::IntToWord(index));
    ReplaceArg(READ_INDEX_POS, Store::IntToWord(0));
    ReplaceArg(ARRAY_POS, newArray->ToWord());
  }
public:
  using Block::ToWord;

  static Queue *New(u_int initialSize) {
    Block *b = Store::AllocBlock(QueueLabel, SIZE);
    Block *array = Store::AllocBlock(QueueArrayLabel, initialSize);
    b->InitArg(WRITE_INDEX_POS, Store::IntToWord(0));
    b->InitArg(READ_INDEX_POS, Store::IntToWord(0));
    b->InitArg(ARRAY_POS, array->ToWord());
    return static_cast<Queue *>(b);
  }
  static Queue *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == QueueLabel);
    return static_cast<Queue *>(b);
  }

  void Enqueue(word w) {
    u_int writeIndex = Store::WordToInt(GetArg(WRITE_INDEX_POS));
    u_int readIndex = Store::WordToInt(GetArg(READ_INDEX_POS));
    Block *array = Store::WordToBlock(GetArg(ARRAY_POS));
    array->ReplaceArg(writeIndex + 1, w);
    u_int size = array->GetSize();
    writeIndex = (writeIndex + 1) % size;
    if (writeIndex == readIndex)
      Enlarge(size / 2);
  }
  bool IsEmpty() {
    return GetArg(READ_INDEX_POS) == GetArg(WRITE_INDEX_POS);
  }
  word Dequeue() { // precondition: queue must not be empty
    u_int readIndex = Store::WordToInt(GetArg(READ_INDEX_POS));
    Block *array = Store::WordToBlock(GetArg(ARRAY_POS));
    Assert(readIndex != Store::WordToInt(GetArg(WRITE_INDEX_POS)));
    readIndex++;
    word result = array->GetArg(readIndex);
    ReplaceArg(READ_INDEX_POS, Store::IntToWord(readIndex % array->GetSize()));
    return result;
  }

  void Blank(u_int threshold) {
    Block *array = Store::WordToBlock(GetArg(ARRAY_POS));
    u_int readIndex = Store::WordToInt(GetArg(READ_INDEX_POS));
    u_int writeIndex = Store::WordToInt(GetArg(READ_INDEX_POS));
    u_int oldSize = array->GetSize();
    u_int newSize = (writeIndex + oldSize - readIndex) % oldSize + threshold;
    if (newSize < oldSize) {
      word *base = array->GetBase();
      if (readIndex < writeIndex) {
	u_int length = writeIndex - readIndex;
	std::memmove(base, base + readIndex, length);
	ReplaceArg(READ_INDEX_POS, Store::IntToWord(0));
	ReplaceArg(WRITE_INDEX_POS, Store::IntToWord(length));
      } else {
	u_int length = oldSize - readIndex;
	u_int newReadIndex = newSize - length;
	std::memmove(base + newReadIndex, base + readIndex, length);
	ReplaceArg(READ_INDEX_POS, Store::IntToWord(newReadIndex));
      }
      HeaderOp::EncodeSize(array, newSize);
    }
    if (readIndex < writeIndex) {
      for (u_int i = writeIndex + 1; i <= newSize; i++)
	array->ReplaceArg(i, Store::IntToWord(0));
      for (u_int i = 1; i <= readIndex; i++)
	array->ReplaceArg(i, Store::IntToWord(0));
    } else {
      for (u_int i = writeIndex + 1; i <= readIndex; i++)
	array->ReplaceArg(i, Store::IntToWord(0));
    }
  }
};

#endif __ADT__QUEUE_HH__
