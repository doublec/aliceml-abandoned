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

#include "store/Store.hh"
#include <cstring>

class Queue: private Block {
private:
  static const u_int SIZE = 3;
  static const u_int WRITE_INDEX_POS = 1;
  static const u_int READ_INDEX_POS = 2;
  static const u_int ARRAY_POS = 3;

  u_int GetWriteIndex() {
    return Store::DirectWordToInt(GetArg(WRITE_INDEX_POS));
  }
  void SetWriteIndex(u_int writeIndex) {
    ReplaceArg(WRITE_INDEX_POS, writeIndex);
  }
  u_int GetReadIndex() {
    return Store::DirectWordToInt(GetArg(READ_INDEX_POS));
  }
  void SetReadIndex(u_int readIndex) {
    ReplaceArg(READ_INDEX_POS, readIndex);
  }
  Block *GetArray() {
    return Store::DirectWordToBlock(GetArg(ARRAY_POS));
  }
  void SetArray(Block *array) {
    ReplaceArg(ARRAY_POS, array->ToWord());
  }

  void Enlarge(u_int threshold) {
    Block *oldArray = GetArray();
    u_int oldSize = oldArray->GetSize();
    u_int newSize = oldSize + threshold;
    Block *newArray = Store::AllocBlock(QUEUEARRAY_LABEL, newSize);
    u_int index = GetReadIndex();
    Assert(index == GetWriteIndex());
    word *oldBase = oldArray->GetBase();
    word *newBase = newArray->GetBase();
    std::memcpy(newBase, oldBase + index, oldSize - index);
    std::memcpy(newBase + index, oldBase, index);
    SetWriteIndex(index);
    SetReadIndex(0);
    SetArray(newArray);
  }
protected:
  u_int GetNumberOfElements() {
    u_int size = GetArray()->GetSize();
    u_int nentries = GetWriteIndex() + size - GetReadIndex();
    return nentries > size? nentries - size: nentries;
  }
  word GetNthElement(u_int n) {
    Assert(n < GetNumberOfElements());
    Block *array = GetArray();
    u_int size = array->GetSize();
    u_int index = GetReadIndex() + n;
    if (index >= size)
      index -= size;
    return array->GetArg(index + 1);
  }
public:
  using Block::ToWord;

  static Queue *New(u_int initialSize) {
    Block *b = Store::AllocBlock(QUEUE_LABEL, SIZE);
    Block *array = Store::AllocBlock(QUEUEARRAY_LABEL, initialSize);
    b->InitArg(WRITE_INDEX_POS, 0);
    b->InitArg(READ_INDEX_POS, 0);
    b->InitArg(ARRAY_POS, array->ToWord());
    return static_cast<Queue *>(b);
  }
  static Queue *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == QUEUE_LABEL);
    return static_cast<Queue *>(b);
  }

  void Enqueue(word w) {
    Block *array = GetArray();
    u_int writeIndex = GetWriteIndex();
    array->ReplaceArg(writeIndex + 1, w);
    u_int size = array->GetSize();
    if (++writeIndex == size)
      writeIndex = 0;
    SetWriteIndex(writeIndex);
    if (writeIndex == GetReadIndex())
      Enlarge(size / 2);
  }
  bool IsEmpty() {
    return GetWriteIndex() == GetReadIndex();
  }
  word Dequeue() { // precondition: queue must not be empty
    u_int readIndex = GetReadIndex();
    Block *array = GetArray();
    Assert(readIndex != GetWriteIndex());
    readIndex++;
    word result = array->GetArg(readIndex);
    SetReadIndex(readIndex == array->GetSize()? 0: readIndex);
    return result;
  }

  void Blank() {
    Block *array = GetArray();
    u_int readIndex = GetReadIndex();
    u_int writeIndex = GetWriteIndex();
    u_int oldSize = array->GetSize();
    u_int nentries = writeIndex + oldSize - readIndex;
    if (nentries > oldSize)
      nentries -= oldSize;
    u_int newSize = (nentries * 3 / 2 + 1 + oldSize) / 2;
    Assert(newSize != 0 && newSize > nentries);
    if (newSize >= oldSize) {
      newSize = oldSize;
    } else {
      word *base = array->GetBase();
      if (readIndex < writeIndex) {
	u_int length = writeIndex - readIndex;
	std::memmove(base, base + readIndex, length);
	SetWriteIndex(length);
	SetReadIndex(0);
      } else {
	u_int length = oldSize - readIndex;
	u_int newReadIndex = newSize - length;
	std::memmove(base + newReadIndex, base + readIndex, length);
	SetReadIndex(newReadIndex);
      }
      HeaderOp::EncodeSize(array, newSize);
    }
    if (readIndex < writeIndex) {
      for (u_int i = writeIndex + 1; i <= newSize; i++)
	array->ReplaceArg(i, 0);
      for (u_int i = 1; i <= readIndex; i++)
	array->ReplaceArg(i, 0);
    } else {
      for (u_int i = writeIndex + 1; i <= readIndex; i++)
	array->ReplaceArg(i, 0);
    }
  }
};

#endif __ADT__QUEUE_HH__
