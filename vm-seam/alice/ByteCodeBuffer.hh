//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTE_CODE_BUFFER_HH__
#define __ALICE_BYTE_CODE_BUFFER_HH__

#include <cstring> 
#include "alice/Data.hh"

#define CODE_SLOT_SIZE sizeof(word)
#define SLOT_TO_INT(slot) (int) slot
#define INT_TO_SLOT(i) (slot_type) i

#define MAX_SLOT 0xFFFFFFFF // ajust this const if sizeof(word) != 4
typedef word slot_type;

#define INITIAL_WRITEBUFFER_SIZE 10000 // TODO: find reasonable size


// write-only (dynamic) byte code buffer
class AliceDll WriteBuffer {
private:
  static slot_type *codeBuf;
  static u_int size;
  static u_int top;
public: 
  static void Init(u_int s = INITIAL_WRITEBUFFER_SIZE) {
    Assert(s > 1); // otherwise resize won't work
    top = 0;
    if(s > size || codeBuf == NULL) {
      delete(codeBuf);
      size = s;
      codeBuf = new slot_type[size];
    }
  }
  static u_int GetSize() {
    return size;
  }
  static Chunk *FlushCode() {
    // copy code into chunk
    Chunk *code = Store::AllocChunk(top * CODE_SLOT_SIZE, STORE_GEN_OLDEST);
    memcpy(code->GetBase(), codeBuf, top * CODE_SLOT_SIZE);
    // shrink buffer for next use
    if(top * 3 / 2 < size) {
      size = size * 2 / 3;
      slot_type *newCodeBuf = new slot_type[size];
      delete(codeBuf);
      codeBuf = newCodeBuf;
    }
    top = 0;
    return code;
  }
  static void SetSlot(u_int index, int slot) {
    if(index >= top)
      top = index+1;
    if(index+1 >= size) {
      size = size * 3 / 2; 
      slot_type *newCodeBuf = new slot_type[size];
      memcpy(newCodeBuf, codeBuf, top * CODE_SLOT_SIZE);
      delete(codeBuf);
      codeBuf = newCodeBuf;
    }
    codeBuf[index] = (slot_type) (slot); // & MAX_SLOT);
  }
};

// read-only byte code buffer
class AliceDll ReadBuffer : private Chunk {
public:
  static ReadBuffer *New(Chunk *code) {
    return STATIC_CAST(ReadBuffer *, code->GetBase());
  }			     
  void RewriteSlot(u_int index, int slot) {
    ((slot_type *) this)[index] = (slot_type) slot;
  }
  slot_type GetSlot(u_int index) {
    return ((slot_type *) this)[index];
  }
  u_int GetSlotInt(u_int index) {
    return SLOT_TO_INT(GetSlot(index));
  }
};

#endif // __ALICE_BYTE_CODE_BUFFER_HH__
