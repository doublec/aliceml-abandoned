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

// GNUC allows labels as values for direct threaded code
// NOTE: for some misterious reason direct threaded code is slower on PowerPC
#if defined(__GNUC__) && !defined(__ppc__)
#define THREADED
#else
#undef THREADED
#endif // __GNUC__

// type definition for ProgramCounter
typedef u_int CodeSlot;

#ifdef THREADED
typedef CodeSlot *ProgramCounter; // pointer to the current code slot
#else
typedef CodeSlot ProgramCounter; // index into the code
#endif

#define CODE_SLOT_SIZE sizeof(CodeSlot)
#define SLOT_TO_INT(slot) (u_int) slot
#define INT_TO_SLOT(i) (CodeSlot) i

#define INITIAL_WRITEBUFFER_SIZE 10000 // TODO: find reasonable size

// write-only (dynamic) byte code buffer
class AliceDll WriteBuffer {
private:
  static CodeSlot *codeBuf;
  static u_int size;
  static u_int top;
public: 
  static void Init(u_int s = INITIAL_WRITEBUFFER_SIZE) {
    Assert(s > 1); // otherwise resize won't work
    top = 0;
    if(s > size || codeBuf == NULL) {
      delete(codeBuf);
      size = s;
      codeBuf = new CodeSlot[size];
    }
  }
  static u_int GetSize() {
    return size;
  }
  static void Shrink(u_int offset) {
    top -= offset;
  }
  static Chunk *FlushCode() {
    // copy code into chunk
    Chunk *code = Store::AllocChunk(top * CODE_SLOT_SIZE, STORE_GEN_OLDEST);
    memcpy(code->GetBase(), codeBuf, top * CODE_SLOT_SIZE);
    // shrink buffer for next use
    if(top * 3 / 2 < size) {
      size = size * 2 / 3;
      CodeSlot *newCodeBuf = new CodeSlot[size];
      delete(codeBuf);
      codeBuf = newCodeBuf;
    }
    top = 0;
    return code;
  }
  static __attribute__((always_inline)) void SetSlot(u_int index, CodeSlot slot) {
    if(index >= top)
      top = index+1;
    if(index+1 >= size) {
      size = size * 3 / 2; 
      CodeSlot *newCodeBuf = new CodeSlot[size];
      memcpy(newCodeBuf, codeBuf, top * CODE_SLOT_SIZE);
      delete(codeBuf);
      codeBuf = newCodeBuf;
    }
    codeBuf[index] = slot;
  }
};

// read-only byte code buffer
// this is only needed for a switch-based interpreter
// as with threaded code you access the code directly via the pc
class AliceDll ReadBuffer : private Chunk {
public:
  static ReadBuffer *New(Chunk *code) {
    return STATIC_CAST(ReadBuffer *, code->GetBase());
  }			     
  void RewriteSlot(u_int index, CodeSlot slot) {
    ((CodeSlot *) this)[index] = slot;
  }
  CodeSlot GetSlot(u_int index) {
    return ((CodeSlot *) this)[index];
  }
  u_int GetSlotInt(u_int index) {
    return SLOT_TO_INT(GetSlot(index));
  }
};

#endif // __ALICE_BYTE_CODE_BUFFER_HH__
