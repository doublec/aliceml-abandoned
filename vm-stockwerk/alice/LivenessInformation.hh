#ifndef __ALICE_LIVENESS_INFORMATION_H__
#define __ALICE_LIVENESS_INFORMATION_H__

#if defined(INTERFACE)
#pragma interface "alice/LivenessInformation.hh"
#endif

#include "store/Store.hh"

class LivenessInformation : private Chunk {
protected:
  static u_int RowSize(u_int m) {
    return (sizeof(word) + (m / sizeof(char)) + 1);
  }
  static u_int IndexSize() {
    return 2 * sizeof(word);
  }
  static u_int RowOffset() {
    return sizeof(word);
  }
  static u_int GetSize(u_int n, u_int m) {
    return IndexSize() + n * RowSize(m);
  }
  void SetSize(u_int n, u_int m) {
    u_int *base = (u_int *) Chunk::GetBase();
    base[0] = n;
    base[1] = m;
  }
  u_int GetN() {
    u_int *base = (u_int *) Chunk::GetBase();
    return base[0];
  }
  void Clear() {
    u_int size = Chunk::GetSize();
    memset(Chunk::GetBase() + IndexSize(), 0, size - IndexSize());
  }
  static word GetRowIndex(char *row) {
    return ((word *) row)[0];
  }
  static void SetRowIndex(char *row, word pc) {
    ((word *) row)[0] = pc;
  }
  char *GetRowBase() {
    return (Chunk::GetBase() + IndexSize());
  }
public:
  using Chunk::ToWord;

  u_int GetM() {
    u_int *base = (u_int *) Chunk::GetBase();
    return base[1];
  }
  void SetN(u_int n) {
    u_int *base = (u_int *) Chunk::GetBase();
    base[0] = n;
  }
  char *GetRow(u_int i) {
    char *base = Chunk::GetBase();
    u_int  m   = GetM();
    return (base + IndexSize() + i * RowSize(m));
  }
  char *SeekRow(word pc) {
    char *base = GetRowBase();
    u_int rs   = RowSize(GetM());
    for (u_int i = GetN(); i--;) {
      char *row = base + i * rs;
      Assert(row < Chunk::GetBase() + Chunk::GetSize());
      Assert(row + rs <= Chunk::GetBase() + Chunk::GetSize());
      if (GetRowIndex(row) == pc)
	return row;
    }
    return NULL;
  }
  void SetRowBit(char *row, u_int i) {
    u_int offset = i / sizeof(char);
    char key     = 1 << (i % sizeof(char));
    u_int index  = RowOffset() + offset;
    Assert(row >= Chunk::GetBase());
    Assert(row + RowSize(GetM()) <= Chunk::GetBase() + Chunk::GetSize());
    Assert((row + index) < Chunk::GetBase() + Chunk::GetSize());
    row[index] |= key;
  }
  char GetRowBit(char *row, u_int i) {
    u_int offset = i / sizeof(char);
    char key     = 1 << (i % sizeof(char));
    u_int index  = RowOffset() + offset;
    Assert(row >= Chunk::GetBase());
    Assert(row + RowSize(GetM()) <= Chunk::GetBase() + Chunk::GetSize());
    Assert((row + index) < Chunk::GetBase() + Chunk::GetSize());
    return (row[index] & key);
  }
  // LivenessInformation Constructor
  static LivenessInformation *New(u_int n, u_int m) {
    u_int size = GetSize(n, m);
    LivenessInformation *info = (LivenessInformation *) Store::AllocChunk(size);
    info->SetSize(n, m);
    info->Clear();
    return info;
  }
  LivenessInformation *AddRow(u_int n, word pc) {
    u_int curN = GetN();
    LivenessInformation *info;
    if (n >= curN) {
      u_int newN      = ((curN * 3) >> 1);
      u_int m         = GetM();
      u_int cloneSize = GetSize(curN, m) - IndexSize();
      info = LivenessInformation::New(newN, m);
      memcpy(info->GetRowBase(), this->GetRowBase(), cloneSize);
    }
    else
      info = this;
    SetRowIndex(info->GetRow(n), pc);
    return info;
  }
  LivenessInformation *CloneRows(u_int n) {
    u_int m                   = GetM();
    LivenessInformation *info = LivenessInformation::New(n, m);
    memcpy(info->GetRowBase(), this->GetRowBase(), GetSize(n, m) - IndexSize());
    return info;
  }
  // LivenessInformation Untagging
  static LivenessInformation *FromWordDirect(word info) {
    return (LivenessInformation *) Store::DirectWordToChunk(info);
  }
};

#endif
