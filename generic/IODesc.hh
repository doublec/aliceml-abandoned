//
// Author:
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__IO_DESC_HH__
#define __GENERIC__IO_DESC_HH__

#if defined(INTERFACE)
#pragma interface "generic/IODesc.hh"
#endif

#if USE_WINSOCK
#include <windows.h>
#endif

#include "generic/String.hh"
#include "generic/FinalizationSet.hh"

class IODescFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

class SeamDll IODesc: private Block {
public:
  enum {
    TYPE_CLOSED		= 0x00,
    TYPE_FD		= 0x01,
#if USE_WINSOCK
    TYPE_HANDLE		= 0x02,
    TYPE_FORWARDED	= 0x03,
#endif
    TYPE_MASK		= 0x03,
    DIR_READER		= 0x00,
    DIR_WRITER		= 0x04,
    DIR_MASK		= 0x04
  };
private:
  static IODescFinalizationSet *finalizationSet;

  enum {
    FLAGS_POS, NAME_POS, FD_POS,
#if USE_WINSOCK
    HANDLE_POS,
#endif
    FINALIZATION_KEY_POS, SIZE
  };

  u_int GetFlags() {
    return Store::DirectWordToInt(GetArg(FLAGS_POS));
  }
  bool IsFile();
public:
  enum kind {
    FILE, DIR, SYMLINK, TTY, PIPE, SOCKET, DEVICE, CLOSED, UNKNOWN
  };
  enum result {
    result_ok, result_closed, result_request,
    result_system_error, result_socket_error, result_would_block
  };

  using Block::ToWord;

  static void Init();

  static IODesc *NewClosed(String *name);
  static IODesc *NewFromFD(u_int dir, String *name, int fd);
#if USE_WINSOCK
  static IODesc *NewFromHandle(u_int dir, String *name, HANDLE handle);
  static IODesc *NewForwarded(u_int dir, String *name, HANDLE handle);
#endif
  static IODesc *NewFromStdIn();
  static IODesc *NewFromStdOut();
  static IODesc *NewFromStdErr();

  static IODesc *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == IODESC_LABEL);
    return static_cast<IODesc *>(p);
  }
  static IODesc *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == IODESC_LABEL);
    return static_cast<IODesc *>(p);
  }

  u_int GetType() {
    return GetFlags() & TYPE_MASK;
  }
  u_int GetDir() {
    return GetFlags() & DIR_MASK;
  }
  int GetFD() {
#if USE_WINSOCK
    Assert(GetType() == TYPE_FD || GetType() == TYPE_FORWARDED);
#else
    Assert(GetType() == TYPE_FD);
#endif
    return static_cast<int>(Store::DirectWordToInt(GetArg(FD_POS)));
  }
#if USE_WINSOCK
  HANDLE GetHandle() {
    Assert(GetType() == TYPE_HANDLE || GetType() == TYPE_FORWARDED);
    HANDLE *p = (HANDLE *)
      Store::DirectWordToChunk(GetArg(HANDLE_POS))->GetBase();
    return p[0];
  }
#endif
  u_int GetOrdinal() {
    return Store::DirectWordToInt(GetArg(FINALIZATION_KEY_POS));
  }
  kind GetKind();
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }
  u_int GetChunkSize();
  result Close();

  bool SupportsDoBlock();
  result DoBlock();
  bool SupportsGetPos();
  result GetPos(u_int &out);
  bool SupportsSetPos();
  result SetPos(u_int pos);
  bool SupportsEndPos();
  result EndPos(u_int &out);
  result GetNumberOfAvailableBytes(int &out);
  result Read(u_char *buf, int n, int &out);
  result Write(const u_char *buf, int n, int &out);
  bool SupportsNonblocking();
  result CanInput(bool &out);
  result CanOutput(bool &out);
  result ReadNonblocking(u_char *buf, int n, int &out);
  result WriteNonblocking(const u_char *buf, int n, int &out);
};

#endif
