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

#include "generic/String.hh"
#include "generic/FinalizationSet.hh"

class IODescFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

class IODesc: private Block {
public:
  static const u_int TYPE_CLOSED	= 0x00;
  static const u_int TYPE_FD		= 0x01;
#if defined(__MINGW32__) || defined(_MSC_VER)
  static const u_int TYPE_HANDLE	= 0x02;
  static const u_int TYPE_FORWARDED	= 0x03;
#endif
  static const u_int TYPE_MASK		= 0x03;
  static const u_int DIR_READER		= 0x00;
  static const u_int DIR_WRITER		= 0x04;
  static const u_int DIR_MASK		= 0x04;
private:
  static IODescFinalizationSet *finalizationSet;

  enum {
    FLAGS_POS, NAME_POS, FD_POS,
#if defined(__MINGW32__) || defined(_MSC_VER)
    HANDLE_POS,
#endif
    FINALIZATION_KEY_POS, SIZE
  };

  u_int GetFlags() {
    return Store::DirectWordToInt(GetArg(FLAGS_POS));
  }
public:
  enum kind {
    FILE, DIR, SYMLINK, TTY, PIPE, SOCKET, DEVICE, CLOSED, UNKNOWN
  };

  using Block::ToWord;

  static void Init();

  static IODesc *NewClosed(String *name);
  static IODesc *NewFromFD(u_int dir, String *name, int fd);
#if defined(__MINGW32__) || defined(_MSC_VER)
  static IODesc *NewFromHandle(u_int dir, String *name, HANDLE handle);
  static IODesc *NewForwarded(u_int dir, String *name, HANDLE handle);
#endif
  static IODesc *NewFromStdIn();
  static IODesc *NewFromStdOut();
  static IODesc *NewFromStdErr();

  static IODesc *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == IO_DESC_LABEL);
    return static_cast<IODesc *>(p);
  }
  static IODesc *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == IO_DESC_LABEL);
    return static_cast<IODesc *>(p);
  }

  u_int GetType() {
    return GetFlags() & TYPE_MASK;
  }
  int GetFD() {
#if defined(__MINGW32__) || defined(_MSC_VER)
    Assert(GetType() == TYPE_FD || GetType() == TYPE_FORWARDED);
#else
    Assert(GetType() == TYPE_FD);
#endif
    return Store::DirectWordToInt(GetArg(FD_POS));
  }
#if defined(__MINGW32__) || defined(_MSC_VER)
  HANDLE GetHandle() {
    Assert(GetType() == TYPE_HANDLE || GetType() == TYPE_FORWARDED);
    return Store::WordToUnmanagedPointer(GetArg(HANDLE_POS));
  }
#endif
  u_int GetOrdinal() {
    return Store::DirectWordToInt(GetArg(FINALIZATION_KEY_POS));
  }
  kind GetKind();
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }

  void Close();
};

#endif
