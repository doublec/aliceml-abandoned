//
// Author:
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include <cstdlib>
#include <cstring>
#if USE_WINSOCK
#include <windows.h>
#else
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#define GetLastError() errno
#define WSAGetLastError() errno
#endif

#include "alice/Authoring.hh"

static word ClosedStreamConstructor;
static word SysErrConstructor;
#include "SysErr.icc"

//
// Primitives
//
#define INTERPRET_RESULT(result) {				\
  switch (result) {						\
  case IODesc::result_ok: RETURN_UNIT;				\
  case IODesc::result_closed: RAISE(ClosedStreamConstructor);	\
  case IODesc::result_request: REQUEST(Scheduler::currentData);	\
  case IODesc::result_system_error: RAISE_SYS_ERR();		\
  case IODesc::result_socket_error: RAISE_SOCK_ERR();		\
  default: Error("invalid result");				\
  }								\
}

#define x_buf x0
#define x_i x1
#define x_iodesc x2
#define x_sz x3

#define DECLARE_BUF(DECLARE_X)						 \
  DECLARE_IODESC(ioDesc, x_iodesc);					 \
  int sz;								 \
  u_char *buf;								 \
  {									 \
    DECLARE_X(x, x_buf);						 \
    DECLARE_INT(i, x_i);						 \
    u_int length = x->GetLength();					 \
    if (i < 0 || static_cast<u_int>(i) > length)			 \
      RAISE(PrimitiveTable::General_Subscript);				 \
    TagVal *tagVal = TagVal::FromWord(x_sz);				 \
    if (tagVal == INVALID_POINTER) {					 \
      DECLARE_INT(none, x_sz); Assert(none == Types::NONE); none = none; \
      sz = length - i;							 \
    } else {								 \
      DECLARE_INT(sz0, tagVal->Sel(0));					 \
      if (sz0 < 0) RAISE(PrimitiveTable::General_Size);			 \
      if (static_cast<u_int>(i + sz0) > length)				 \
	RAISE(PrimitiveTable::General_Subscript);			 \
      sz = sz0;								 \
    }									 \
    if (sz == 0) RAISE(PrimitiveTable::General_Domain);			 \
    buf = x->GetValue() + i;						 \
  }

DEFINE1(UnsafeIODesc_hash) {
  DECLARE_IODESC(ioDesc, x0);
  RETURN_INT(ioDesc->GetOrdinal());
} END

DEFINE2(UnsafeIODesc_compare) {
  DECLARE_IODESC(ioDesc1, x0);
  DECLARE_IODESC(ioDesc2, x1);
  u_int ordinal1 = ioDesc1->GetOrdinal();
  u_int ordinal2 = ioDesc2->GetOrdinal();
  if (ordinal1 == ordinal2) {
    RETURN_INT(Types::EQUAL);
  } else if (ordinal1 < ordinal2) {
    RETURN_INT(Types::LESS);
  } else { // ordinal1 > ordinal2
    RETURN_INT(Types::GREATER);
  }
} END

DEFINE1(UnsafeIODesc_kind) {
  DECLARE_IODESC(ioDesc, x0);
  switch (ioDesc->GetKind()) {
  case IODesc::FILE: RETURN_INT(Types::FILE);
  case IODesc::DIR: RETURN_INT(Types::DIR);
  case IODesc::SYMLINK: RETURN_INT(Types::SYMLINK);
  case IODesc::TTY: RETURN_INT(Types::TTY);
  case IODesc::PIPE: RETURN_INT(Types::PIPE);
  case IODesc::SOCKET: RETURN_INT(Types::SOCKET);
  case IODesc::DEVICE: RETURN_INT(Types::DEVICE);
  case IODesc::CLOSED: RETURN_INT(Types::CLOSED);
  case IODesc::UNKNOWN: RETURN_INT(Types::UNKNOWN);
  default: Error("unknown kind");
  }
} END

DEFINE1(UnsafeIODesc_name) {
  DECLARE_IODESC(ioDesc, x0);
  RETURN(ioDesc->GetName()->ToWord());
} END

DEFINE1(UnsafeIODesc_chunkSize) {
  DECLARE_IODESC(ioDesc, x0);
  RETURN_INT(ioDesc->GetChunkSize());
} END

DEFINE1(UnsafeIODesc_close) {
  DECLARE_IODESC(ioDesc, x0);
  INTERPRET_RESULT(ioDesc->Close());
} END

DEFINE1(UnsafeIODesc_capabilities) {
  DECLARE_IODESC(ioDesc, x0);
  Tuple *result = Tuple::New(Types::CAPABILITIES_SIZE);
  result->Init(Types::block, BOOL_TO_WORD(ioDesc->SupportsDoBlock()));
  result->Init(Types::setPos, BOOL_TO_WORD(ioDesc->SupportsSetPos()));
  result->Init(Types::endPos, BOOL_TO_WORD(ioDesc->SupportsEndPos()));
  result->Init(Types::verifyPos, BOOL_TO_WORD(ioDesc->SupportsGetPos()));
  RETURN(result->ToWord());
} END

DEFINE1(UnsafeIODesc_block) {
  DECLARE_IODESC(ioDesc, x0);
  INTERPRET_RESULT(ioDesc->DoBlock());
} END

DEFINE2(UnsafeIODesc_setPos) {
  DECLARE_IODESC(ioDesc, x0);
  DECLARE_INT(pos, x1);
  INTERPRET_RESULT(ioDesc->SetPos(pos));
} END

DEFINE1(UnsafeIODesc_endPos) {
  DECLARE_IODESC(ioDesc, x0);
  u_int out;
  IODesc::result result = ioDesc->EndPos(out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_INT(out);
} END

DEFINE1(UnsafeIODesc_verifyPos) {
  DECLARE_IODESC(ioDesc, x0);
  u_int out;
  IODesc::result result = ioDesc->GetPos(out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_INT(out);
} END

DEFINE1(UnsafeIODesc_avail) {
  DECLARE_IODESC(ioDesc, x0);
  int out;
  IODesc::result result = ioDesc->GetNumberOfAvailableBytes(out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  if (out < 0) RETURN_INT(Types::NONE);
  TagVal *some = TagVal::New(Types::SOME, 1);
  some->Init(0, Store::IntToWord(out));
  RETURN(some->ToWord());
} END

DEFINE1(UnsafeIODesc_readerCapabilities) {
  DECLARE_IODESC(ioDesc, x0);
  bool supportsNonblocking = ioDesc->SupportsNonblocking();
  Tuple *result = Tuple::New(Types::READER_CAPABILITIES_SIZE);
  result->Init(Types::readVec, BOOL_TO_WORD(true));
  result->Init(Types::readArr, BOOL_TO_WORD(true));
  result->Init(Types::readVecNB, BOOL_TO_WORD(supportsNonblocking));
  result->Init(Types::readArrNB, BOOL_TO_WORD(supportsNonblocking));
  result->Init(Types::canInput, BOOL_TO_WORD(supportsNonblocking));
  RETURN(result->ToWord());
} END

DEFINE2(UnsafeIODesc_readVec) {
  DECLARE_IODESC(ioDesc, x0);
  DECLARE_INT(n, x1);
  if (n < 0 || static_cast<u_int>(n) > Vector::maxLen)
    RAISE(PrimitiveTable::General_Size);
  if (n == 0) RAISE(PrimitiveTable::General_Domain);
  String *string0 = String::New(n);
  u_char *buf = string0->GetValue();
  int out;
  IODesc::result result = ioDesc->Read(buf, n, out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  if (out == n) RETURN(string0->ToWord());
  String *string = String::New(out);
  std::memcpy(string->GetValue(), buf, out);
  RETURN(string->ToWord());
} END

DEFINE4(UnsafeIODesc_readArr) {
  DECLARE_BUF(DECLARE_WORD8ARRAY);
  int out;
  IODesc::result result = ioDesc->Read(buf, sz, out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_INT(out);
} END

DEFINE2(UnsafeIODesc_readVecNB) {
  DECLARE_IODESC(ioDesc, x0);
  DECLARE_INT(n, x1);
  if (n < 0 || static_cast<u_int>(n) > Vector::maxLen)
    RAISE(PrimitiveTable::General_Size);
  if (n == 0) RAISE(PrimitiveTable::General_Domain);
  String *string0 = String::New(n);
  u_char *buf = string0->GetValue();
  int out;
  IODesc::result result = ioDesc->ReadNonblocking(buf, n, out);
  if (result == IODesc::result_ok) {
    if (out == n) RETURN(string0->ToWord());
    String *string = String::New(out);
    std::memcpy(string->GetValue(), buf, out);
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, string->ToWord());
    RETURN(some->ToWord());
  } else if (result == IODesc::result_would_block) {
    RETURN_INT(Types::NONE);
  } else INTERPRET_RESULT(result);
} END

DEFINE4(UnsafeIODesc_readArrNB) {
  DECLARE_BUF(DECLARE_WORD8ARRAY);
  int out;
  IODesc::result result = ioDesc->ReadNonblocking(buf, sz, out);
  if (result == IODesc::result_ok) {
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, Store::IntToWord(out));
    RETURN(some->ToWord());
  } else if (result == IODesc::result_would_block) {
    RETURN_INT(Types::NONE);
  } else INTERPRET_RESULT(result);
} END

DEFINE1(UnsafeIODesc_canInput) {
  DECLARE_IODESC(ioDesc, x0);
  bool out;
  IODesc::result result = ioDesc->CanInput(out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_BOOL(out);
} END

DEFINE1(UnsafeIODesc_writerCapabilities) {
  DECLARE_IODESC(ioDesc, x0);
  bool supportsNonblocking = ioDesc->SupportsNonblocking();
  Tuple *result = Tuple::New(Types::WRITER_CAPABILITIES_SIZE);
  result->Init(Types::writeVec, BOOL_TO_WORD(true));
  result->Init(Types::writeArr, BOOL_TO_WORD(true));
  result->Init(Types::writeVecNB, BOOL_TO_WORD(supportsNonblocking));
  result->Init(Types::writeArrNB, BOOL_TO_WORD(supportsNonblocking));
  result->Init(Types::canOutput, BOOL_TO_WORD(supportsNonblocking));
  RETURN(result->ToWord());
} END

DEFINE4(UnsafeIODesc_writeVec) {
  DECLARE_BUF(DECLARE_WORD8VECTOR);
  int out;
  IODesc::result result = ioDesc->Write(buf, sz, out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_INT(out);
} END

DEFINE4(UnsafeIODesc_writeArr) {
  DECLARE_BUF(DECLARE_WORD8ARRAY);
  int out;
  IODesc::result result = ioDesc->Write(buf, sz, out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_INT(out);
} END

DEFINE4(UnsafeIODesc_writeVecNB) {
  DECLARE_BUF(DECLARE_WORD8VECTOR);
  int out;
  IODesc::result result = ioDesc->WriteNonblocking(buf, sz, out);
  if (result == IODesc::result_ok) {
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, Store::IntToWord(out));
    RETURN(some->ToWord());
  } else if (result == IODesc::result_would_block) {
    RETURN_INT(Types::NONE);
  } else INTERPRET_RESULT(result);
} END

DEFINE4(UnsafeIODesc_writeArrNB) {
  DECLARE_BUF(DECLARE_WORD8ARRAY);
  int out;
  IODesc::result result = ioDesc->WriteNonblocking(buf, sz, out);
  if (result == IODesc::result_ok) {
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, Store::IntToWord(out));
    RETURN(some->ToWord());
  } else if (result == IODesc::result_would_block) {
    RETURN_INT(Types::NONE);
  } else INTERPRET_RESULT(result);
} END

DEFINE1(UnsafeIODesc_canOutput) {
  DECLARE_IODESC(ioDesc, x0);
  bool out;
  IODesc::result result = ioDesc->CanOutput(out);
  if (result != IODesc::result_ok) INTERPRET_RESULT(result);
  RETURN_BOOL(out);
} END

DEFINE1(UnsafeIODesc_openIn) {
  DECLARE_STRING(name, x0);
#if USE_WINSOCK
  HANDLE hFile = CreateFile(name->ExportC(), GENERIC_READ, FILE_SHARE_READ,
			    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE) RAISE_SYS_ERR();
  RETURN(IODesc::NewFromHandle(IODesc::DIR_READER, name, hFile)->ToWord());
#else
  int fd = open(name->ExportC(), O_RDONLY | O_NONBLOCK);
  if (fd == -1) RAISE_SYS_ERR();
  RETURN(IODesc::NewFromFD(IODesc::DIR_READER, name, fd)->ToWord());
#endif
} END

DEFINE1(UnsafeIODesc_openOut) {
  DECLARE_STRING(name, x0);
#if USE_WINSOCK
  HANDLE hFile = CreateFile(name->ExportC(), GENERIC_WRITE, 0,
			    NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE) RAISE_SYS_ERR();
  RETURN(IODesc::NewFromHandle(IODesc::DIR_WRITER, name, hFile)->ToWord());
#else
  int fd = open(name->ExportC(), O_WRONLY | O_CREAT | O_TRUNC | O_NONBLOCK,
		S_IRWXU | S_IRWXG | S_IRWXO);
  if (fd == -1) RAISE_SYS_ERR();
  RETURN(IODesc::NewFromFD(IODesc::DIR_WRITER, name, fd)->ToWord());
#endif
} END

DEFINE1(UnsafeIODesc_openAppend) {
  DECLARE_STRING(name, x0);
#if USE_WINSOCK
  HANDLE hFile = CreateFile(name->ExportC(), GENERIC_WRITE, 0,
			    NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE) RAISE_SYS_ERR();
  if (SetFilePointer(hFile, 0, NULL, FILE_END) == INVALID_SET_FILE_POINTER)
    RAISE_SYS_ERR();
  RETURN(IODesc::NewFromHandle(IODesc::DIR_WRITER, name, hFile)->ToWord());
#else
  int fd = open(name->ExportC(), O_WRONLY | O_CREAT | O_APPEND | O_NONBLOCK,
		S_IRWXU | S_IRWXG | S_IRWXO);
  if (fd == -1) RAISE_SYS_ERR();
  RETURN(IODesc::NewFromFD(IODesc::DIR_WRITER, name, fd)->ToWord());
#endif
} END

AliceDll word UnsafeIODesc() {
  SysErrConstructor =
    UniqueConstructor::New("SysErr", "OS.SysErr")->ToWord();
  RootSet::Add(SysErrConstructor);
  ClosedStreamConstructor =
    UniqueConstructor::New("ClosedStream", "IO.ClosedStream")->ToWord();
  RootSet::Add(ClosedStreamConstructor);

  Record *record = Record::New(32);
  record->Init("'ClosedStream", ClosedStreamConstructor);
  record->Init("ClosedStream", ClosedStreamConstructor);
  // common operations supported by all readers/writers
  INIT_STRUCTURE(record, "UnsafeIODesc", "hash",
		 UnsafeIODesc_hash, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "compare",
		 UnsafeIODesc_compare, 2);
  INIT_STRUCTURE(record, "UnsafeIODesc", "kind",
		 UnsafeIODesc_kind, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "name",
		 UnsafeIODesc_name, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "chunkSize",
		 UnsafeIODesc_chunkSize, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "close",
		 UnsafeIODesc_close, 1);
  // common operations not supported by all readers/writers
  INIT_STRUCTURE(record, "UnsafeIODesc", "capabilities",
		 UnsafeIODesc_capabilities, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "block",
		 UnsafeIODesc_block, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "setPos",
		 UnsafeIODesc_setPos, 2);
  INIT_STRUCTURE(record, "UnsafeIODesc", "endPos",
		 UnsafeIODesc_endPos, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "verifyPos",
		 UnsafeIODesc_verifyPos, 1);
  // reader operations supported by all readers
  INIT_STRUCTURE(record, "UnsafeIODesc", "avail",
		 UnsafeIODesc_avail, 1);
  // reader operations not supported by all readers
  INIT_STRUCTURE(record, "UnsafeIODesc", "readerCapabilities",
		 UnsafeIODesc_readerCapabilities, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "readVec",
		 UnsafeIODesc_readVec, 2);
  INIT_STRUCTURE(record, "UnsafeIODesc", "readArr",
		 UnsafeIODesc_readArr, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "readVecNB",
		 UnsafeIODesc_readVecNB, 2);
  INIT_STRUCTURE(record, "UnsafeIODesc", "readArrNB",
		 UnsafeIODesc_readArrNB, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "canInput",
		 UnsafeIODesc_canInput, 1);
  // writer operations not supported by all writers
  INIT_STRUCTURE(record, "UnsafeIODesc", "writerCapabilities",
		 UnsafeIODesc_writerCapabilities, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "writeVec",
		 UnsafeIODesc_writeVec, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "writeArr",
		 UnsafeIODesc_writeArr, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "writeVecNB",
		 UnsafeIODesc_writeVecNB, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "writeArrNB",
		 UnsafeIODesc_writeArrNB, 4);
  INIT_STRUCTURE(record, "UnsafeIODesc", "canOutput",
		 UnsafeIODesc_canOutput, 1);
  // creating tty iodescs
  record->Init("stdIn", IODesc::NewFromStdIn()->ToWord());
  record->Init("stdOut", IODesc::NewFromStdOut()->ToWord());
  record->Init("stdErr", IODesc::NewFromStdErr()->ToWord());
  // creating file iodescs
  INIT_STRUCTURE(record, "UnsafeIODesc", "openIn",
		 UnsafeIODesc_openIn, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "openOut",
		 UnsafeIODesc_openOut, 1);
  INIT_STRUCTURE(record, "UnsafeIODesc", "openAppend",
		 UnsafeIODesc_openAppend, 1);

  //--** creating pipe iodescs

  RETURN_STRUCTURE("UnsafeIODesc$", record);
}
