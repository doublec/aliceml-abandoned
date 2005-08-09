//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__TYPES_HH__
#define __ALICE__TYPES_HH__

#if defined(INTERFACE)
#pragma interface "alice/Types.hh"
#endif

class Types {
public:
  // Bool.bool
  enum { _false, _true };
  // General.order
  enum { EQUAL, GREATER, LESS };
  // Option.option
  enum { NONE, SOME };
  // List.list
  enum { cons, nil };
  // Future.status
  enum { DETERMINED, FAILED, FUTURE };
  // IODesc
  //    Io
  enum { cause, function, name };
  //    iodesc_kind
  enum { CLOSED, DEVICE, DIR, FILE, PIPE, SOCKET, SYMLINK, TTY, UNKNOWN };
  //    capabilities
  enum { block, endPos, setPos, verifyPos,
	 CAPABILITIES_SIZE };
  //    readerCapabilities
  enum { canInput, readArr, readArrNB, readVec, readVecNB,
	 READER_CAPABILITIES_SIZE };
  //    writerCapabilities
  enum { canOutput, writeArr, writeArrNB, writeVec, writeVecNB,
	 WRITER_CAPABILITIES_SIZE };
  // Component.component
  enum { EVALUATED, UNEVALUATED, VNATIVE };
  enum { inf1, mod };                   // EVALUATED labels
  enum { body, imports, inf2 };         // UNEVALUATED labels
  enum { name1, component };            // VNATIVE labels
  // Config.platform
  enum { Config_UNIX, Config_WIN32 };
  // Label.lab
  enum { ALPHA, NUM };
  // Name.name
  enum { ExId };
};

#endif
