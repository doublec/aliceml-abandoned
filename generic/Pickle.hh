//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//   Guido Tack <tack@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//   Guido Tack, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PICKLE_HH__
#define __GENERIC__PICKLE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Pickle.hh"
#endif

//   pickle        ::=  init instrs ENDOFSTREAM
//   init	   ::=  "seam" major minor INIT stackSize noOfLocals
//   major         ::= <uint>
//   minor         ::= <uint>
//   stackSize     ::= <uint>
//   noOfLocals    ::= <uint>
//   instrs        ::=  instr instrs
// 	             |  (* empty *)
//   instr	   ::=  simpleInstr
//                   |  complexInstr
//   simpleInstr   ::=  STORE address
// 	             |  LOAD address
//                   |  POSINT <uint>
//                   |  NEGINT <uint>
//                   |  CHUNK size <byte>*size
//                   |  MCHUNK size <byte>*size
//                   |  UNIQUE
//   complexInstr  ::=  ANNOUNCE complexInstr' address
// 	             |  FULFILL address
//                   |  complexInstr'
//   complexInstr' ::=  BLOCK label size
//                   |  MBLOCK label size
//                   |  TUPLE size
//                   |  CLOSURE size
//                   |  TRANSFORM
//   address       ::=  <uint>
//   size          ::=  <uint>
//   label         ::=  <uint>

class Pickle {
public:
  static const u_int majorVersion = 3;
  static const u_int minorVersion = 0;

  enum Tag {
    INIT,         // 0
    STORE,        // 1
    LOAD,         // 2
    POSINT,       // 3
    NEGINT,       // 4
    CHUNK,        // 5
    MCHUNK,       // 6
    UNIQUE,       // 7
    BLOCK,        // 8
    MBLOCK,       // 9
    TUPLE,        // 10
    CLOSURE,      // 11
    TRANSFORM,    // 12
    aBLOCK,       // 13
    aMBLOCK,      // 14
    aTUPLE,       // 15
    aCLOSURE,     // 16
    aTRANSFORM,   // 17
    FULFILL,      // 18
    ENDOFSTREAM   // 19
  };

};

#endif
