//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributor:
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
//   init	   ::=  INIT stackSize noOfLocals
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
//                   |  UNIQUE
//   complexInstr  ::=  ANNOUNCE complexInstr' address
// 	             |  FULFILL address
//                   |  complexInstr'
//   complexInstr' ::=  BLOCK label size
//                   |  TUPLE size
//                   |  CLOSURE size
//                   |  TRANSFORM
//   address       ::=  <uint>
//   size          ::=  <uint>
//   label         ::=  <uint>

class Pickle {
public:
  enum Tag {
    INIT,         // 0
    STORE,        // 1
    LOAD,         // 2
    POSINT,       // 3
    NEGINT,       // 4
    CHUNK,        // 5
    UNIQUE,       // 6
    BLOCK,        // 7
    TUPLE,        // 8
    CLOSURE,      // 9
    TRANSFORM,    // 10
    aBLOCK,       // 11
    aTUPLE,       // 12
    aCLOSURE,     // 13
    aTRANSFORM,   // 14
    FULFILL,      // 15
    ENDOFSTREAM   // 16
  };

};

#endif
