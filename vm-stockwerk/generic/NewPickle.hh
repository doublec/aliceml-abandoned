//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__NEWPICKLE_HH__
#define __GENERIC__NEWPICKLE_HH__

#if defined(INTERFACE)
#pragma interface "generic/NewPickle.hh"
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
//                   |  UNIQUE (??INTERN??)
//   complexInstr  ::=  ANNOUNCE complexInstr' address
// 	             |  FULFILL address
//                   |  complexInstr'
//   complexInstr' ::=  BLOCK label size
//   address       ::=  <uint>
//   size          ::=  <uint>
//   label         ::=  <uint>

class NewPickle {
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
    aBLOCK,       // 8
    FULFILL,      // 9
    ENDOFSTREAM   // A
  };

};

#endif
