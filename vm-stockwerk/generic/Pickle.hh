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

#ifndef __GENERIC__PICKLE_HH__
#define __GENERIC__PICKLE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Pickle.hh"
#endif

// pickle    ::= int | chunk | unique | block | tuple | closure | transform
// int       ::= POSINT <uint> | NEGINT <uint>
// chunk     ::= CHUNK size <byte>*size
// unique    ::= UNIQUE (chunk|reference)
// size      ::= <uint>
// block     ::= BLOCK label size field*size
// tuple     ::= TUPLE size field*size
// closure   ::= CLOSURE size field*size
// label     ::= <uint>
// field     ::= pickle | reference
// reference ::= REF id
// id        ::= <uint>
// transform ::= TRANSFORM (chunk|reference) field

class Pickle {
public:
  enum Tag {
    POSINT,
    NEGINT,
    CHUNK,
    UNIQUE,
    BLOCK,
    TUPLE,
    CLOSURE,
    REF,
    TRANSFORM
  };
};

#endif
