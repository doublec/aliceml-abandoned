//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PICKLING__PRIM_PICKLE_HH__
#define __GENERIC__PICKLING__PRIM_PICKLE_HH__

//
// pickle    ::= int | chunk | block | closure | transform
// int       ::= POSINT <uint> | NEGINT <uint>
// chunk     ::= CHUNK size <byte>*size
// size      ::= <uint>
// block     ::= BLOCK label size field*size
// closure   ::= CLOSURE size field*size
// label     ::= <uint>
// field     ::= pickle | reference
// reference ::= REF id
// id        ::= <uint>
// transform ::= TRANSFORM (chunk|reference) field
//
// Well-formedness conditions:
// -- Only backward references are allowed.
// -- The reference within a transform must not reference another transform.
//

class PrimPickle {
public:
  typedef unsigned char byte;

  enum type {
    POSINT    = 0,
    NEGINT    = 1,
    CHUNK     = 2,
    BLOCK     = 3,
    CLOSURE   = 4,
    REF       = 5,
    TRANSFORM = 6
  };
};

#endif __GENERIC__PICKLING__PRIM_PICKLE_HH__
