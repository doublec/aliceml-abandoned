//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <iostream.h>

#if defined(INTERFACE)
#pragma implementation "alicedata.hh"
#endif
#include "alicedata.hh"
//
// Class Members
//
t_label AliceLabel::MIN_LABEL;
t_label AliceLabel::MAX_LABEL;

t_label AliceLabel::Array;
t_label AliceLabel::Builtin;
t_label AliceLabel::Cell;
t_label AliceLabel::Constructor;
t_label AliceLabel::ConVal;
t_label AliceLabel::Real;
t_label AliceLabel::Record;
t_label AliceLabel::String;
t_label AliceLabel::Tuple;
t_label AliceLabel::Vector;
t_label AliceLabel::WideString;
//
// Method Implementations
//
void AliceLabel::CreateAliceLabel() {
  MIN_LABEL   = (t_label) 0;
  MAX_LABEL   = (t_label) (MAX_LSIZE - 14);
  
  Array       = (t_label) (MAX_LSIZE - 13);
  ArrayZero   = (t_label) (MAX_LSIZE - 12);
  Builtin     = (t_label) (MAX_LSIZE - 11);
  Cell        = (t_label) (MAX_LSIZE - 10);
  Constructor = (t_label) (MAX_LSIZE - 9);
  ConVal      = (t_label) (MAX_LSIZE - 8);
  Real        = (t_label) (MAX_LSIZE - 7);
  Record      = (t_label) (MAX_LSIZE - 6);
  String      = (t_label) (MAX_LSIZE - 5);
  Tuple       = (t_label) (MAX_LSIZE - 4);
  Vector      = (t_label) (MAX_LSIZE - 3);
  VectorZero  = (t_label) (MAX_LSIZE - 2);
  WideString  = (t_label) (MAX_LSIZE - 1);
}
