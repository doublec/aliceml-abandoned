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
  MAX_LABEL   = (t_label) (MAX_LSIZE - 12);
  
  Array       = (t_label) (MAX_LSIZE - 11);
  Builtin     = (t_label) (MAX_LSIZE - 10);
  Cell        = (t_label) (MAX_LSIZE - 9);
  Constructor = (t_label) (MAX_LSIZE - 8);
  ConVal      = (t_label) (MAX_LSIZE - 7);
  Real        = (t_label) (MAX_LSIZE - 6);
  Record      = (t_label) (MAX_LSIZE - 5);
  String      = (t_label) (MAX_LSIZE - 4);
  Tuple       = (t_label) (MAX_LSIZE - 3);
  Vector      = (t_label) (MAX_LSIZE - 2);
  WideString  = (t_label) (MAX_LSIZE - 1);
}
