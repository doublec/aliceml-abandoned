#ifndef __value_hh__
#define __value_hh__

#include "base.hh"
#include "headerop.hh"
#include "pointerop.hh"

class Tuple {
protected:
  word ar[2];
public:
  static DynamicTuple *intgen_set;
  t_label GetLabel() {
    return HeaderOp::DecodeLabel(this);
  }
  t_size GetSize() {
    return HeaderOp::DecodeSize(this);
  }
  word GetArg(t_field f) {
    Assert(f > (t_field) INVALID_FIELD);
    Assert(f <= (t_field) GetSize());
    return (word) ar[(u_int) f];
  }
  void InitArg(t_field f, word v) {
    Assert(f > (t_field) INVALID_FIELD);
    Assert(f <= (t_field) GetSize());
    //    Assert(ar[(u_int) f] == 0);
    ar[(u_int) f] = v;
  }
  void ReplaceArg(t_field f, word v) {
    Assert(f > (t_field) INVALID_FIELD);
    Assert(f <= (t_field) GetSize());
    if (!PointerOp::IsInt(v) &&
	(HeaderOp::GetHeader(PointerOp::RemoveTag(v)) < HeaderOp::GetHeader(this))) {
      intgen_set->Add(ToWord()); // Attention: static binding
    }
    ar[(u_int) f] = v;
  }
  word ToWord() {
    return PointerOp::EncodeTuple(this);
  }
};

class Transient : private Tuple {
protected:
  void PerformBind(word v, t_label l) {
    switch (GetLabel()) {
    case PROMISE:
    case FUTURE:
      ReplaceArg(Store::GenTField(1), v);
      HeaderOp::EncodeLabel(this, REF);
      break;
    default:
      Assert(0);
    }
  }
  
public:
  using Tuple::GetLabel;
  using Tuple::GetSize;
  using Tuple::GetArg;
  using Tuple::InitArg;
  void ReplaceArg(t_field f, word v) {
    Assert(f > (t_field) INVALID_FIELD);
    Assert(f <= (t_field) GetSize());
    if (!PointerOp::IsInt(v) &&
	(HeaderOp::GetHeader(PointerOp::RemoveTag(v)) < HeaderOp::GetHeader(this))) {
      intgen_set->Add(ToWord()); // Attention: static binding
    }
    ar[(u_int) f] = v;
  }
  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  void Bind(word v) {
    PerformBind(v, REF);
  }
  void Cancelled(word ex) {
    PerformBind(ex, CANCELLED);
  }
};

#endif
