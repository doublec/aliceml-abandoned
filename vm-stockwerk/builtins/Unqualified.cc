//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "builtins/Authoring.hh"

/*--** currently broken
static Interpreter::result Compare(TaskStack *taskStack,
				   word x0, word x1, word &out) {
  Block *a = Store::WordToBlock(x0);
  Block *b = Store::WordToBlock(x1);
  if (a == INVALID_POINTER) { // a is Transient or int
    DECLARE_INT(i, x0);
    if (b != INVALID_POINTER) { // b is TagVal
      RETURN_BOOL(false);
    }
    DECLARE_INT(j, x1);
    RETURN_BOOL(i == j);
  }
  if (b == INVALID_POINTER) { // b is Transient or int, but a is a block
    DECLARE_INT(j, x1);
    RETURN_BOOL(false);
  }
  // from here, both a and b are blocks
  BlockLabel label = a->GetLabel();
  if (label == AliceLabel::ToBlockLabel(AliceLabel::Array) ||
      label == AliceLabel::ToBlockLabel(AliceLabel::ArrayZero) ||
      label == AliceLabel::ToBlockLabel(AliceLabel::Builtin) ||
      label == AliceLabel::ToBlockLabel(AliceLabel::Constructor) ||
      label == AliceLabel::ToBlockLabel(AliceLabel::Cell)) {
    RETURN_BOOL(a == b);
  } else if (label == b->GetLabel()) {
    int size = a->GetSize();
    if (b->GetSize() != size)
      RETURN_BOOL(false);
    for (int i = 1; i <= size; i++) {
      if (Compare(taskStack,
		  a->GetArg(i), b->GetArg(i), out) == Primitive::CONTINUE) {
	if (!Store::WordToInt(out)) {
	  RETURN_BOOL(false);
	}
      } else {
	return Primitive::REQUEST;
      }
    }
    RETURN_BOOL(true);
  }
  RETURN_BOOL(false);
}

DEFINE2(opeq) {
  return Compare(taskStack, x0, x1, out);
}

DEFINE2(opnoteq) {
  if (Compare(taskStack, x0, x1, out) == Primitive::CONTINUE) {
    out = Store::IntToWord(!Store::WordToInt(out));
    return Primitive::CONTINUE;
  } else {
    return Primitive::REQUEST;
  }
}

void Primitive::RegisterUnqualified() {
  Register("=", opeq);
  Register("<>", opnoteq);
};
*/
