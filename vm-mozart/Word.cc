/*
 *  Author:
 *    Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 *  Copyright:
 *    Leif Kornstaedt, 1999
 *
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 */

#include <stdio.h>
#include <iostream.h>
#include "mozart.h"

//
// Word Extension
//

#define MAXWIDTH 32
#define TRUNCATE(v, n) \
	(((unsigned int) (v << (MAXWIDTH - n))) >> (MAXWIDTH - n))

class MsgBuffer;
void marshalNumber(unsigned int i, MsgBuffer *bs);
unsigned int unmarshalNumber(MsgBuffer *bs);

class Word: public OZ_Extension {
public:
  static int id;

  unsigned int size;
  unsigned int value;

  Word(int s, int v) {
    Assert(s <= MAXWIDTH);
    size = s;
    value = TRUNCATE(v, s);
  }

  Word(const Word &w) {
    size = w.size;
    value = w.value;
  }

  int getIdV(void) {
    return id;
  }

  OZ_Term typeV(void) {
    return OZ_atom("word");
  }

  OZ_Return eqV(OZ_Term t) {
    if (OZ_isExtension(t)) {
      OZ_Extension *e = OZ_getExtension(t);
      if (e->getIdV() == id) {
	Word *w = (Word *) e;
	if (w->size == size && w->value == value) {
	  return PROCEED;
	}
      }
    }
    return FAILED;
  }

  void printStreamV(ostream &out, int depth) {
    out << "<word" << size << " 0w" << value << ">";
  }

  OZ_Term printV(int depth) {
    char s[11];
    sprintf(s, "%u", value);
    return OZ_mkTupleC("#", 5, OZ_atom("<word"), OZ_int(size),
		       OZ_atom(" 0w"), OZ_atom(s), OZ_atom(">"));
  }

  OZ_Extension *gCollectV(void) {
    return new Word(*this);
  }

  OZ_Extension *sCloneV(void) {
    return new Word(*this);
  }

  void gCollectRecurseV(void) {}

  void sCloneRecurseV(void) {}

  OZ_Boolean marshalV(void *p) {
    MsgBuffer *bs = (MsgBuffer *) p;
    marshalNumber(size, bs);
    marshalNumber(value, bs);
    return OZ_TRUE;
  }
};

int Word::id;

inline static bool OZ_isWord(OZ_Term t) {
  t = OZ_deref(t);
  return OZ_isExtension(t) && OZ_getExtension(t)->getIdV() == Word::id;
}

inline static Word *OZ_WordToC(OZ_Term t) {
  return (Word *) OZ_getExtension(OZ_deref(t));
}

#define OZ_declareWord(ARG, VAR) \
	OZ_declareType(ARG, VAR, Word *, "word", \
		       OZ_isWord, OZ_WordToC)

#define OZ_word(size, value) OZ_extension(new Word(size, value))
#define OZ_RETURN_WORD(size, value) OZ_RETURN(OZ_word(size, value))

OZ_Term unmarshalWord(void *p) {
  MsgBuffer *bs = (MsgBuffer *) p;
  int size = unmarshalNumber(bs);
  int value = unmarshalNumber(bs);
  return OZ_word(size, value);
}

//
// Builtins
//

OZ_BI_define(Word_is, 1, 1) {
  OZ_declareDetTerm(0, x);
  OZ_RETURN_BOOL(OZ_isWord(x));
} OZ_BI_end

OZ_BI_define(Word_make, 2, 1) {
  OZ_declareInt(0, size);
  if (size <= 0 || size > MAXWIDTH) {
    return OZ_raiseDebug(OZ_makeException(OZ_atom("system"), OZ_atom("kernel"),
					  "Word.make", 1, OZ_int(size)));
  }
  OZ_declareInt(1, value);   //--** may truncate too much!
  OZ_RETURN_WORD(size, value);
} OZ_BI_end

OZ_BI_define(Word_size, 1, 1) {
  OZ_declareWord(0, w);
  OZ_RETURN_INT(w->size);
} OZ_BI_end

OZ_BI_define(Word_toInt, 1, 1) {
  OZ_declareWord(0, w);
  OZ_RETURN(OZ_unsignedInt(w->value));
} OZ_BI_end

OZ_BI_define(Word_toIntX, 1, 1) {
  OZ_declareWord(0, w);
  signed int v = w->value << (MAXWIDTH - w->size);
  OZ_RETURN_INT(v >> (MAXWIDTH - w->size));
} OZ_BI_end

OZ_BI_define(Word_orb, 2, 1) {
  OZ_declareWord(0, w1);
  OZ_declareWord(1, w2);
  if (w1->size != w2->size) {
    return OZ_raiseDebug(OZ_makeException(OZ_atom("system"), OZ_atom("kernel"),
					  "Word.binop", 2,
					  OZ_in(0), OZ_in(1)));
  }
  OZ_RETURN_WORD(w1->size, w1->value | w2->value);
} OZ_BI_end

OZ_BI_define(Word_xorb, 2, 1) {
  OZ_declareWord(0, w1);
  OZ_declareWord(1, w2);
  if (w1->size != w2->size) {
    return OZ_raiseDebug(OZ_makeException(OZ_atom("system"), OZ_atom("kernel"),
					  "Word.binop", 2,
					  OZ_in(0), OZ_in(1)));
  }
  OZ_RETURN_WORD(w1->size, w1->value ^ w2->value);
} OZ_BI_end

OZ_BI_define(Word_andb, 2, 1) {
  OZ_declareWord(0, w1);
  OZ_declareWord(1, w2);
  if (w1->size != w2->size) {
    return OZ_raiseDebug(OZ_makeException(OZ_atom("system"), OZ_atom("kernel"),
					  "Word.binop", 2,
					  OZ_in(0), OZ_in(1)));
  }
  OZ_RETURN_WORD(w1->size, w1->value & w2->value);
} OZ_BI_end

OZ_BI_define(Word_notb, 1, 1) {
  OZ_declareWord(0, w);
  OZ_RETURN_WORD(w->size, ~w->value);
} OZ_BI_end

OZ_BI_define(Word_shl, 2, 1) {
  OZ_declareWord(0, w);
  OZ_declareInt(1, n);
  OZ_RETURN_WORD(w->size, w->value << n);
} OZ_BI_end

OZ_BI_define(Word_lsr, 2, 1) {
  OZ_declareWord(0, w);
  OZ_declareInt(1, n);
  OZ_RETURN_WORD(w->size, w->value >> n);
} OZ_BI_end

OZ_BI_define(Word_asr, 2, 1) {
  OZ_declareWord(0, w);
  OZ_declareInt(1, n);
  signed int v = w->value << (MAXWIDTH - w->size);
  OZ_RETURN_WORD(w->size, v >> (MAXWIDTH - w->size + n));
} OZ_BI_end

//
// Interface
//

OZ_C_proc_interface *oz_init_module(void) {
  static OZ_C_proc_interface interface[] = {
    {"is", 1, 1, Word_is},
    {"make", 2, 1, Word_make},
    {"size", 1, 1, Word_size},
    {"toInt", 1, 1, Word_toInt},
    {"toIntX", 1, 1, Word_toIntX},
    {"orb", 2, 1, Word_orb},
    {"xorb", 2, 1, Word_xorb},
    {"andb", 2, 1, Word_andb},
    {"notb", 1, 1, Word_notb},
    {"<<", 2, 1, Word_shl},
    {">>", 2, 1, Word_lsr},
    {"~>>", 2, 1, Word_asr},
    {0, 0, 0, 0}
  };
  Word::id = oz_newUniqueId();
  oz_registerExtension(Word::id, unmarshalWord);
  return interface;
}
