//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__DATA_HH__
#define __ALICE__DATA_HH__

#if defined(INTERFACE)
#pragma interface "alice/Data.hh"
#endif

#include <cstring>
#include "alice/Base.hh"

#include <gmp.h>

class Transform;

class AliceDll Alice {
public:
  static const BlockLabel Array   = MIN_DATA_LABEL;
  static const BlockLabel Cell    = (BlockLabel) (MIN_DATA_LABEL + 1);
  static const BlockLabel ConVal  = (BlockLabel) (MIN_DATA_LABEL + 2);
  static const BlockLabel Record  = (BlockLabel) (MIN_DATA_LABEL + 3);
  static const BlockLabel Vector  = (BlockLabel) (MIN_DATA_LABEL + 4);
  static const BlockLabel BIG_TAG = (BlockLabel) (MIN_DATA_LABEL + 5);
  static const BlockLabel MIN_TAG = (BlockLabel) (MIN_DATA_LABEL + 6);
  static const BlockLabel MAX_TAG = MAX_DATA_LABEL;

  static bool IsTag(BlockLabel l) {
    return l >= MIN_TAG && l <= MAX_TAG;
  }
  static bool IsBigTagVal(u_int nLabels) {
    return (MIN_TAG + nLabels > MAX_TAG);
  }
  static BlockLabel TagToLabel(u_int l) {
    Assert(l <= MAX_TAG - MIN_TAG);
    return Store::MakeLabel(MIN_TAG + l);
  }
  static u_int LabelToTag(BlockLabel l) {
    Assert(IsTag(l));
    return l - MIN_TAG;
  }
};

class AliceDll Array: private Block {
private:
  enum { LENGTH_POS, BASE_SIZE };
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b = Store::AllocBlock(Alice::Array, BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return STATIC_CAST(Array *, b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Array);
    return STATIC_CAST(Array *, b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Array);
    return STATIC_CAST(Array *, b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void Update(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class AliceDll Cell: private Block {
protected:
  enum { VALUE_POS, SIZE };
public:
  using Block::ToWord;

  static Cell *New() {
    return STATIC_CAST(Cell *, Store::AllocBlock(Alice::Cell, SIZE));
  }
  static Cell *New(word value) {
    Block *b = Store::AllocBlock(Alice::Cell, SIZE);
    b->InitArg(VALUE_POS, value);
    return STATIC_CAST(Cell *, b);
  }
  static Cell *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Cell);
    return STATIC_CAST(Cell *, b);
  }
  static Cell *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Cell);
    return STATIC_CAST(Cell *, b);
  }

  void Init(word value) {
    InitArg(VALUE_POS, value);
  }
  void Assign(word value) {
    ReplaceArg(VALUE_POS, value);
  }
  word Access() {
    return GetArg(VALUE_POS);
  }
  word Exchange(word value) {
    word val = GetArg(VALUE_POS);
    ReplaceArg(VALUE_POS, value);
    return val;
  }
};

class AliceDll Constructor: private ConcreteRepresentation {
private:
  enum { NAME_POS, TRANSFORM_POS, SIZE };
  static ConcreteRepresentationHandler *handler;
public:
  using ConcreteRepresentation::ToWord;

  static void Init();

  static Constructor *New(String *name, ::Block *guid);
  static Constructor *New(String *name) {
    ConcreteRepresentation *b = ConcreteRepresentation::New(handler, SIZE);
    b->Init(NAME_POS, name->ToWord());
    b->Init(TRANSFORM_POS, Store::IntToWord(0)); // constructed lazily
    return STATIC_CAST(Constructor *, b);
  }
  static Constructor *FromWord(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWord(x);
    Assert(b == INVALID_POINTER || b->GetSize() == SIZE);
    return STATIC_CAST(Constructor *, b);
  }
  static Constructor *FromWordDirect(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWordDirect(x);
    Assert(b->GetSize() == SIZE);
    return STATIC_CAST(Constructor *, b);
  }

  String *GetName() {
    return String::FromWordDirect(Get(NAME_POS));
  }
  Transform *GetTransform();
};

class AliceDll ConVal: private Block {
protected:
  enum { CON_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static ConVal *New(Block *constructor, u_int n) {
    Block *b = Store::AllocBlock(Alice::ConVal, BASE_SIZE + n);
    b->InitArg(CON_POS, constructor->ToWord());
    return STATIC_CAST(ConVal *, b);
  }
  static ConVal *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == UNIQUESTRING_LABEL ||
	   b->GetLabel() == CONCRETE_LABEL);
    return STATIC_CAST(ConVal *, b);
  }
  static ConVal *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == UNIQUESTRING_LABEL ||
	   b->GetLabel() == CONCRETE_LABEL);
    return STATIC_CAST(ConVal *, b);
  }

  bool IsConVal() { // as opposed to a Constructor, see FromWord
    return GetLabel() == Alice::ConVal;
  }
  Block *GetConstructor() {
    Assert(GetLabel() == Alice::ConVal);
    return Store::DirectWordToBlock(GetArg(CON_POS));
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(BASE_SIZE + n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Sel(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

#define Real Double

class AliceDll TagVal: private Block {
public:
  using Block::ToWord;

  static TagVal *New(u_int tag, u_int n) {
    return STATIC_CAST(TagVal *, Store::AllocBlock(Alice::TagToLabel(tag), n));
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || Alice::IsTag(p->GetLabel()));
    return STATIC_CAST(TagVal *, p);
  }
  static TagVal *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(Alice::IsTag(p->GetLabel()));
    return STATIC_CAST(TagVal *, p);
  }

  u_int GetTag() {
    return Alice::LabelToTag(GetLabel());
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(index, value);
  }
  word Sel(u_int index) {
    return GetArg(index);
  }
  static u_int GetOffset() {
    return 0;
  }
};

class AliceDll BigTagVal: private Block {
protected:
  enum { BIG_TAG_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static BigTagVal *New(u_int tag, u_int n) {
    Block *b = Store::AllocBlock(Alice::BIG_TAG, n + BASE_SIZE);
    b->InitArg(BIG_TAG_POS, Store::IntToWord(tag));
    return STATIC_CAST(BigTagVal *, b);
  }
  static BigTagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == Alice::BIG_TAG);
    return STATIC_CAST(BigTagVal *, p);
  }
  static BigTagVal *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == Alice::BIG_TAG);
    return STATIC_CAST(BigTagVal *, p);
  }

  u_int GetTag() {
    return Store::DirectWordToInt(GetArg(BIG_TAG_POS));
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(BASE_SIZE + n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Sel(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
  static u_int GetOffset() {
    return BASE_SIZE;
  }
};

class AliceDll UniqueConstructor: public Constructor {
public:
  static UniqueConstructor *New(String *name, String *id) {
    return STATIC_CAST(UniqueConstructor *, Constructor::New(name, Store::DirectWordToBlock(id->ToWord())));
  }
  static UniqueConstructor *New(const char *name, const char *id) {
    return STATIC_CAST(UniqueConstructor *, Constructor::New(String::New(name),
			Store::DirectWordToBlock(String::New(id)->ToWord())));
  }
  static UniqueConstructor *FromWord(word x) {
    return STATIC_CAST(UniqueConstructor *, Constructor::FromWord(x));
  }
  static UniqueConstructor *FromWordDirect(word x) {
    return STATIC_CAST(UniqueConstructor *, Constructor::FromWordDirect(x));
  }
};

class AliceDll Vector: private Block {
protected:
  enum { LENGTH_POS, BASE_SIZE };
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Vector *New(u_int length) {
    Block *b =
      Store::AllocBlock(Alice::Vector, BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return STATIC_CAST(Vector *, b);
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Vector);
    return STATIC_CAST(Vector *, b);
  }
  static Vector *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Vector);
    return STATIC_CAST(Vector *, b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class AliceDll Word8Array: private String {
  //--** deriving from String yields wrong equality (must be token equality)
public:
  static const u_int maxLen = String::maxSize;

  using String::ToWord;
  using String::GetValue;

  static Word8Array *New(u_int length) {
    return STATIC_CAST(Word8Array *, String::New(length));
  }
  static Word8Array *FromWord(word x) {
    return STATIC_CAST(Word8Array *, String::FromWord(x));
  }
  static Word8Array *FromWordDirect(word x) {
    return STATIC_CAST(Word8Array *, String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = i;
  }
  void Update(u_int index, word value) {
    Init(index, value);
  }
  word Sub(u_int index) {
    return Store::IntToWord(GetValue()[index]);
  }
};

class AliceDll Word8Vector: private String {
public:
  static const u_int maxLen = String::maxSize;

  using String::ToWord;
  using String::GetValue;

  static Word8Vector *New(u_int length) {
    return STATIC_CAST(Word8Vector *, String::New(length));
  }
  static Word8Vector *FromWord(word x) {
    return STATIC_CAST(Word8Vector *, String::FromWord(x));
  }
  static Word8Vector *FromWordDirect(word x) {
    return STATIC_CAST(Word8Vector *, String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = i;
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    Init(index, value);
  }
  word Sub(u_int index) {
    return Store::IntToWord(GetValue()[index]);
  }
};

class AliceDll Record: private Block {
protected:
  enum { WIDTH_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static Record *New(u_int n) {
    Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE + n * 2);
    b->InitArg(WIDTH_POS, n);
    return STATIC_CAST(Record *, b);
  }
  static Record *New(Vector *labels) {
    u_int n = labels->GetLength();
    Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE + n * 2);
    b->InitArg(WIDTH_POS, n);
    for (u_int i = n; i--; ) {
      UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
      b->InitArg(BASE_SIZE + i * 2, label->ToWord());
    }
    return STATIC_CAST(Record *, b);
  }
  static Record *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == Alice::Record);
    return STATIC_CAST(Record *, p);
  }
  static Record *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == Alice::Record);
    return STATIC_CAST(Record *, p);
  }

  void Init(u_int i, word value) {
    InitArg(BASE_SIZE + i * 2 + 1, value);
  }
  void Init(const char *s, word value);
  void AssertLabel(u_int i, UniqueString *label) {
    Assert(GetArg(BASE_SIZE + i * 2) == label->ToWord()); i = i; label = label;
  }
  word PolySel(UniqueString *label) {
    word wLabel = label->ToWord();
    u_int n = Store::DirectWordToInt(GetArg(WIDTH_POS));
    Assert(n != 0);
    u_int index = label->Hash() % n;
    u_int i = index;
    while (true) {
      if (GetArg(BASE_SIZE + i * 2) == wLabel)
	return GetArg(BASE_SIZE + i * 2 + 1);
      i = (i + 1) % n;
      Assert(i != index);
    }
  }
};

class AliceDll BigInt : private Chunk {
private:
  static const int SIZE = sizeof(MP_INT);
public:
  using Chunk::ToWord;
  static BigInt *New(void) {
    Chunk *c = Store::AllocChunk(SIZE);
    MP_INT *big = STATIC_CAST(MP_INT*, c->GetBase());
    mpz_init(big);
    return STATIC_CAST(BigInt *, c);
  }
  static BigInt *New(BigInt *old) {
    Chunk *c = Store::AllocChunk(SIZE);
    MP_INT *big = STATIC_CAST(MP_INT*, c->GetBase());
    mpz_init_set(big, old->big());
    return STATIC_CAST(BigInt *, c);
  }
  static BigInt *New(int i) {
    Chunk *c = Store::AllocChunk(SIZE);
    MP_INT *big = STATIC_CAST(MP_INT*, c->GetBase());
    mpz_init_set_si(big, i);
    return STATIC_CAST(BigInt *, c);
  }
  static BigInt *New(unsigned int i) {
    Chunk *c = Store::AllocChunk(SIZE);
    MP_INT *big = STATIC_CAST(MP_INT*, c->GetBase());
    mpz_init_set_ui(big, i);
    return STATIC_CAST(BigInt *, c);
  }
  static BigInt *New(double d) {
    Chunk *c = Store::AllocChunk(SIZE);
    MP_INT *big = STATIC_CAST(MP_INT*, c->GetBase());
    mpz_init_set_d(big, d);
    return STATIC_CAST(BigInt *, c);
  }
  static BigInt *FromWordDirect(word x) {
    Chunk *c = Store::DirectWordToChunk(x);
    Assert(c->GetSize() == SIZE);
    return STATIC_CAST(BigInt *, c);
  }
  MP_INT *big(void) { 
    return STATIC_CAST(MP_INT*, this->GetBase());
  }
  int toInt(void) {
    MP_INT *b = big();
    if (mpz_fits_slong_p(b)) {
      long int i = mpz_get_si(b);
      if (i>MAX_VALID_INT || i<MIN_VALID_INT) {
        return INVALID_INT;
      } else {
        return i;
      }
    } else {
      return INVALID_INT;
    }
  }
  void destroy(void) {
    MP_INT *b = big();
    mpz_clear(b);
  }

  bool operator==(int i) {
    return mpz_cmp_si(big(), i)==0;
  }

#define MKOP1(op, mpop) \
  BigInt *op(void) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big()); \
    return n; \
  }
  MKOP1(negate, mpz_neg);
  MKOP1(abs, mpz_abs);
  MKOP1(notb, mpz_com);
#undef MKOP1

#define MKOP2(op, mpop) \
  BigInt *op(BigInt *b) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big(), b->big()); \
    return n; \
  } \
  BigInt *op(unsigned long int i) { \
    BigInt *n = BigInt::New(); \
    mpop ## _ui(n->big(), big(), i); \
    return n; \
  }
  MKOP2(add, mpz_add);
  MKOP2(sub, mpz_sub);
#undef MKOP2

  BigInt *mul(BigInt *b) {
    BigInt *n = BigInt::New();
    mpz_mul(n->big(), big(), b->big());
    return n;
  }
  BigInt *mul(long int i) {
    BigInt *n = BigInt::New();
    mpz_mul_si(n->big(), big(), i);
    return n;
  }

#define MKOP2(op, mpop) \
  BigInt *op(BigInt *b) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big(), b->big()); \
    return n; \
  } \
  BigInt *op(MP_INT *b) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big(), b); \
    return n; \
  }
  MKOP2(div, mpz_fdiv_q);
  MKOP2(mod, mpz_fdiv_r);
  MKOP2(quot, mpz_tdiv_q);
  MKOP2(rem, mpz_tdiv_r);
  MKOP2(orb, mpz_ior);
  MKOP2(xorb, mpz_xor);
  MKOP2(andb, mpz_and);
#undef MKOP2

  void divMod(BigInt *b, BigInt *d, BigInt *m) {
    mpz_fdiv_qr(big(), b->big(), d->big(), m->big());
  }
  void quotRem(BigInt *b, BigInt *q, BigInt *r) {
    mpz_tdiv_qr(big(), b->big(), q->big(), r->big());
  }

  BigInt *pow(BigInt *exp) {
    BigInt *n = BigInt::New();
    mpz_t mod; mpz_init_set_ui(mod, 1);
    mpz_powm(n->big(), big(), exp->big(), mod);
    return n;
  }

  unsigned long int log2(void) {
    return mpz_sizeinbase(big(), 2)-1;
  }

  BigInt *shiftr(unsigned long int b) {
    BigInt *n = BigInt::New();
    mpz_fdiv_q_2exp(n->big(), big(), b);
    return n;
  }

  BigInt *shiftl(unsigned long int b) {
    BigInt *n = BigInt::New();
    mpz_mul_2exp(n->big(), big(), b);
    return n;
  }

  int compare(BigInt *b) {
    if (this==b) return 0;
    return mpz_cmp(big(), b->big());
  }
  int compare(int i) {
    return mpz_cmp_si(big(), i);
  }
  bool less(BigInt *b) {
    return mpz_cmp(big(), b->big()) < 0;
  }
  bool lessEq(BigInt *b) {
    return mpz_cmp(big(), b->big()) <= 0;
  }
  bool greater(BigInt *b) {
    return mpz_cmp(big(), b->big()) > 0;
  }
  bool greaterEq(BigInt *b) {
    return mpz_cmp(big(), b->big()) >= 0;
  }

  bool less(int i) {
    return mpz_cmp_si(big(), i) < 0;
  }
  bool lessEq(int i) {
    return mpz_cmp_si(big(), i) <= 0;
  }
  bool greater(int i) {
    return mpz_cmp_si(big(), i) > 0;
  }
  bool greaterEq(int i) {
    return mpz_cmp_si(big(), i) >= 0;
  }

};

#endif
