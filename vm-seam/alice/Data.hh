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

#include "alice/Base.hh"


class Transform;

inline s_int mydiv(s_int a, s_int b) {
  // This function is only here to bypass a constant folding bug in g++.
  // If we define RETURN_WORD as
  //   RETURN_INT(static_cast<s_int>((w) * NONBITS_EXP) / NONBITS_EXP)
  // then RETURN_WORD(0x80000000) evaluates to RETURN_WORD(0x80000000)
  // instead of RETURN_WORD(0).
  return a / b;
}

class AliceDll Alice {
public:
  static const BlockLabel Array   = MIN_DATA_LABEL;
  static const BlockLabel Promise = static_cast<BlockLabel>(MIN_DATA_LABEL + 1);
  static const BlockLabel Cell    = static_cast<BlockLabel>(MIN_DATA_LABEL + 2);
  static const BlockLabel ConVal  = static_cast<BlockLabel>(MIN_DATA_LABEL + 3);
  static const BlockLabel Record  = static_cast<BlockLabel>(MIN_DATA_LABEL + 4);
  static const BlockLabel Vector  = static_cast<BlockLabel>(MIN_DATA_LABEL + 5);
  static const BlockLabel BIG_TAG = static_cast<BlockLabel>(MIN_DATA_LABEL + 6);
  static const BlockLabel MIN_TAG = static_cast<BlockLabel>(MIN_DATA_LABEL + 7);
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
    Block *b = Store::AllocMutableBlock(Alice::Array, BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Array);
    return static_cast<Array *>(b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Array);
    return static_cast<Array *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    Assert(index <= Store::DirectWordToInt(GetArg(LENGTH_POS)));
    InitArg(BASE_SIZE + index, value);
  }
  void Update(u_int index, word value) {
    Assert(index <= Store::DirectWordToInt(GetArg(LENGTH_POS)));
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    Assert(index <= Store::DirectWordToInt(GetArg(LENGTH_POS)));
    return GetArg(BASE_SIZE + index);
  }
};

class AliceDll Promise: private Block {
protected:
  enum { FLAG_POS, FUTURE_POS, SIZE };
public:
  using Block::ToWord;

  static Promise *New() {
    Block *b = Store::AllocMutableBlock(Alice::Promise, SIZE);
    b->InitArg(FLAG_POS, Store::IntToWord(false));
    b->InitArg(FUTURE_POS, Future::New()->ToWord());
    return static_cast<Promise *>(b);
  }
  static Promise *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Promise);
    return static_cast<Promise *>(b);
  }
  static Promise *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Promise);
    return static_cast<Promise *>(b);
  }

  bool IsFulfilled() {
    return Store::WordToInt(GetArg(FLAG_POS));
  }
  word GetFuture() {
    return GetArg(FUTURE_POS);
  }
  bool Fulfill(word value) {
    if (IsFulfilled()) return false;
    ReplaceArg(FLAG_POS, Store::IntToWord(true));
    Future *future =
      static_cast<Future *>(Store::WordToTransient(GetArg(FUTURE_POS)));
    future->ScheduleWaitingThreads();
    future->Become(REF_LABEL, value);
    return true;
  }
  bool Fail(word exn) {
    if (IsFulfilled()) return false;
    ReplaceArg(FLAG_POS, Store::IntToWord(true));
    Future *future =
      static_cast<Future *>(Store::WordToTransient(GetArg(FUTURE_POS)));
    Tuple *package = Tuple::New(2);
    Backtrace *backtrace = Backtrace::New(); // TODO: have primitive in BT
    package->Init(0, exn);
    package->Init(1, backtrace->ToWord());
    future->ScheduleWaitingThreads();
    future->Become(CANCELLED_LABEL, package->ToWord());
    return true;
  }
};

class AliceDll Cell: private Block {
protected:
  enum { VALUE_POS, SIZE };
public:
  using Block::ToWord;

  static Cell *New() {
    return static_cast<Cell *>(Store::AllocMutableBlock(Alice::Cell, SIZE));
  }
  static Cell *New(word value) {
    Block *b = Store::AllocMutableBlock(Alice::Cell, SIZE);
    b->InitArg(VALUE_POS, value);
    return static_cast<Cell *>(b);
  }
  static Cell *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Cell);
    return static_cast<Cell *>(b);
  }
  static Cell *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Cell);
    return static_cast<Cell *>(b);
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
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWord(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWord(x);
    Assert(b == INVALID_POINTER || b->GetSize() == SIZE);
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWordDirect(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWordDirect(x);
    Assert(b->GetSize() == SIZE);
    return static_cast<Constructor *>(b);
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
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == UNIQUESTRING_LABEL ||
	   b->GetLabel() == CONCRETE_LABEL);
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == UNIQUESTRING_LABEL ||
	   b->GetLabel() == CONCRETE_LABEL);
    return static_cast<ConVal *>(b);
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
    return static_cast<TagVal *>(Store::AllocBlock(Alice::TagToLabel(tag), n));
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || Alice::IsTag(p->GetLabel()));
    return static_cast<TagVal *>(p);
  }
  static TagVal *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(Alice::IsTag(p->GetLabel()));
    return static_cast<TagVal *>(p);
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
    return static_cast<BigTagVal *>(b);
  }
  static BigTagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == Alice::BIG_TAG);
    return static_cast<BigTagVal *>(p);
  }
  static BigTagVal *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == Alice::BIG_TAG);
    return static_cast<BigTagVal *>(p);
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
    return static_cast<UniqueConstructor *>(Constructor::New(name, Store::DirectWordToBlock(id->ToWord())));
  }
  static UniqueConstructor *New(const char *name, const char *id) {
    return static_cast<UniqueConstructor *>(Constructor::New(String::New(name),
			Store::DirectWordToBlock(String::New(id)->ToWord())));
  }
  static UniqueConstructor *FromWord(word x) {
    return static_cast<UniqueConstructor *>(Constructor::FromWord(x));
  }
  static UniqueConstructor *FromWordDirect(word x) {
    return static_cast<UniqueConstructor *>(Constructor::FromWordDirect(x));
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
    return static_cast<Vector *>(b);
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Vector);
    return static_cast<Vector *>(b);
  }
  static Vector *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Vector);
    return static_cast<Vector *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    ReplaceArgUnchecked(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class AliceDll Word8Array: private Chunk {
public:
  static const u_int maxLen = String::maxSize;

  using Chunk::ToWord;

  static Word8Array *New(u_int length) {
    Chunk *c = Store::AllocMutableChunk(length);
    return static_cast<Word8Array *>(c);
  }
  static Word8Array *FromWord(word x) {
    return static_cast<Word8Array *>(Store::WordToChunk(x));
  }
  static Word8Array *FromWordDirect(word x) {
    return static_cast<Word8Array *>(Store::DirectWordToChunk(x));
  }

  u_char *GetValue() {
    return reinterpret_cast<u_char *>(GetBase());
  }
  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= -128 && i <= 127);
    GetValue()[index] = static_cast<u_char>(i);
  }
  void InitChunk(u_int index, u_int size, const u_char *chunk) {
    Assert(index + size <= GetSize());
    u_char *base = GetValue();
    memcpy(base + index, chunk, size);
  }
  void Update(u_int index, word value) {
    Init(index, value);
  }
  void UpdateChunk(u_int index, u_int size, const u_char *chunk) {
    InitChunk(index, size, chunk);
  }
  word Sub(u_int index) {
    return Store::IntToWord(static_cast<signed char>(GetValue()[index]));
  }
};

class AliceDll Word8Vector: private String {
public:
  static const u_int maxLen = String::maxSize;

  using String::ToWord;
  using String::GetValue;

  static Word8Vector *New(u_int length) {
    return static_cast<Word8Vector *>(String::New(length));
  }
  static Word8Vector *FromWord(word x) {
    return static_cast<Word8Vector *>(String::FromWord(x));
  }
  static Word8Vector *FromWordDirect(word x) {
    return static_cast<Word8Vector *>(String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= -128 && i <= 127);
    GetValue()[index] = static_cast<u_char>(i);
  }
  void InitChunk(u_int index, u_int size, const u_char *chunk) {
    Assert(index + size <= GetSize());
    u_char *base = GetValue();
    memcpy(base + index, chunk, size);
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    Init(index, value);
  }
  word Sub(u_int index) {
    return Store::IntToWord(static_cast<signed char>(GetValue()[index]));
  }
};

class AliceDll CharArray: private Chunk {
public:
  static const u_int maxLen = String::maxSize;

  using Chunk::ToWord;

  static CharArray *New(u_int length) {
    Chunk *c = Store::AllocMutableChunk(length);
    return static_cast<CharArray *>(c);
  }
  static CharArray *FromWord(word x) {
    return static_cast<CharArray *>(Store::WordToChunk(x));
  }
  static CharArray *FromWordDirect(word x) {
    return static_cast<CharArray *>(Store::DirectWordToChunk(x));
  }

  u_char *GetValue() {
    return reinterpret_cast<u_char *>(GetBase());
  }
  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = static_cast<u_char>(i);
  }
  void InitChunk(u_int index, u_int size, const u_char *chunk) {
    Assert(index + size <= GetSize());
    u_char *base = GetValue();
    memcpy(base + index, chunk, size);
  }
  void Update(u_int index, word value) {
    Init(index, value);
  }
  void UpdateChunk(u_int index, u_int size, const u_char *chunk) {
    InitChunk(index, size, chunk);
  }
  word Sub(u_int index) {
    return Store::IntToWord(GetValue()[index]);
  }
};

class AliceDll CharVector: private String {
public:
  static const u_int maxLen = String::maxSize;

  using String::ToWord;
  using String::GetValue;

  static CharVector *New(u_int length) {
    return static_cast<CharVector *>(String::New(length));
  }
  static CharVector *FromWord(word x) {
    return static_cast<CharVector *>(String::FromWord(x));
  }
  static CharVector *FromWordDirect(word x) {
    return static_cast<CharVector *>(String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = static_cast<u_char>(i);
  }
  void InitChunk(u_int index, u_int size, const u_char *chunk) {
    Assert(index + size <= GetSize());
    u_char *base = GetValue();
    memcpy(base + index, chunk, size);
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
    for (u_int i = n; i--; ) {
      b->InitArg(BASE_SIZE + i * 2, Store::IntToWord(0));
    }
    return static_cast<Record *>(b);
  }
  static Record *New(Vector *labels) {
    u_int n = labels->GetLength();
    Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE + n * 2);
    b->InitArg(WIDTH_POS, n);
    for (u_int i = n; i--; ) {
      UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
      b->InitArg(BASE_SIZE + i * 2, label->ToWord());
    }
    return static_cast<Record *>(b);
  }
  static Record *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == Alice::Record);
    return static_cast<Record *>(p);
  }
  static Record *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == Alice::Record);
    return static_cast<Record *>(p);
  }

  void Init(u_int i, word value) {
    InitArg(BASE_SIZE + i * 2 + 1, value);
  }
  void Init(const char *s, word value);
  void AssertLabel(u_int i, UniqueString *label) {
    Assert(GetArg(BASE_SIZE + i * 2) == label->ToWord()); i = i; label = label;
  }
  
  
  /**
   * Select the value from this Record that has the specified label, or
   * exit with an error message if this Record has no such label.
   */
  word PolySel(UniqueString *label) {
    word wLabel = label->ToWord();
    u_int n = Store::DirectWordToInt(GetArg(WIDTH_POS));
    Assert(n != 0);
    u_int index = label->Hash() % n;
    u_int i = index;
    do {
      if (GetArg(BASE_SIZE + i * 2) == wLabel)
	return GetArg(BASE_SIZE + i * 2 + 1);
      i = (i + 1) % n;
    } while (i != index);
    
    fprintf(stderr, "Could not find field %s in structure ", label->ToString()->ExportC());
    this->Dump();
    Error("Aborting.");
  }
  
  
  /**
   * A convenience method over PolySel(UniqueString *)
   */
  word PolySel(String *label) {
    return this->PolySel(UniqueString::New(label));
  }
  
  
  /**
   * A convenience method over PolySel(UniqueString *)
   */
  word PolySel(const char *label) {
    return this->PolySel(String::New(label));
  }
  
  
  /**
   * Used for debugging
   */
  void Dump(){
    fprintf(stderr, "{");
    
    u_int width = Store::DirectWordToInt(GetArg(WIDTH_POS));
    for (int i=0; i<width; i++) {
      if (i) {
	    fprintf(stderr, ", ");
      }
      word lab = GetArg(BASE_SIZE + i * 2);
      if (lab == Store::IntToWord(0)) {
        fprintf(stderr, "?");
      } else {
        fprintf(stderr, "%s", UniqueString::FromWord(lab)->ToString()->ExportC());
      }
    }
    
    fprintf(stderr, "}\n");
  }
};

class AliceDll BigInt : private Chunk {
private:
  static const int SIZE = sizeof(MP_INT);
public:
  using Chunk::ToWord;
  static BigInt *New(void);
  static BigInt *New(BigInt *old);
  static BigInt *New(s_int i);
  static BigInt *New(u_int i);
  static BigInt *New(double d);
  s_int toInt(void);
  void destroy(void);
  bool operator==(long i);


  static BigInt *FromWordDirect(word x) {
    Chunk *c = Store::DirectWordToChunk(x);
    Assert(c->GetSize() == SIZE);
    return static_cast<BigInt *>(c);
  }
  MP_INT *big(void) { 
    return reinterpret_cast<MP_INT*>(this->GetBase());
  }

  BigInt *negate(void);
  BigInt *abs(void);
  BigInt *notb(void);
  
  BigInt *add(BigInt *b);
  BigInt *add(unsigned long i);
  BigInt *sub(BigInt *b);
  BigInt *sub(unsigned long i);

  BigInt *mul(BigInt *b);
  BigInt *mul(long i);

  BigInt *div(BigInt *b);
  BigInt *div(MP_INT *b);
  BigInt *mod(BigInt *b);
  BigInt *mod(MP_INT *b);
  BigInt *quot(BigInt *b);
  BigInt *quot(MP_INT *b);
  BigInt *rem(BigInt *b);
  BigInt *rem(MP_INT *b);
  BigInt *orb(BigInt *b);
  BigInt *orb(MP_INT *b);
  BigInt *xorb(BigInt *b);
  BigInt *xorb(MP_INT *b);
  BigInt *andb(BigInt *b);
  BigInt *andb(MP_INT *b);

  void divMod(BigInt *b, BigInt *d, BigInt *m);
  void quotRem(BigInt *b, BigInt *q, BigInt *r);

  BigInt *pow(unsigned long exp);

  unsigned long log2(void);

  BigInt *shiftr(unsigned long b);

  BigInt *shiftl(unsigned long b);

  int compare(BigInt *b);
  int compare(long i);
  bool less(BigInt *b);
  bool lessEq(BigInt *b);
  bool greater(BigInt *b);
  bool greaterEq(BigInt *b);

  bool less(long i);
  bool lessEq(long i);
  bool greater(long i);
  bool greaterEq(long i);

};

#endif
