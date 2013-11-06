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

#if defined(INTERFACE)
#pragma implementation "alice/Data.hh"
#endif

#include "alice/Data.hh"
#include "alice/Guid.hh"
#include "alice/AliceLanguageLayer.hh"

static const u_int initialTableSize = 16; // to be checked


int Alice::Compare(word x0, word x1) {
  Block *a = Store::WordToBlock(x0);
  Block *b = Store::WordToBlock(x1);
  if (a == INVALID_POINTER) { // x0 is Transient or int
    s_int i = Store::WordToInt(x0);
    if (i == INVALID_INT) { // x0 is transient
      Scheduler::SetCurrentData(x0);
      return -1;
    }
    // x0 is int
    if (b != INVALID_POINTER) // x1 is Block
      {
        // test if x1 is a bigInt
        if (b->GetLabel()==CONCRETE_LABEL) {
          ConcreteRepresentation *cr =
            reinterpret_cast<ConcreteRepresentation *>(b);
          if (cr->GetHandler()==PrimitiveTable::gmpHandler) {
            return BigInt::FromWordDirect(cr->Get(0))->compare(i)==0;
          }
        }
        return 0;
      }
    // x1 is Transient or int
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::SetCurrentData(x1);
      return -1;
    }
    return (i == j);
  }
  if (b == INVALID_POINTER) { // x1 is Transient or int, but x0 is a block
    s_int j = Store::WordToInt(x1);
    if (j == INVALID_INT) { // x1 is Transient
      Scheduler::SetCurrentData(x1);
      return -1;
    } // x1 is int
    // test if x0 is a bigInt
    if (a->GetLabel()==CONCRETE_LABEL) {
      ConcreteRepresentation *cr =
        reinterpret_cast<ConcreteRepresentation *>(a);
      if (cr->GetHandler()==PrimitiveTable::gmpHandler) {
        return BigInt::FromWordDirect(cr->Get(0))->compare(j)==0;
      }
    }
    return 0;
  }
  // from here, both x0 and x1 are blocks
  BlockLabel label = a->GetLabel();
  if (label != b->GetLabel())
    return 0;
  switch (label) {
  case TUPLE_LABEL:
  case Alice::ConVal:
  case Alice::Vector:
    {
    structural_equality:
      u_int size = a->GetSize();
      if (b->GetSize() != size)
	return 0;
      for (u_int i = 0; i < size; i++) { // request left-to-right
	int result = Compare(a->GetArg(i), b->GetArg(i));
	if (result != 1)
	  return result;
      }
      return 1;
    }
  case CONCRETE_LABEL:
    {
      ConcreteRepresentation *ac = reinterpret_cast<ConcreteRepresentation *>(a);
      ConcreteRepresentation *bc = reinterpret_cast<ConcreteRepresentation *>(b);
      if (ac->GetHandler()==PrimitiveTable::gmpHandler &&
          ac->GetHandler()==bc->GetHandler()) {
        BigInt *a = BigInt::FromWordDirect(ac->Get(0));
        BigInt *b = BigInt::FromWordDirect(bc->Get(0));
        return (a->compare(b) == 0);
      } else {
        return (ac==bc);
      }
    }
  case CHUNK_LABEL:
    {
      if (a->IsMutable()) {
	Assert(b->IsMutable());
	return (a == b);
      }
      Chunk *ac = reinterpret_cast<Chunk *>(a);
      Chunk *bc = reinterpret_cast<Chunk *>(b);
      u_int size = ac->GetSize();
      return bc->GetSize() == size &&
	!std::memcmp(ac->GetBase(), bc->GetBase(), size);
    }
  default:
    if (a->IsMutable()) {
      Assert(b->IsMutable());
      return (a == b);
    }
    if (Alice::IsTag(label)) {
      goto structural_equality;
    } else // fall back to identity-based equality
      return (a == b);
  }
}


word Vector::empty;

void Vector::Init() {
  Block *b = Store::AllocBlock(Alice::Vector, BASE_SIZE);
  b->InitArg(LENGTH_POS, Store::IntToWord(0));
  empty = b->ToWord();
  RootSet::Add(empty);
}


word Record::empty;

void Record::Init() {
  Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE);
  b->InitArg(WIDTH_POS, Store::IntToWord(0));
  empty = b->ToWord();
  RootSet::Add(empty);
}


//
// ConstructorHandler
//

class ConstructorHandler: public ConcreteRepresentationHandler {
public:
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);
};

Transform *
ConstructorHandler::GetAbstractRepresentation(ConcreteRepresentation *b) {
  Constructor *constructor = reinterpret_cast<Constructor *>(b);
  return constructor->GetTransform();
}

//
// Constructor
//

static word wConstructorMap;

ConcreteRepresentationHandler *Constructor::handler;

void Constructor::Init() {
  handler = new ConstructorHandler();
  wConstructorMap = ChunkMap::New(initialTableSize)->ToWord();
  RootSet::Add(wConstructorMap);
}

static Transform *MakeConstructorTransform(String *name, word key) {
  Tuple *tuple = Tuple::New(2);
  tuple->Init(0, name->ToWord());
  tuple->Init(1, key);
  String *string =
    String::FromWordDirect(AliceLanguageLayer::TransformNames::constructor);
  Chunk *transformName = Store::DirectWordToChunk(string->ToWord());
  return Transform::New(transformName, tuple->ToWord());
}

Constructor *Constructor::New(String *name, ::Block *guid) {
  Assert(guid != INVALID_POINTER);
  ChunkMap *constructorMap = ChunkMap::FromWordDirect(wConstructorMap);
  word wKey = guid->ToWord();
  if (constructorMap->IsMember(wKey)) {
    return Constructor::FromWordDirect(constructorMap->Get(wKey));
  } else {
    ConcreteRepresentation *b = ConcreteRepresentation::New(handler, SIZE);
    b->Init(NAME_POS, name->ToWord());
    b->Init(TRANSFORM_POS, MakeConstructorTransform(name, wKey)->ToWord());
    constructorMap->Put(wKey, b->ToWord());
    return static_cast<Constructor *>(b);
  }
}

Transform *Constructor::GetTransform() {
  word transformWord = Get(TRANSFORM_POS);
  if (transformWord == Store::IntToWord(0)) {
    Transform *transform =
      MakeConstructorTransform(GetName(), Guid::New()->ToWord());
    Replace(TRANSFORM_POS, transform->ToWord());
    return transform;
  } else {
    return Transform::FromWordDirect(transformWord);
  }
}

//
// Record
//

void Record::Init(const char *s, word value) {
  UniqueString *label = UniqueString::New(String::New(s));
  u_int n = Store::DirectWordToInt(GetArg(WIDTH_POS));
  Assert(n != 0);
  u_int index = label->Hash() % n;
  u_int i = index;
  while (true) {
    if (GetArg(BASE_SIZE + i * 2) == Store::IntToWord(0)) {
      InitArg(BASE_SIZE + i * 2, label->ToWord());
      InitArg(BASE_SIZE + i * 2 + 1, value);
      return;
    }
    i = (i + 1) % n;
    Assert(i != index);
  }
}

//
// BigInt
//

BigInt* BigInt::New(void) {
  Chunk *c = Store::AllocChunk(SIZE);
  MP_INT *big = reinterpret_cast<MP_INT*>(c->GetBase());
  mpz_init(big);
  return static_cast<BigInt *>(c);
}
BigInt* BigInt::New(BigInt *old) {
  Chunk *c = Store::AllocChunk(SIZE);
  MP_INT *big = reinterpret_cast<MP_INT*>(c->GetBase());
  mpz_init_set(big, old->big());
  return static_cast<BigInt *>(c);
}
BigInt* BigInt::New(s_int i) {
  Chunk *c = Store::AllocChunk(SIZE);
  MP_INT *big = reinterpret_cast<MP_INT*>(c->GetBase());
  mpz_init_set_si(big, i);
  return static_cast<BigInt *>(c);
}
BigInt* BigInt::New(u_int i) {
  Chunk *c = Store::AllocChunk(SIZE);
  MP_INT *big = reinterpret_cast<MP_INT*>(c->GetBase());
  mpz_init_set_ui(big, i);
  return static_cast<BigInt *>(c);
}
BigInt* BigInt::New(double d) {
  Chunk *c = Store::AllocChunk(SIZE);
  MP_INT *big = reinterpret_cast<MP_INT*>(c->GetBase());
  mpz_init_set_d(big, d);
  return static_cast<BigInt *>(c);
}

s_int BigInt::toInt(void) {
  MP_INT *b = big();
  if (mpz_fits_slong_p(b)) {
    long i = mpz_get_si(b);
    if (i>MAX_VALID_INT || i<MIN_VALID_INT) {
      return INVALID_INT;
    } else {
      return i;
    }
  } else {
    return INVALID_INT;
  }
}
void BigInt::destroy(void) {
  MP_INT *b = big();
  mpz_clear(b);
}

bool BigInt::operator==(long i) {
  return mpz_cmp_si(big(), i)==0;
}

#define MKOP1(op, mpop) \
  BigInt *BigInt::op(void) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big()); \
    return n; \
  }
  MKOP1(negate, mpz_neg);
  MKOP1(abs, mpz_abs);
  MKOP1(notb, mpz_com);
#undef MKOP1

#define MKOP2(op, mpop) \
  BigInt *BigInt::op(BigInt *b) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big(), b->big()); \
    return n; \
  } \
  BigInt *BigInt::op(unsigned long i) { \
    BigInt *n = BigInt::New(); \
    mpop ## _ui(n->big(), big(), i); \
    return n; \
  }
  MKOP2(add, mpz_add);
  MKOP2(sub, mpz_sub);
#undef MKOP2

BigInt *BigInt::mul(BigInt *b) {
  BigInt *n = BigInt::New();
  mpz_mul(n->big(), big(), b->big());
  return n;
}
BigInt *BigInt::mul(long i) {
  BigInt *n = BigInt::New();
  mpz_mul_si(n->big(), big(), i);
  return n;
}

#define MKOP2(op, mpop) \
  BigInt *BigInt::op(BigInt *b) { \
    BigInt *n = BigInt::New(); \
    mpop(n->big(), big(), b->big()); \
    return n; \
  } \
  BigInt *BigInt::op(MP_INT *b) { \
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

void BigInt::divMod(BigInt *b, BigInt *d, BigInt *m) {
  mpz_fdiv_qr(d->big (), m->big (), big(), b->big());
}
void BigInt::quotRem(BigInt *b, BigInt *q, BigInt *r) {
  mpz_tdiv_qr(q->big (), r->big (), big(), b->big());
}

BigInt *BigInt::pow(unsigned long exp) {
  BigInt *n = BigInt::New();
  mpz_pow_ui(n->big(), big(), exp);
  return n;
}

unsigned long BigInt::log2(void) {
  return mpz_sizeinbase(big(), 2)-1;
}

BigInt *BigInt::shiftr(unsigned long b) {
  BigInt *n = BigInt::New();
  mpz_fdiv_q_2exp(n->big(), big(), b);
  return n;
}

BigInt *BigInt::shiftl(unsigned long b) {
  BigInt *n = BigInt::New();
  mpz_mul_2exp(n->big(), big(), b);
  return n;
}

int BigInt::compare(BigInt *b) {
  if (this==b) return 0;
  return mpz_cmp(big(), b->big());
}
int BigInt::compare(long i) {
  return mpz_cmp_si(big(), i);
}
bool BigInt::less(BigInt *b) {
  return mpz_cmp(big(), b->big()) < 0;
}
bool BigInt::lessEq(BigInt *b) {
  return mpz_cmp(big(), b->big()) <= 0;
}
bool BigInt::greater(BigInt *b) {
  return mpz_cmp(big(), b->big()) > 0;
}
bool BigInt::greaterEq(BigInt *b) {
  return mpz_cmp(big(), b->big()) >= 0;
}

bool BigInt::less(long i) {
  return mpz_cmp_si(big(), i) < 0;
}
bool BigInt::lessEq(long i) {
  return mpz_cmp_si(big(), i) <= 0;
}
bool BigInt::greater(long i) {
  return mpz_cmp_si(big(), i) > 0;
}
bool BigInt::greaterEq(long i) {
  return mpz_cmp_si(big(), i) >= 0;
}
