//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdio>
#include <sys/time.h>
#include <unistd.h>
#include "store/Store.hh"

enum NodeType {
  T_CONS,
  T_NIL
};

class ConsCell : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int CAR_POS = 0;
  static const u_int CDR_POS = 1;
public:
  using Block::ToWord;

  word Car() {
    return GetArg(CAR_POS);
  }
  word Cdr() {
    return GetArg(CDR_POS);
  }
  void SetCar(word car) {
    ReplaceArg(CAR_POS, car);
  }
  void SetCdr(word cdr) {
    ReplaceArg(CDR_POS, cdr);
  }
  static ConsCell *FromBlock(Block *x) {
    return (ConsCell *) x;
  }
  static ConsCell *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
    return FromBlock(p);
  }
  Block *ToArray(NodeType t) {
    ConsCell *c = this;
    u_int size  = 0;
    Block *p    = INVALID_POINTER;
    
    while (c != INVALID_POINTER) {
      size++;
      c = FromWord(c->Cdr());
    }
    c    = this;
    p    = Store::AllocBlock((BlockLabel) t, size);
    size = 0;

    while (c != INVALID_POINTER) {
      p->InitArg(size++, c->Car());
      c = FromWord(c->Cdr());
    }
    return p;
  }

  static Block *New(word car, word cdr) {
    Block *p = Store::AllocBlock((BlockLabel) T_CONS, SIZE);

    p->InitArg(CAR_POS, car);
    p->InitArg(CDR_POS, cdr);
    return p;
  }
};

class NilNode : private Block {
private:
  static const u_int SIZE      = 1;
  static const u_int DUMMY_POS = 0;
public:
  using Block::ToWord;

  static Block *New() {
    Block *p = Store::AllocBlock((BlockLabel) T_NIL, SIZE);

    p->InitArg(DUMMY_POS, Store::IntToWord(0));
    return p;
  }
  static NilNode *FromBlock(Block *x) {
    return (NilNode *) x;
  }
  static NilNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
    return FromBlock(p);
  }
};

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

static word CreateList(u_int n) {
  word root = NilNode::New()->ToWord();

  for (;n--;) {
    root = ConsCell::New(Store::IntToWord(n), root)->ToWord();
  }
  return root;
}

int main(void) {
  static struct timeval start_t, end_t;
  u_int memLimits[STORE_GENERATION_NUM];
  Block *p;
  word root;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);

  p = Store::AllocBlock(MIN_DATA_LABEL, 1);
  std::printf("Creating List...\n");
  Store::gcLiveMem = 0;
  Store::totalMem = 0;
  gettimeofday(&start_t, INVALID_POINTER);
  p->InitArg(0, CreateList(4000000));
  gettimeofday(&end_t, INVALID_POINTER);
  std::printf("Done\n");
  //Store::ResetTime();
  double all_time = (((end_t.tv_sec - start_t.tv_sec) * 1000) +
		    ((end_t.tv_usec - start_t.tv_usec) / 1000));
  double all_mb  = ((0.0 + Store::totalMem) / (1024 * 1024));
  double all_mss = all_mb / all_time * 1000;

  std::printf("Allocated %g MB in %g ms at %g MB/s.\n", all_mb, all_time, all_mss);
  root = p->ToWord();
  Store::MemStat();
  gettimeofday(&start_t, INVALID_POINTER);
  Store::DoGC(root);
  gettimeofday(&end_t, INVALID_POINTER);
  double gc_time = (((end_t.tv_sec - start_t.tv_sec) * 1000) +
		    ((end_t.tv_usec - start_t.tv_usec) / 1000));
  double gc_mb  = ((0.0 + Store::gcLiveMem) / (1024 * 1024));
  double gc_mss = gc_mb / gc_time * 1000;

  std::printf("Copied %g MB in %g ms at %g MB/s.\n", gc_mb, gc_time, gc_mss);
  Store::MemStat();
  std::fflush(stdout);
  return 0;
}
