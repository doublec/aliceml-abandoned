//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/Debug.hh"
#endif

#include <cstdio>
#include "store/Store.hh"
#include "emulator/Debug.hh"

typedef union {
  Transient *pt;
  Chunk *pc;
  Block *pb;
  int pi;
} word_data;

static const char *TransLabel(BlockLabel l) {
  switch (l) {
  case HOLE_LABEL:
    return "HOLE";
  case FUTURE_LABEL:
    return "FUTURE";
  case REF_LABEL:
    return "REF";
  case CANCELLED_LABEL:
    return "CANCELLED";
  case BYNEED_LABEL:
    return "BYNEED";
  case HASHTABLE_LABEL:
    return "HASHTABLE";
  case QUEUE_LABEL:
    return "QUEUE";
  case STACK_LABEL:
    return "STACK";
  case THREAD_LABEL:
    return "THREAD";
  case TUPLE_LABEL:
    return "TUPLE";
  case EMPTYARG_LABEL:
    return "EMPTYARG";
  case ONEARG_LABEL:
    return "ONEARG";
  case TUPARGS_LABEL:
    return "TUPARGS";
  case CLOSURE_LABEL:
    return "CLOSURE";
  case HANDLERBLOCK_LABEL:
    return "HANDLER";
  default:
    return "UNKNOWN";
  }
}

static void Print(Chunk *c) {
  fprintf(stderr, "'%.*s'\n", (int) c->GetSize(), c->GetBase());
}

static void PerformDump(word x, u_int index, u_int level, u_int depth) {
  word_data w;
  if (depth > Debug::maxDepth) {
    fprintf(stderr, "%*c...\n", level, ' ');
  }
  else if ((w.pt = Store::WordToTransient(x)) != INVALID_POINTER) {
    fprintf(stderr, "%*cTRANSIENT(%s)[%d]\n", level, ' ',
	    TransLabel(w.pb->GetLabel()), index);
    PerformDump(w.pb->GetArg(0), 0, level + 2, depth + 1);
    fprintf(stderr, "%*cENDTRANSIENT\n", level, ' ');
  }
  else if ((w.pc = Store::WordToChunk(x)) != INVALID_POINTER) {
    fprintf(stderr, "%*cCHUNK(%d)[%d]=", level, ' ',
	    w.pc->GetSize(), index);
    Print(w.pc);
  }
  else if ((w.pb = Store::WordToBlock(x)) != INVALID_POINTER) {
    u_int size  = w.pb->GetSize();
    fprintf(stderr, "%*cBLOCK(%s=%d, %d)[%d]\n", level, ' ',
	    TransLabel(w.pb->GetLabel()), w.pb->GetLabel(), size, index);
    u_int showSize = (size <= Debug::maxWidth ? size : Debug::maxWidth);
    for (u_int i = 0; i < showSize; i++) {
      PerformDump(w.pb->GetArg(i), i, level + 2, depth + 1);
    }
    fprintf(stderr, "%*cENDBLOCK\n", level, ' ');
  }
  // Assume Int
  else {
    w.pi = Store::WordToInt(x);
    fprintf(stderr, "%*cINT[%d]=%d\n", level, ' ', index, w.pi);
  }
}

u_int Debug::maxWidth = 190;
u_int Debug::maxDepth = 6;

void Debug::Dump(word x) {
  PerformDump(x, 0, 0, 0);
}
