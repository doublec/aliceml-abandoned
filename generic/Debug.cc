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
#pragma implementation "generic/Debug.hh"
#endif

#include <cstdio>
#include "store/Store.hh"
#include "generic/Debug.hh"

typedef union {
  Transient *pt;
  Chunk *pc;
  Block *pb;
  s_int pi;
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
  case INT_MAP_LABEL:
    return "INTMAP";
  case CHUNK_MAP_LABEL:
    return "CHUNKMAP";
  case MAP_LABEL:
    return "MAP";
  case WEAK_MAP_LABEL:
    return "WEAKMAP";
  case QUEUE_LABEL:
    return "QUEUE";
  case STACK_LABEL:
    return "STACK";
  case UNIQUESTRING_LABEL:
    return "UNIQUESTRING";
  case THREAD_LABEL:
    return "THREAD";
  case CLOSURE_LABEL:
    return "CLOSURE";
  case TUPLE_LABEL:
    return "TUPLE";
  case TRANSFORM_LABEL:
    return "TRANSFORM";
  case ARGS_LABEL:
    return "ARGS";
  case CONCRETE_LABEL:
    return "CONCRETE";
  case IODESC_LABEL:
    return "IODESC";
  default:
    return "UNKNOWN";
  }
}

static void
PerformDump(FILE *file, word x, int index, int level, int depth) {
  word_data w;
  
  if (depth > Debug::maxDepth) {
    std::fprintf(file, "%*c...\n", level, ' ');
  }
  else if (x == STATIC_CAST(word, NULL)) {
    std::fprintf(file, "%*cNULL POINTER[%d]\n", level, ' ', index);
  }
  else if (PointerOp::IsInt(x)) {
    w.pi = Store::WordToInt(x);
    std::fprintf(file, "%*cINT[%d]=%"S_INTF"\n", level, ' ', index, w.pi);
  }
  else if (PointerOp::IsTransient(x)) {
    Block *p = PointerOp::RemoveTag(x);
    std::fprintf(file, "%*cTRANSIENT(%s)[%d]\n", level, ' ',
		 TransLabel(p->GetLabel()), index);
    PerformDump(file, p->GetArg(0), 0, level + 2, depth + 1);
    std::fprintf(file, "%*cENDTRANSIENT\n", level, ' ');
  }
  else if ((w.pb = Store::WordToBlock(x)) != INVALID_POINTER) {
    if (w.pb->GetLabel() == CHUNK_LABEL) {
      std::fprintf(file, "%*cCHUNK(%"U_INTF")[%d]='%.*s'\n", level, ' ',
		   w.pc->GetSize(), index,
		   (int) w.pc->GetSize(), w.pc->GetBase());
    } else {
      u_int size  = w.pb->GetSize();
      std::fprintf(file, "%*cBLOCK(%s=%d, %"U_INTF")[%d]\n", level, ' ',
		   TransLabel(w.pb->GetLabel()), w.pb->GetLabel(),
		   size, index);
      u_int showSize = (size <= Debug::maxWidth ? size : Debug::maxWidth);
      for (int i = 0; i < showSize; i++) {
	PerformDump(file, w.pb->GetArg(i), i, level + 2, depth + 1);
      }
      std::fprintf(file, "%*cENDBLOCK\n", level, ' ');
    }
  }
}

// Implementation of class Debug
u_int Debug::maxWidth = 3;
u_int Debug::maxDepth = 5;

void Debug::Dump(word x) {
  PerformDump(stderr, x, 0, 2, 0);
}

void Debug::DumpTo(FILE *file, word x) {
  PerformDump(file, x, 0, 2, 0);
}
