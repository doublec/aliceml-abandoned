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
#ifndef __STORE__PARAMETER_HH__
#define __STORE__PARAMETER_HH__

//
// Configure Store Header Fields
//

// Header Width in bits (e.g. 32 or 64)
#define HEADER_FULL_WIDTH          32
// Space used for Garbage-Collection (Generation and GCMark) Flags
#define HEADER_GEN_GC_MARK_WIDTH   2
// Space used for Forward Ptrs
#define HEADER_FWDPTR_WIDTH        (HEADER_FULL_WIDTH - HEADER_GEN_GCMARK_WIDTH)
// Space used for Block Size Shift Indicator
#define HEADER_SIZESHIFT_WIDTH     1
// Space used for Block Size (MAX_BLOCKSIZE = (2 ^ (HEADER_SIZE_WIDTH) - 1))
#define HEADER_SIZE_WIDTH          20
// Space used for Block Tag (MAX_TAGSIZE = (2 ^ (HEADER_TAG_WIDTH) - 1))
#define HEADER_TAG_WIDTH           8
// Space used for Intgen-Marking (Names thanks to Christian)
#define HEADER_CHILDISH_WIDTH      1

//
// Configure Store Memory Settings
//

// Size of each allocated Memory Chunk
#define STORE_MEMCHUNK_SIZE      (1024 * 128)
// Number of Memory Generations
// (must fit in HEADER_GEN_GCMARK_WIDTH starting at 1; zero is reserved)
// to be done
#define STORE_GENERATION_NUM     ((1 << HEADER_GEN_GC_MARK_WIDTH) - 1)
// Initial Intgen-Pointer-Set Size
#define STORE_INITIAL_INTGEN     1024
// Initial Weak-Dictionary-Set Size
#define STORE_INITIAL_WKDICT     256

//
// Introduce Helper Block Label (without _LABEL suffix)
//

#define STORE_HELPER_LABEL_ARRAY \
  static const char *helper_arr[] = \
  { "GENSET", \
    "INT_MAP", "CHUNK_MAP", "MAP", \
    "HASHNODEARRAY", "HASHNODE", "HANDLEDHASHNODE", \
    "QUEUE", "QUEUEARRAY", \
    "STACK", "STACKARRAY", \
    "ROOTSETELEMENT", \
    "UNIQUESTRING", \
    "THREAD", "TASKSTACK", \
    "CLOSURE", \
    "TUPLE", \
    "CONCRETE", \
    "TRANSFORM", \
    "ARGS", \
    "IODESC", \
    "DEBUG_ENVIRONMENT" \
  }

#endif
