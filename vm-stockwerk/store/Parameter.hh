//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
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

// Header Width in bits (32 or 64)
#define HEADER_FULL_WIDTH        32
// Space used for Garbage-Collection Flags
#define HEADER_GC_MARK_WIDTH     1
// Space used for Block Tag (MAX_TAGSIZE = (2 ^ (HEADER_TAG_WIDTH) - 1))
#define HEADER_TAG_WIDTH         12
// Space used for Block Size (MAX_BLOCKSIZE = (2 ^ (HEADER_SIZE_WIDTH) - 1))
#define HEADER_SIZE_WIDTH        16
// Space used for Intgen-Marking
#define HEADER_INTGEN_MARK_WIDTH 1
// Space used for Generation
#define HEADER_GENERATION_WIDTH  2

//
// Configure Store Memory Settings
//

// Size of each allocated Memory Chunk
#define STORE_MEMCHUNK_SIZE      (1024 * 128)
// Number of Memory Generations (must fit in HEADER_GENERATION_WIDTH starting at 0)
#define STORE_GENERATION_NUM     3
// Initial Intgen-Pointer-Set Size
#define STORE_INITIAL_INTGEN     4

//
// Configure Store Performance Settings
//

// ChunkTop pointer to be stored in register (0 or 1)
#define STORE_CHUNKTOP_IN_REG    0

//
// Introduce Helper Block Label (without _LABEL suffix)
//

#define STORE_HELPER_LABEL_ARRAY \
  static const char *helper_arr[] = \
  { "GENSET", \
    "HASHTABLE", "HASHNODEARRAY", "HASHNODE", \
    "QUEUE", "QUEUEARRAY", \
    "STACK", "STACKARRAY", \
    "THREAD", \
    "CONCRETECODE", \
    "CLOSURE" \
  }

#endif __STORE__PARAMETER_HH__
