//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2002
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#ifndef __GENERIC__PARTITION_HH
#define __GENERIC__PARTITION_HH

#if defined(INTERFACE)
#pragma interface "generic/Minimizer.hh"
#endif

#include "generic/Worker.hh"
#include "adt/Stack.hh"
#include "store/Store.hh"

class PBlock;

class SeamDll Partition : private Block {
private:
  static const BlockLabel PARTITION_LABEL = MIN_DATA_LABEL;
  enum {NA_POS, BA_POS, TO_DO_POS,
	BLOCK_COUNT_POS, SIZE};  

  void InitBlocks();
  PBlock *GetBlock(u_int blockNo);
  void InitAgenda(Stack *agenda);
  bool splitBlockAtNode(u_int block, u_int nodeIndex);
  void FollowBack(u_int block, u_int edge);
  void DoSplits(Stack *q);
  void ReduceGraph();

public:
  using Block::ToWord;

  static Partition *New();

  static Partition *FromWord(word x);
  static Partition *FromWordDirect(word x);

  static void Minimize(word root);

  int InsertNode(word node);
  void AddParent(int nodeIndex, int edge, int parent);
  void Minimize();
};

class SeamDll PartitionLoader {
public:

  // Static Constructor
  static void Init();

  // PartitionLoader Functions
  static Worker::Result Load(Partition *p, word x);
};

#endif
