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

#include "alice/Authoring.hh"
#include "generic/Minimizer.hh"

//
// Primitives
//

DEFINE2(UnsafeMinimizer_loadGraph) {
  Partition *p = Partition::FromWord(x0);
  PUSH_PRIM_SELF();
  return PartitionLoader::Load(p, x1);
} END

DEFINE0(UnsafeMinimizer_newPartition) {
  word ret = Partition::New()->ToWord();
  RETURN(ret);
} END

DEFINE1(UnsafeMinimizer_minimize) {
  Partition::FromWord(x0)->Minimize();
  RETURN_UNIT;
} END

AliceDll word UnsafeMinimizer() {
  Record *record = Record::New(3);
  INIT_STRUCTURE(record, "UnsafeMinimizer", "newPartition",
		 UnsafeMinimizer_newPartition, 0);  
  INIT_STRUCTURE(record, "UnsafeMinimizer", "loadGraph",
		 UnsafeMinimizer_loadGraph, 2);
  INIT_STRUCTURE(record, "UnsafeMinimizer", "minimize",
		 UnsafeMinimizer_minimize, 1);  
  RETURN_STRUCTURE("UnsafeMinimizer$", record);
}
