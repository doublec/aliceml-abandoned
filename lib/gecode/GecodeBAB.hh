/*
 * Authors:
 *  Guido Tack <tack@ps.uni-sb.de>
 *
 * Copyright:
 *  Guido Tack, 2003
 *
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 * 
 */

#ifndef __GECODEBAB_HH__
#define __GECODEBAB_HH__

#include "gecode-search.hh"
#include "support/dynamic-stack.hh"

enum SBResult { SB_CONSTRAIN, SB_DONE, SB_SOLUTION };

class SearchBestExplCallback {
private:
  DynamicStack<Memory::ArrayAllocator<Space*>,Space*> fs;
  DynamicStack<Memory::ArrayAllocator<Space*>,Space*> bs;
  Space* b;
public:
  SearchBestExplCallback(Space* s);
  ~SearchBestExplCallback() {
    while (!fs.empty()) delete fs.pop();
    while (!bs.empty()) delete bs.pop();
    if (b) delete b;
  }
  SBResult next(Space** s1, Space** s2);
};

#endif
