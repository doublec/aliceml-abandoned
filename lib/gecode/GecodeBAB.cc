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

#include "GecodeBAB.hh"

static Space*
compact(Space* s) {
  if (s->status() == SS_FAILED)
    return s;
  Space* c = s->clone();
  delete s;
  return c;
}

SearchBestExplCallback::SearchBestExplCallback(Space* s) : b(NULL) {
  fs.push(compact(s));
}

SBResult SearchBestExplCallback::next(Space** s1, Space** s2) {
    while (true) {
      while (!fs.empty()) {
	Space* s = fs.pop();
	switch (s->status()) {
	case SS_FAILED:
	  delete s;
	  break;
	case SS_SOLVED:
	  if (b)
	    delete b;
	  b = s;
	  bs.pile(fs);
	  *s1 = b->clone();
	  return SB_SOLUTION;
	case SS_BRANCH:
	  Space* c = s->clone();
	  s->commit(1); c->commit(2);
	  fs.push(c);  
	  fs.push(s);
	}
      }
      if (bs.empty())
	break;
      Space* t = bs.pop();
      //      t->constrain(b);
      fs.push(t);
      *s1 = t;
      *s2 = b;
      return SB_CONSTRAIN;
    }
    return SB_DONE;
}
