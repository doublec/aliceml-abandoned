//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTECODE_INLINER_HH__
#define __ALICE_BYTECODE_INLINER_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeInliner.hh"
#endif

#include "alice/Authoring.hh"
#include "alice/AbstractCode.hh"

class LazyCompileClosure;

//#define INLINE_LIMIT 5 moved to ByteCodeInliner.cc

namespace ByteCodeInliner_Internal {
  static inline u_int GetNumberOfLocals(TagVal *abstractCode) {
    TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
    switch (AbstractCode::GetAnnotation(annotation)) {
    case AbstractCode::Simple:
      return Store::DirectWordToInt(annotation->Sel(0));
    case AbstractCode::Debug:
      return Vector::FromWordDirect(annotation->Sel(0))->GetLength();
    }
  }

  class Container {
  protected:
    u_int size;
    u_int top;
    Tuple *container;

    bool CheckResize() {
      if(top >= size) {
	u_int newSize = size * 3 / 2;
	Tuple *newContainer = Tuple::New(newSize);
	for(u_int i=size; i--; ) 
	  newContainer->Init(i,container->Sel(i));
	size = newSize;
	container = newContainer;
	return true;
      }
      return false;
    }
  public:
    Container() : size(10), top(0) {       
      container = Tuple::New(size); 
    }
    void Append(word item) {
      CheckResize();
      container->Init(top++,item);
    }
    word Sub(u_int i) { return container->Sel(i); }
    u_int GetLength() { return top; }
  };
  class LivenessContainer : private Container {
  private:
    class Element {
    public:
      u_int key;
      u_int pos;
      Element(u_int k, u_int p) : key(k), pos(p) {}
    };

    u_int flattenedSize;
    u_int sortedPos;
    Element **keys;

    void swap(Element *a[], u_int i, u_int j) {
      Element *t = a[i];
      a[i] = a[j];
      a[j] = t;
    }
    void qsort(Element *a[], u_int beg, u_int end);

  public:
    using Container::Sub;
    using Container::GetLength;

    LivenessContainer() : Container(), flattenedSize(0) {
      keys = new Element*[size];
    }
    ~LivenessContainer() {
      for(u_int i=0; i<top; i++)
	delete keys[i];
      delete [] keys;
    }
    void Append(u_int key, word item, u_int itemSize) {
      if(CheckResize()) {
	Element **newKeys = new Element*[size];
	memcpy(newKeys, keys, size * sizeof(u_int));
	delete [] keys;
	keys = newKeys;
      }
      container->Init(top,item);
      keys[top] = new Element(key,top);
      top++;
      flattenedSize += itemSize;
    }
    void Sort() {
      sortedPos=0;
      qsort(keys,0,top);
    }
    word Pop() {
      Assert(sortedPos<top);
      return Sub(keys[sortedPos++]->pos);
    }
    u_int GetFlattenedLength() { return flattenedSize; }
  };
};

class InlineInfo : private Tuple {
private:
  enum { INLINE_MAP_POS, LIVENESS_POS, NLOCALS_POS, NNODES_POS, SIZE };
public:
  using Tuple::ToWord;

  static InlineInfo *New(Map *inlineMap, Vector *liveness, 
			 u_int nLocals, u_int nNodes) {
    Tuple *tup = Tuple::New(SIZE); 
    tup->Init(INLINE_MAP_POS,inlineMap->ToWord());
    tup->Init(LIVENESS_POS,liveness->ToWord());
    tup->Init(NLOCALS_POS,Store::IntToWord(nLocals));
    tup->Init(NNODES_POS,Store::IntToWord(nNodes));
    return (InlineInfo *) tup;
  }
  Map *GetInlineMap() { 
    return Map::FromWordDirect(Tuple::Sel(INLINE_MAP_POS)); 
  }
  Vector *GetLiveness() { 
    return Vector::FromWordDirect(Tuple::Sel(LIVENESS_POS)); 
  }
  u_int GetNLocals() { 
    return Store::DirectWordToInt(Tuple::Sel(NLOCALS_POS)); 
  }
  u_int GetNNodes() { 
    return Store::DirectWordToInt(Tuple::Sel(NNODES_POS)); 
  }

  static InlineInfo *FromWord(word info) {
    return STATIC_CAST(InlineInfo *, Tuple::FromWord(info));
  }
  static InlineInfo *FromWordDirect(word info) {
    return STATIC_CAST(InlineInfo *, Tuple::FromWordDirect(info));
  }
};

class ByteCodeInliner {
private:
  class InlineAnalyser {
  private:
    u_int counter;
    u_int nLocals;
    Vector *subst;
    TagVal *abstractCode;
    Vector *liveness;
    Map *inlineMap;
    Map *appVarPPs;
    u_int callerMaxPP;
    ByteCodeInliner_Internal::LivenessContainer livenessInfo;
    void Append(word key, TagVal *instr,
		TagVal *acc, Closure *closure,
		InlineInfo *inlineInfo);
    Vector *MergeLiveness();
  public:
    InlineAnalyser(TagVal *ac, Map *map, u_int pp) 
      : abstractCode(ac), counter(0), appVarPPs(map), callerMaxPP(pp) {
      subst = Vector::FromWordDirect(abstractCode->Sel(1));
      liveness = Vector::FromWordDirect(abstractCode->Sel(6));
      inlineMap = Map::New(20); 
      nLocals = ByteCodeInliner_Internal::GetNumberOfLocals(abstractCode);
    }
    // This functions breaks an inline analysis cycle introduced by 
    // mutual recursive functions.
    bool CheckCycle(TagVal *acc);
    void Count(TagVal *instr);
    void AnalyseAppVar(TagVal *instr);
    InlineInfo *ComputeInlineInfo() {
      return InlineInfo::New(inlineMap,MergeLiveness(),nLocals,counter);
    } 
  };
  // The driver implements a depth-first search from the root and applies 
  // the analysers to every node exactly ones
  static void Driver(TagVal *root, InlineAnalyser *analyser); 
  static Map *inlineCandidates;
public:

  // Checks if a function is inlinable
  // At the moment we use a very simple heuristic to decide whether we 
  // inline a function or not:
  // We impose a very ad-hoc size level, which is based on the number
  // of nodes inside an abstract code function. More profiling should
  // make this size barrier less ad-hoc.
  static InlineInfo *AnalyseInlining(TagVal *abstractCode);

  // This function must be called in the ByteCodeJitter to indicate the start
  // of the analysis. A new map is created which records the processed 
  // functions.
  static void ResetRoot() { inlineCandidates = Map::New(20); }
};

#endif // __ALICE_BYTECODE_INLINER_HH__
