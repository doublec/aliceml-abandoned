//
// Authors:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#if defined(INTERFACE)
#pragma implementation "alice/DebugEnvironment.hh"
#endif

#include <cstring>
#include "store/Store.hh"
#include "generic/Tuple.hh"
#include "generic/Debug.hh"

#include "alice/AliceLanguageLayer.hh"
#include "alice/AbstractCodeFrame.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AbstractCode.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/DebugEnvironment.hh"
#include "alice/Data.hh"

#define CONS  0
#define NIL   1

// helper
static
Vector *GetVariableNames(Closure *closure) {
  word wConcreteCode = closure->GetConcreteCode();
  AliceConcreteCode *code = AliceConcreteCode::FromWord(wConcreteCode);
  if(code == INVALID_POINTER) {
    Error("INVALID_POINTER found at AliceConcreteCode pos");
  }
  TagVal *abstractCode = code->GetAbstractCode();
  if (abstractCode == INVALID_POINTER) {
    Error("INVALID_POINTER found at AbstractCode pos");
  }
  TagVal *annotations = TagVal::FromWordDirect(abstractCode->Sel(2));
  if (AbstractCode::GetAnnotation(annotations) != AbstractCode::Debug) {
    return INVALID_POINTER;
  }
  return Vector::FromWordDirect(annotations->Sel(0));
}

static word GetTypeScheme(Closure *closure) {
  word wConcreteCode = closure->GetConcreteCode();
  AliceConcreteCode *code = AliceConcreteCode::FromWord(wConcreteCode);
  if(code == INVALID_POINTER) {
    return INVALID_POINTER;
  }
  TagVal *abstractCode = code->GetAbstractCode();
  if (abstractCode == INVALID_POINTER) {
  }
  TagVal *annotations = TagVal::FromWord(abstractCode->Sel(2));
  if (annotations == INVALID_POINTER ||
      AbstractCode::GetAnnotation(annotations) != AbstractCode::Debug) {
    return INVALID_POINTER;
  }
  return annotations->Sel(1);
}

static Vector *GetValues(int length, AbstractCodeFrame::Environment *env) {
  Vector *values = Vector::New(length);
  for (int index = length; index--; ) {
    values->Init(index, env->LookupUnchecked(Store::IntToWord(index)));
  }
  return values;
}

static int GetIndexOf(String *name, Vector *names) {
  // returns -1 if names does not contain name
  for (int index = names->GetLength(); index--; ) {
    String *curr = String::FromWordDirect(names->Sub(index));
    int length1 = name->GetSize();
    int length2 = curr->GetSize();
    if ( length1 == length2 ) {
      int result = std::memcmp(name->GetValue(), curr->GetValue(), length1);
      if (result == 0) {
	return index;
      }
    }
  }
  return -1;
}

static
word GetCurrFrameList(word wNames, word wValues, word wTypes) {
  Vector *names = Vector::FromWordDirect(wNames);
  Vector *values = Vector::FromWordDirect(wValues);
  Vector *types = Vector::FromWordDirect(wTypes);
  word list = Store::IntToWord(NIL);
  for(int index = names->GetLength(); index--; ) {
    TagVal *curr = TagVal::New(CONS,  2);
    Tuple  *triple = Tuple::New(3);
    triple->Init(0, names->Sub(index));
    triple->Init(1, values->Sub(index));
    triple->Init(2, types->Sub(index));
    curr->Init(0, triple->ToWord());
    curr->Init(1, list);
    list = curr->ToWord();
  }
  return list;
}

// DebugEnvironment Constants

word DebugEnvironment::EMPTY;
word DebugEnvironment::INVALID_NAME;

// DebugEnvironment Contructor

DebugEnvironment *DebugEnvironment::New(
		           AbstractCodeFrame::Environment *localEnv, 
			   Closure *globalEnv) {
  Vector *namesTypes = GetVariableNames(globalEnv);
  if (namesTypes == INVALID_POINTER) {
    Error("AbstractCodeFrame without debug annotation");
  }
  Vector *values = GetValues(namesTypes->GetLength(), localEnv);
  // chaining 
  word uplink      = globalEnv->Sub(globalEnv->GetSize() - 1);
  word boundingEnv = EMPTY;
  Block *up = Store::WordToBlock(uplink);
  if (up != INVALID_POINTER && up->GetLabel() == DEBUG_ENVIRONMENT_LABEL) {
    boundingEnv = uplink;
  }
  // filter undefined values
  int filteredLength = 0;
  for(int index = namesTypes->GetLength(); index--; ) {
    if ((values->Sub(index) != AliceLanguageLayer::undefinedValue) &&
	(Store::WordToInt(namesTypes->Sub(index)) == INVALID_INT)) {
      filteredLength++;
    }
  }
  Vector *name = Vector::New(filteredLength);
  Vector *value = Vector::New(filteredLength);
  Vector *types = Vector::New(filteredLength);
  for(int index = values->GetLength(); index--; ) {
    TagVal *nameTypeOption = TagVal::FromWord(namesTypes->Sub(index));
    if(nameTypeOption != INVALID_POINTER) { 
      // SOME
      if (values->Sub(index) != AliceLanguageLayer::undefinedValue) {
	filteredLength--;
	Tuple *ntTuple = Tuple::FromWord(nameTypeOption->Sel(0));
	name->Init(filteredLength, ntTuple->Sel(0));
	types->Init(filteredLength, ntTuple->Sel(1));
	value->Init(filteredLength, values->Sub(index));
      }
    }
  }
  // building the environment
  Block *b = Store::AllocBlock(DEBUG_ENVIRONMENT_LABEL, SIZE);
  b->InitArg(NAME_POS,        name->ToWord());
  b->InitArg(VALUE_POS,       value->ToWord());
  b->InitArg(TYPE_POS,        types->ToWord());
  b->InitArg(TYPE_SCHEME_POS, GetTypeScheme(globalEnv));
  b->InitArg(BOUNDING_POS,    boundingEnv);
  return STATIC_CAST(DebugEnvironment *, b);
}

DebugEnvironment *DebugEnvironment::FromWord(word w) {
  Block *b = Store::WordToBlock(w);
  Assert(b == INVALID_POINTER || b->GetLabel() == DEBUG_ENVIRONMENT_LABEL);
  return STATIC_CAST(DebugEnvironment *, b);
}

DebugEnvironment *DebugEnvironment::FromWordDirect(word w){
  Block *b = Store::DirectWordToBlock(w);
  Assert(b->GetLabel() == DEBUG_ENVIRONMENT_LABEL);
  return STATIC_CAST(DebugEnvironment *, b);
}

word DebugEnvironment::Lookup(String *name) {
  Vector *names = Vector::FromWordDirect(GetArg(NAME_POS));
  int nameIndex = GetIndexOf(name, names);
  
  if (nameIndex == -1) {
    word bound = GetArg(BOUNDING_POS);
    if (bound == EMPTY) {
      return INVALID_NAME;
    }
    return FromWordDirect(bound)->Lookup(name);
  }
  // names contains name
  Vector *values = Vector::FromWordDirect(GetArg(VALUE_POS));
  Vector *types = Vector::FromWordDirect(GetArg(TYPE_POS));
  Tuple  *vttuple = Tuple::New(2);
  vttuple->Init(0, values->Sub(nameIndex));
  vttuple->Init(1, types->Sub(nameIndex));
  return vttuple->ToWord();
}

TagVal *DebugEnvironment::GetNameValueList() {  
  TagVal *curr = TagVal::New(CONS, 2);
  // head creation
  Tuple *schemeEntries = Tuple::New(2);
  schemeEntries->Init(0, GetArg(TYPE_SCHEME_POS));
  schemeEntries->Init(1, GetCurrFrameList(GetArg(NAME_POS), 
					  GetArg(VALUE_POS), 
					  GetArg(TYPE_POS)));
  curr->Init(0, schemeEntries->ToWord());
  // tail creation
  word bound = GetArg(BOUNDING_POS);
  if (bound == EMPTY) {
    curr->Init(1, Store::IntToWord(NIL));
  } else {
    curr->Init(1, FromWordDirect(bound)->GetNameValueList()->ToWord()); 
  }
  return curr;
}
#endif
