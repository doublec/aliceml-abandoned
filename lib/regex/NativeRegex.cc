//
// Author:
//   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Marco Kuhlmann, 2003
//   Guido Tack, 2003
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "Alice.hh"

// use the POSIX.2 interface exclusively

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE
#endif

#include "regex.h"

#ifndef DECLARE_UNMANAGED_POINTER
#define DECLARE_UNMANAGED_POINTER(pointer, x)				\
  void *pointer = NULL;							\
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }	\
  else { pointer = Store::WordToUnmanagedPointer(x); }     
#endif

class RegexRepHandler : public ConcreteRepresentationHandler {
  Transform *RegexRepHandler::GetAbstractRepresentation(ConcreteRepresentation *c);
};

Transform *RegexRepHandler::GetAbstractRepresentation(ConcreteRepresentation *c) {
  c = c;
  return INVALID_POINTER;
}

class RegexFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

static RegexFinalizationSet *regexFinalizationSet;
static RegexRepHandler *regexRepHandler;

void RegexFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  regex_t *r = (regex_t *) Store::WordToUnmanagedPointer(cr->Get(0));
  regfree(r);
}


DEFINE1(regex_regcomp) {
  DECLARE_STRING(pattern, x0);

  regex_t *compiled = (regex_t *) malloc(sizeof(regex_t));
  int result;

  result = regcomp(compiled, pattern->ExportC(), 0);

  if (result == 0) {
    TagVal* tv = TagVal::New(1,1);
    ConcreteRepresentation *cr = ConcreteRepresentation::New(regexRepHandler,1);
    cr->Init(0, Store::UnmanagedPointerToWord(compiled));
    regexFinalizationSet->Register(cr->ToWord());
    tv->Init(0, cr->ToWord());
    RETURN(tv->ToWord()); // SOME regex_t
  } else {
    RETURN(Store::IntToWord(0)); // NONE
  }

} END

// The next function is a wrapper for the regexec function from the GNU C
// Library.  While for the latter, the number of subgroups of a match must
// be known, my_regexec computes this information itself and returns it in
// the references nmatch (number of subgroups, including group 0, which
// contains the complete match) and matchptr (array of indices into the
// string to be matched against indicating the start/end of the subgroups).
// It does so by trying increasingly larger values for these values.

int my_regexec (regex_t* compiled,
		char* match_against,
		size_t & nmatch,
		regmatch_t* & matchptr,
		int eflags) {

  size_t max_nmatch = 2;
  int retval;
  bool needsupdate = true;

  nmatch = (size_t) 0;
  matchptr = (regmatch_t*) NULL;

  while (needsupdate) {
    max_nmatch = max_nmatch * 3/2;
    matchptr = (regmatch_t*) malloc (sizeof (regmatch_t) * max_nmatch);

    retval = regexec (compiled, match_against, max_nmatch, matchptr, eflags);

    if (retval != 0) {
      free (matchptr);
      return retval;
    } else {
      while (needsupdate && (nmatch < (max_nmatch - 1))) {
	if (matchptr[nmatch + 1].rm_so == -1) {
	  needsupdate = false;
	} else {
	  nmatch++;
	}
      }
      free (matchptr);
    }
  }

  nmatch++;
  matchptr = (regmatch_t*) malloc (sizeof (regmatch_t) * nmatch);
  return (regexec (compiled, match_against, nmatch, matchptr, eflags));
}

DEFINE2(regex_regexec) {
  DECLARE_BLOCKTYPE(ConcreteRepresentation, cr, x0);
  //  DECLARE_TUPLE(compiled_tup, x0);
  DECLARE_STRING(match_against, x1);

  regex_t *compiled = (regex_t *) Store::WordToUnmanagedPointer(cr->Get(0));

  size_t nmatch;
  regmatch_t* matchptr;
  int retval;

  retval =
  my_regexec ((regex_t*) compiled, match_against->ExportC(), nmatch, matchptr, 0);

  if (retval == 0) {
    Vector* vec = Vector::New(nmatch);
    for (int i = nmatch; i--; ) {
      Tuple* tup = Tuple::New(2);
      tup->Init(0, Store::IntToWord(matchptr[i].rm_so));
      tup->Init(1, Store::IntToWord(matchptr[i].rm_eo));
      vec->Init(i, tup->ToWord());
    }
    TagVal* tv = TagVal::New(1,1);
    tv->Init(0, vec->ToWord());
    RETURN(tv->ToWord()); // SOME (int * int) vector
  } else {
    RETURN(Store::IntToWord(0)); // NONE
  }
} END

word InitComponent() {
  Record *record = Record::New(2);

  regexFinalizationSet = new RegexFinalizationSet();
  regexRepHandler = new RegexRepHandler();

  INIT_STRUCTURE(record, "NativeRegex", "regcomp",
		 regex_regcomp, 1);
  INIT_STRUCTURE(record, "NativeRegex", "regexec",
		 regex_regexec, 2);

  RETURN_STRUCTURE("NativeRegex$", record);
}
