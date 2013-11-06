//
// Author:
//   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Marco Kuhlmann, 2003
//   Guido Tack, 2003
// 
//  See the file "LICENSE" for information on usage and
//  redistribution of this file, and for a
//     DISCLAIMER OF ALL WARRANTIES.
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

extern "C" {
#include "regex.h"
}

// In future releases, DECLARE_UNMANAGED_POINTER will be defined
// in vm-stockwerk/alice/Authoring.hh.

#ifndef DECLARE_UNMANAGED_POINTER
#define DECLARE_UNMANAGED_POINTER(pointer, x)				\
  void *pointer = NULL;							\
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }	\
  else { pointer = Store::WordToUnmanagedPointer(x); }     
#endif

// In order to make sure that compiled regexps cannot be pickled,
// we register a new representation handler that will return an
// INVALID_POINTER as soon as the abstract representation of
// regex_t is requested.

class RegexRepHandler : public ConcreteRepresentationHandler {
  Transform *GetAbstractRepresentation(ConcreteRepresentation *c);
};

Transform *RegexRepHandler::GetAbstractRepresentation(ConcreteRepresentation *) {
  return INVALID_POINTER;
}

static RegexRepHandler *regexRepHandler;

// Memory allocated by compiled regular expressions need to be
// freed as soon as references to them are removed.  For that, we
// need a customised finalization set.

class RegexFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

void RegexFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  regex_t* r = (regex_t*) Store::WordToUnmanagedPointer(cr->Get(0));
  regfree(r);
}

static RegexFinalizationSet *regexFinalizationSet;


// regcomp : string -> 'regex_t
//
// compiles a string into a regular expression; returns SOME
// compiled regular expression if compilation succeeds; NONE
// otherwise.

DEFINE1(regex_regcomp) {
  DECLARE_STRING(pattern, x0);

  regex_t *compiled = (regex_t*) malloc(sizeof(regex_t));

  int retval =
    regcomp(compiled, pattern->ExportC(), REG_EXTENDED);

  if (retval == 0) {
    ConcreteRepresentation *cr = ConcreteRepresentation::New(regexRepHandler, 1);
    cr->Init(0, Store::UnmanagedPointerToWord(compiled));
    regexFinalizationSet->Register(cr->ToWord());
    TagVal* tv = TagVal::New(1, 1);
    tv->Init(0, cr->ToWord());
    RETURN(tv->ToWord()); // SOME regex_t
  } else {
    RETURN(Store::IntToWord(0)); // NONE
  }

} END

// The next function is a wrapper for the regexec function.
// While for the latter, the number of subgroups of a match must
// be known, my_regexec computes this information itself and
// returns it in the references nmatch (number of subgroups
// including group 0 that contains the complete match) and
// matchptr (array of indices into the match string indicating
// the start/end of the subgroups).

int my_regexec (regex_t* compiled,
		char* match_against,
		size_t & nmatch,
		regmatch_t* & matchptr,
		int eflags) {

  size_t max_nmatch = 2; // start with two subgroups
  bool needsupdate = true;

  nmatch = (size_t) 0;
  matchptr = (regmatch_t*) NULL;

  while (needsupdate) {
    max_nmatch = max_nmatch * 3/2;
    matchptr = (regmatch_t*) malloc (sizeof(regmatch_t) * max_nmatch);

    int retval =
      regexec(compiled, match_against, max_nmatch, matchptr, eflags);

    if (retval != 0) { // error
      free(matchptr);
      return retval;
    } else {
      while (needsupdate && nmatch < (max_nmatch - 1)) {
	if (matchptr[nmatch + 1].rm_so == -1) { // no subgroup
	  needsupdate = false;
	} else {
	  nmatch++;
	}
      }
      free(matchptr);
    }
  }

  nmatch++;
  matchptr = (regmatch_t*) malloc (sizeof(regmatch_t) * nmatch);
  return (regexec(compiled, match_against, nmatch, matchptr, eflags));
}

// regexec : 'regex_t * string -> (int * int) vector option
//
// applies a compiled regular expression to a string; returns
// SOME vector with the indices of the matched substrings in case
// there was a match, NONE otherwise.

DEFINE2(regex_regexec) {
  DECLARE_BLOCKTYPE(ConcreteRepresentation, cr, x0);
  DECLARE_STRING(match_against, x1);

  regex_t* compiled = (regex_t*) Store::WordToUnmanagedPointer(cr->Get(0));
  size_t nmatch;
  regmatch_t* matchptr;

  int retval =
    my_regexec ((regex_t*) compiled, match_against->ExportC(), nmatch, matchptr, 0);

  if (retval == 0) {
    Vector* vec = Vector::New(nmatch);
    for (int i = nmatch; i--; ) {
      Tuple* tup = Tuple::New(2);
      tup->Init(0, Store::IntToWord(matchptr[i].rm_so));
      tup->Init(1, Store::IntToWord(matchptr[i].rm_eo));
      vec->Init(i, tup->ToWord());
    }
    TagVal* tv = TagVal::New(1, 1);
    tv->Init(0, vec->ToWord());
    RETURN(tv->ToWord()); // SOME (int * int) vector
  } else {
    RETURN(Store::IntToWord(0)); // NONE
  }

} END

// Finally, initialise an Alice component providing the binding
// (a record with one entry for every function) and register the
// finalisation set and the representation handler.

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
