//
// Authors:
//   Benedikt Grundmann <bgrund@ps.uni-sb.de> 
//
// Copyright:
//   Benedikt Grundmann, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

// This is really unsatisfactory:
// As far as I know ftime is available
// on both linux and windows but deprecated
// while gettimeofday which is the function
// to use on linux does not exists on 
// windows.
#include <sys/timeb.h>

// return current time in milliseconds.
DEFINE0(UnsafeTime_now) {
  struct timeb tb;
  ftime (&tb);
  BigInt *res	= BigInt::New ((double)tb.time);
  mpz_mul_ui (res->big (), res->big (), 1000UL);
  mpz_add_ui (res->big (), res->big (), tb.millitm);
  RETURN_INTINF(res);
} END

AliceDll word UnsafeTime() {
  Record *record = Record::New(1);
  INIT_STRUCTURE(record, "UnsafeTime", "now", UnsafeTime_now, 1);
  RETURN_STRUCTURE("UnsafeTime$", record);
}
