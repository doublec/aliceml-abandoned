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

#if defined(HAVE_GETTIMEOFDAY)

#include <sys/time.h>
#include <time.h>

DEFINE0(UnsafeTime_now) {
  struct timeval tv;
  gettimeofday(&tv, 0);
  BigInt *res = BigInt::New((double)tv.tv_sec);
  mpz_mul_ui(res->big(), res->big(), 1000000UL);
  mpz_add_ui(res->big(), res->big(), tv.tv_usec);
  mpz_div_ui(res->big(), res->big(), 1000UL);
  RETURN_INTINF(res);
} END

#else

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

#endif

AliceDll word UnsafeTime() {
  Record *record = Record::New(1);
  INIT_STRUCTURE(record, "UnsafeTime", "now", UnsafeTime_now, 0);
  RETURN_STRUCTURE("UnsafeTime$", record);
}
