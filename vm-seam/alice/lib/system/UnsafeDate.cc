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
#include <time.h>

#define INT_TO_WORD(i) Store::IntToWord(i)
#define WORD_TO_INT(w) Store::WordToInt(w)

#define AWAIT(y,x) \
        if (Store::WordToTransient(x) != INVALID_POINTER) REQUEST(x); \
        word y = PointerOp::Deref(x);


static word exceptionDate;

#define RAISE_DATE              \
    { \
        ConVal *conVal = ConVal::New ((Block*)Constructor::FromWordDirect (exceptionDate), 0); \
        RAISE(conVal->ToWord ()); \
    }

static inline word from_tm (struct tm *tm) {
    Tuple *t = Tuple::New (9);
    t->Init (0, INT_TO_WORD (tm->tm_sec));
    t->Init (1, INT_TO_WORD (tm->tm_min));
    t->Init (2, INT_TO_WORD (tm->tm_hour));
    t->Init (3, INT_TO_WORD (tm->tm_mday));
    t->Init (4, INT_TO_WORD (tm->tm_mon));
    t->Init (5, INT_TO_WORD (tm->tm_year));
    t->Init (6, INT_TO_WORD (tm->tm_wday));
    t->Init (7, INT_TO_WORD (tm->tm_yday));
    t->Init (8, INT_TO_WORD (tm->tm_isdst));

    return t->ToWord ();
}

static inline void to_tm (Tuple *t, struct tm *tm) {
    tm->tm_sec  = WORD_TO_INT (t->Sel (0));
    tm->tm_min  = WORD_TO_INT (t->Sel (1));
    tm->tm_hour = WORD_TO_INT (t->Sel (2));
    tm->tm_mday = WORD_TO_INT (t->Sel (3));
    tm->tm_mon  = WORD_TO_INT (t->Sel (4));
    tm->tm_year = WORD_TO_INT (t->Sel (5));
    tm->tm_wday = WORD_TO_INT (t->Sel (6));
    tm->tm_yday = WORD_TO_INT (t->Sel (7));
    tm->tm_isdst = WORD_TO_INT (t->Sel (8));
}


DEFINE1(UnsafeDate_fromTimeLocal) {
    DECLARE_INTINF(t, x0);
    struct tm *tm;
    time_t time = static_cast<time_t> (mpz_get_d (t->big ()));
    tm = localtime (&time);
    if (tm == NULL) {
        RAISE_DATE;
    } else {
        RETURN(from_tm (tm));
    }
} END


DEFINE1(UnsafeDate_fromTimeUniv) {
    DECLARE_INTINF(t, x0);
    struct tm *tm;
    time_t time = static_cast<time_t> (mpz_get_d (t->big ()));
    tm = gmtime (&time);
    if (tm == NULL) {
        RAISE_DATE;
    } else {
        RETURN(from_tm (tm));
    }
} END


DEFINE1(UnsafeDate_toTime) {
    DECLARE_TUPLE(t, x0); 
    struct tm tm;
    to_tm (t, &tm);
    time_t time = mktime (&tm);
    if (time == (time_t) -1) {
        RAISE_DATE;
    } else {
        RETURN_INTINF(BigInt::New (static_cast<double> (time)));
    }
} END


DEFINE2(UnsafeDate_fmt) {
    DECLARE_STRING(fmt, x0);
    DECLARE_TUPLE(t, x1);
    struct tm tm;
    char buf[512];
    to_tm (t, &tm);
    int size = strftime(buf, sizeof(buf), fmt->ExportC (), &tm);
    if (size > 0) {
        RETURN(String::New (buf, size)->ToWord ());
    } else if (size == 0 && !strcmp(fmt->ExportC (), "%p")) {
        // educated guess: if size = 0 resulting string
        // might be empty iff fmt = "%p" (AM|PM)
        RETURN(String::New (0U)->ToWord ());
    } else {
        RAISE_DATE;
    }
} END


DEFINE1(UnsafeDate_toString) {
    DECLARE_TUPLE(t, x0);
    struct tm tm;
    to_tm (t, &tm);
    const char *res = asctime(&tm);
    if (res != NULL) {
        RETURN(String::New (res)->ToWord ());
    } else {
        RAISE_DATE;
    }
} END


AliceDll word UnsafeDate() {
  exceptionDate = UniqueConstructor::New ("Date", "Date.Date")->ToWord ();
  RootSet::Add (exceptionDate);
  Record *record = Record::New(7);
  record->Init ("'Date", exceptionDate);
  record->Init ("Date", exceptionDate);
  INIT_STRUCTURE(record, "UnsafeDate", "fromTimeLocal", UnsafeDate_fromTimeLocal, 1);
  INIT_STRUCTURE(record, "UnsafeDate", "fromTimeUniv",  UnsafeDate_fromTimeUniv,  1);
  INIT_STRUCTURE(record, "UnsafeDate", "toTime",        UnsafeDate_toTime, 1);
  INIT_STRUCTURE(record, "UnsafeDate", "fmt",           UnsafeDate_fmt, 2);
  INIT_STRUCTURE(record, "UnsafeDate", "toString",      UnsafeDate_toString, 1);
  RETURN_STRUCTURE("UnsafeDate$", record);
}
