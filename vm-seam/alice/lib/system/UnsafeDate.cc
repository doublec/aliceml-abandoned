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

static inline int fromMonth (word m) {
    TagVal *t = TagVal::FromWord (m);
    switch (t->GetTag ()) {
        // Apr
        case 0: return 3;
        // Aug
        case 1: return 7;
        // Dec
        case 2: return 11;
        // Feb
        case 3: return 1;
        // Jan
        case 4: return 0;
        // Jul
        case 5: return 6;
        // Jun
        case 6: return 5;
        // Mar
        case 7: return 2;
        // May
        case 8: return 4;
        // Nov
        case 9: return 10;
        // Oct
        case 10: return 9;
        // Sep
        case 11: return 8;
        
        default:
                 Error ("Invalid month")
    }
}


static inline word mkMonth (int mon) {
    TagVal *t;
    
    switch (mon) {
        // Apr
        case 3: t = TagVal::New (0, 0); break;
        // Aug
        case 7: t = TagVal::New (1, 0); break;
        // Dec
        case 11: t = TagVal::New (2, 0); break;
        // Feb
        case 1: t = TagVal::New (3, 0); break;
        // Jan
        case 0: t = TagVal::New (4, 0); break;
        // Jul
        case 6: t = TagVal::New (5, 0); break;
        // Jun
        case 5: t = TagVal::New (6, 0); break;
        // Mar
        case 2: t = TagVal::New (7, 0); break;
        // May
        case 4: t = TagVal::New (8, 0); break;
        // Nov
        case 10: t = TagVal::New (9, 0); break;
        // Oct
        case 9: t = TagVal::New (10, 0); break;
        // Sep
        case 8: t = TagVal::New (11, 0); break;
        default:
                Error ("invalid month.");
    }

    return t->ToWord ();
}

static inline word mkWeekday (int wd) {
    TagVal *t;
    
    switch (wd) {
        // Son
        case 0: t = TagVal::New (3, 0); break;
        // Mon
        case 1: t = TagVal::New (1, 0); break;
        // Tue
        case 2: t = TagVal::New (5, 0); break;
        // Wed
        case 3: t = TagVal::New (6, 0); break;
        // Thu
        case 4: t = TagVal::New (4, 0); break;
        // Fri
        case 5: t = TagVal::New (0, 0); break;
        // Sat
        case 6: t = TagVal::New (2, 0); break;

        default:
                Error("invalid weekday.");
    }

    return t->ToWord();    
}


static inline int fromWeekday (word w) {
    TagVal *t = TagVal::FromWord (w);

    switch (t->GetTag ()) {
        //Fri
        case 0: return  (4);
        //Mon
        case 1: return  (0);
        //Sat
        case 2: return  (5);
        //Son
        case 3: return  (6);
        //Thu
        case 4: return  (3);
        //Tue
        case 5: return  (1);
        //Wed
        case 6: return  (2);

        default:
                Error ("invalid weekday.");
    }
}


static inline word mkBool (bool b) {
    TagVal *t = TagVal::New (b ? Types::_true : Types::_false, 0);
    return t->ToWord ();
}

static inline word mkSome (word w) {
    TagVal *some = TagVal::New (Types::SOME, 1);
    some->Init (0, w);
    return some->ToWord ();
}
            
static inline word mkNone () {
    TagVal *n = TagVal::New (Types::NONE, 0);
    return n->ToWord ();
}

static inline bool isSome (word w) {
    TagVal *t = TagVal::FromWord (w);
    return t->GetTag () == Types::SOME;
}

static inline word valOf (word w) {
    Assert (isSome (w));
    TagVal *t = TagVal::FromWord (w);
    return t->Sel (0);
}

static inline bool isTrue (word w) {
    TagVal *t = TagVal::FromWord (w);
    return t->GetTag () == Types::_true;
}

static word exceptionDate;

#define RAISE_DATE              \
    { \
        ConVal *conVal = ConVal::New ((Block*)Constructor::FromWordDirect (exceptionDate), 0); \
        RAISE(conVal->ToWord ()); \
    }

const int baseYear = 1900;

static inline word mkDate (struct tm *tm) {
    TagVal *t = TagVal::New (0, 10);
    t->Init (0, INT_TO_WORD (tm->tm_mday));
    t->Init (1, INT_TO_WORD (tm->tm_hour));
    t->Init (2, tm->tm_isdst > 0 ? mkSome (mkBool (true)) 
              : ( tm->tm_isdst == 0 ? mkSome (mkBool (false)) 
                    : mkNone () ) );
    t->Init (3, INT_TO_WORD (tm->tm_min));
    t->Init (4, mkMonth (tm->tm_mon));
    t->Init (5, mkNone ());
    t->Init (6, INT_TO_WORD (tm->tm_sec));
    t->Init (7, mkWeekday (tm->tm_wday));
    t->Init (8, INT_TO_WORD (tm->tm_yday));
    t->Init (9, INT_TO_WORD (tm->tm_year + baseYear));
    return t->ToWord ();
}

static inline void to_tm (TagVal *t, struct tm *tm) {
    tm->tm_mday     = WORD_TO_INT (t->Sel (0));
    tm->tm_hour     = WORD_TO_INT (t->Sel (1));
    if (isSome (t->Sel (2))) {
        if (isTrue ( valOf (t->Sel (2)) )) {
            tm->tm_isdst = 1;
        } else {
            tm->tm_isdst = 0;
        }
    } else {
        tm->tm_isdst = -1;
    }
    tm->tm_min      = WORD_TO_INT (t->Sel (3));
    tm->tm_mon      = fromMonth (t->Sel (4));
    tm->tm_sec      = WORD_TO_INT (t->Sel (6));
    tm->tm_wday     = fromWeekday (t->Sel (7));
    tm->tm_yday     = WORD_TO_INT (t->Sel (8));
    tm->tm_year     = WORD_TO_INT (t->Sel (9)) - baseYear;
}


DEFINE1(UnsafeDate_fromTimeLocal) {
    DECLARE_INTINF(t, x0);
    struct tm *tm;
    time_t time = static_cast<time_t> (mpz_get_d (t->big ()));
    tm = localtime (&time);
    if (tm == NULL) {
        RAISE_DATE;
    } else {
        RETURN(mkDate (tm));
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
        RETURN(mkDate (tm));
    }
} END


DEFINE1(UnsafeDate_toTime) {
    TagVal *t = TagVal::FromWord (x0);
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
    TagVal *t   = TagVal::FromWord (x1);
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


DEFINE2(UnsafeDate_toString) {
    TagVal *t   = TagVal::FromWord (x0);
    struct tm tm;
    to_tm (t, &tm);
    const char *res = asctime(&tm);
    if (res != NULL) {
        RETURN(String::New (res)->ToWord ());
    } else {
        RAISE_DATE;
    }
} END


DEFINE0(UnsafeDate_Date) {
    Constructor *ccVal = Constructor::FromWord (exceptionDate);
    ConVal *conVal     = ConVal::New ((Block*)ccVal, 0);
    RETURN(conVal->ToWord ());
} END


AliceDll word UnsafeDate() {
  exceptionDate = UniqueConstructor::New ("Date", "Date.Date")->ToWord ();
  RootSet::Add (exceptionDate);
  Record *record = Record::New(7);
  record->Init ("'Date", exceptionDate);
  INIT_STRUCTURE(record, "UnsafeDate", "Date", UnsafeDate_Date, 0);
  INIT_STRUCTURE(record, "UnsafeDate", "fromTimeLocal", UnsafeDate_fromTimeLocal, 1);
  INIT_STRUCTURE(record, "UnsafeDate", "fromTimeUniv",  UnsafeDate_fromTimeUniv,  1);
  INIT_STRUCTURE(record, "UnsafeDate", "toTime",        UnsafeDate_toTime, 1);
  INIT_STRUCTURE(record, "UnsafeDate", "fmt",           UnsafeDate_fmt, 1);
  INIT_STRUCTURE(record, "UnsafeDate", "toString",      UnsafeDate_toString, 1);
  RETURN_STRUCTURE("UnsafeDate$", record);
}
