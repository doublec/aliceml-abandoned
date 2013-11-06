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

#if defined(HAVE_FEGETROUND) || defined(HAVE_FESETROUND)
#include <fenv.h>
#endif

/* rounding_mode    name_in_alice   int_in_seam
 * FE_DOWNWARD      TO_NEGINF       1
 * FE_TONEAREST     TO_NEAREST      0
 * FE_TOWARDZERO    TO_ZERO         3
 * FE_UPWARD        TO_POSINF       2
 */

#if defined(HAVE_FEGETROUND)
DEFINE0(IEEEReal_getRoundingMode) {
    word res;
    switch (fegetround ()) {
        case FE_TONEAREST: res =  Store::IntToWord (0);    break;
        case FE_DOWNWARD: res =  Store::IntToWord (1); break;
        case FE_UPWARD: res =  Store::IntToWord (2); break;
        case FE_TOWARDZERO: res =  Store::IntToWord (3); break;
        default: Error ("unknown/invalid rounding mode");
    }
    RETURN(res);
} END

#else

DEFINE0(IEEEReal_getRoundingMode) {
    Error ("IEEEReal.getRoundingMode: ISO C 99 fegetround not available");
    RETURN_UNIT;
} END


#endif

#if defined(HAVE_FESETROUND)

DEFINE1(IEEEReal_setRoundingMode) {
    DECLARE_INT(mode, x0);
    int femode;
    switch (mode) {
        case 0: femode = FE_TONEAREST; break;
        case 1: femode = FE_DOWNWARD; break;
        case 2: femode = FE_UPWARD; break;
        case 3: femode = FE_TOWARDZERO; break;
        default: Assert (0);
    }
    fesetround (femode);
    RETURN_UNIT;
} END

#else

DEFINE1(IEEEReal_setRoundingMode) {
    Error ("IEEEReal.setRoundingMode: ISO C 99 fesetround not available");
    RETURN_UNIT;
} END

#endif

void PrimitiveTable::RegisterIEEEReal() {
    Register("IEEEReal.getRoundingMode", IEEEReal_getRoundingMode, 0);
    Register("IEEEReal.setRoundingMode", IEEEReal_setRoundingMode, 1);
}


