//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __PREBOUND_HH__
#define __PREBOUND_HH__

typedef word (*FUNPTR)(...);

namespace Builtins {
  namespace Prebound {
    namespace Array {
      FUNPTR array;
      FUNPTR fromList;
      FUNPTR length;
      FUNPTR sub;
      FUNPTR update;
    }
    namespace Char {
      FUNPTR opless;
      FUNPTR opgreater;
      FUNPTR oplessEq;
      FUNPTR opgreaterEq;
      FUNPTR ord;
      FUNPTR chr;
      FUNPTR isAlpha;
      FUNPTR isAlphaNum;
      FUNPTR isCntrl;
      FUNPTR isDigit;
      FUNPTR isGraph;
      FUNPTR isHexDigit;
      FUNPTR isLower;
      FUNPTR isPrint;
      FUNPTR isPunct;
      FUNPTR isSpace;
      FUNPTR isUpper;
      FUNPTR toLower;
      FUNPTR toUpper;
    }
    namespace CommandLine {
      FUNPTR name;
      FUNPTR arguments;
    }
    namespace Future {
      FUNPTR alarmQuote;
      FUNPTR await;
      FUNPTR awaitOne;
      FUNPTR byneed;
      FUNPTR concur;
      FUNPTR isFailed;
      FUNPTR isFuture;
    }
    namespace General {
      FUNPTR assign;
      FUNPTR exchange;
      FUNPTR exnName;
    }
    namespace Hole {
      FUNPTR fail;
      FUNPTR fill;
      FUNPTR future;
      FUNPTR hole;
      FUNPTR isFailed;
    }
    namespace Int {
      FUNPTR opnegate;
      FUNPTR opadd;
      FUNPTR opsub;
      FUNPTR opmul;
      FUNPTR opless;
      FUNPTR opgreater;
      FUNPTR oplessEq;
      FUNPTR opgreaterEq;
      FUNPTR abs;
      FUNPTR compare;
      FUNPTR div;
      FUNPTR mod;
      FUNPTR quot;
      FUNPTR rem;
      FUNPTR toString;
    }
    namespace Math {
      FUNPTR acos;
      FUNPTR acosh;
      FUNPTR asin;
      FUNPTR asinh;
      FUNPTR atan;
      FUNPTR atanh;
      FUNPTR atan2;
      FUNPTR cos;
      FUNPTR cosh;
      FUNPTR exp;
      FUNPTR ln;
      FUNPTR pow;
      FUNPTR sin;
      FUNPTR sinh;
      FUNPTR sqrt;
      FUNPTR tan;
      FUNPTR tanh;
    }
    namespace OS {
      FUNPTR system;
      FUNPTR exit;
      FUNPTR getEnv;
    }
    namespace Real {
      FUNPTR opnegate;
      FUNPTR opadd;
      FUNPTR opsub;
      FUNPTR opmul;
      FUNPTR opdiv;
      FUNPTR opless;
      FUNPTR opgreater;
      FUNPTR oplessEq;
      FUNPTR opgreaterEq;
      FUNPTR ceil;
      FUNPTR compare;
      FUNPTR floor;
      FUNPTR fromInt;
      FUNPTR realCeil;
      FUNPTR realFloor;
      FUNPTR realRound;
      FUNPTR realTrunc;
      FUNPTR rem;
      FUNPTR round;
      FUNPTR toString;
      FUNPTR trunc;
    }
    namespace String {
      FUNPTR append;
      FUNPTR opless;
      FUNPTR opgreater;
      FUNPTR oplessEq;
      FUNPTR opgreaterEq;
      FUNPTR compare;
      FUNPTR explode;
      FUNPTR implode;
      FUNPTR size;
      FUNPTR sub;
      FUNPTR substring;
      FUNPTR str;
    }
    namespace TextIO {
      FUNPTR openIn;
      FUNPTR inputAll;
      FUNPTR inputLine;
      FUNPTR closeIn;
      FUNPTR openOut;
      FUNPTR output;
      FUNPTR output1;
      FUNPTR flushOut;
      FUNPTR closeOut;
      FUNPTR print;
    }
    namespace Thread {
      FUNPTR terminate;
      FUNPTR current;
      FUNPTR isSuspended;
      FUNPTR raiseIn;
      FUNPTR resume;
      FUNPTR state;
      FUNPTR suspend;
      FUNPTR yield;
    }
    namespace Unsafe {
      namespace Array {
	FUNPTR sub;
	FUNPTR update;
      }
      namespace String {
	FUNPTR sub;
      }
      namespace Vector {
	FUNPTR sub;
      }
      FUNPTR cast;
      FUNPTR getTag;
      FUNPTR getValue;
    }
    namespace Vector {
      FUNPTR fromList;
      FUNPTR length;
      FUNPTR sub;
    }
    namespace Word {
      FUNPTR fromIntQuote;
      FUNPTR fromInt;
      FUNPTR toInt;
      FUNPTR toIntX;
      FUNPTR opadd;
      FUNPTR opsub;
      FUNPTR opmul;
      FUNPTR opdiv;
      FUNPTR mod;
      FUNPTR orb;
      FUNPTR xorb;
      FUNPTR andb;
      FUNPTR notb;
      FUNPTR shl;
      FUNPTR shr;
      FUNPTR arithshr;
      FUNPTR toString;
    }
    FUNPTR opeq;
    FUNPTR opnoteq;
  }
}

#endif
