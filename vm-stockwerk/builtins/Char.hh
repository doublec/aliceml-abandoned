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
#ifndef __CHAR_HH__
#define __CHAR_HH__

namespace Builtins {
  namespace Char {
    word opless(word a, word b);
    word opgreater(word a, word b);
    word oplessEq(word a, word b);
    word opgreaterEq(word a, word b);

    word ord(word a);
    word chr(word a);
    
    word isAlpha(word a);
    word isAlphaNum(word a);
    word isCntrl(word a);
    word isDigit(word a);
    word isGraph(word a);
    word isHexDigit(word a);
    word isLower(word a);
    word isPrint(word a);
    word isPunct(word a);
    word isSpace(word a);
    word isUpper(word a);

    word toLower(word a);
    word toUpper(word a);
  }
}

#endif
