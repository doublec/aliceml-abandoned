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
#ifndef __TEST__STORE__SCHEME__ENVIRONMENT_HH__
#define __TEST__STORE__SCHEME__ENVIRONMENT_HH__

class Environment {
protected:
  static const u_int MAX_CLOSURE_DEPTH = 256;
  static word closures[MAX_CLOSURE_DEPTH];
  static word semi_locals[MAX_CLOSURE_DEPTH];
  static int top;

  static u_int Length(word l);
  static int SearchSemi(u_int name);
public:
  static void Reset();
  static int SearchFrame(word frame, u_int name);
  static void SearchId(u_int name, VarType *type, u_int *id);
};

#endif __TEST__STORE__SCHEME__ENVIRONMENT_HH__
