//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __PICKLE_HH__
#define __PICKLE_HH__

//
// Opcodes
//

class Pickle {
public:
  typedef enum {
    Con, StaticCon
  } con_label;

  typedef enum {
    OneArg, TupArgs
  } args_label;

  typedef enum {
    AppPrim, AppVar, ConTest, GetTup, IntTest, Kill, PutCon, PutConst,
    PutFun, PutGlobal, PutNew, PutRef, PutSel, PutTag, PutTup, PutVec,
    Raise, RealTest, Reraise, Return, StringTest, TagTest, VecTest
  } instr_label;
};

#endif
