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
#ifndef __TEST__STORE__SCHEME__INTERPRETER_HH__
#define __TEST__STORE__SCHEME__INTERPRETER_HH__

#include "Nodes.hh"

class Interpreter {
private:
  static const u_int ROOT_SIZE      = 3;
  static const u_int STACK_SIZE     = 16;
  static const u_int TASK_STACK_POS = 1;
  static const u_int EVAL_STACK_POS = 2;
  static const u_int ENV_STACK_POS  = 3;
protected:
  static word root;

  static int HaveTask();
  static void PushTask(word task);
  static word PopTask();
  static u_int PushId(IdNode *id);
  static void PopId();
  static void AssignId(u_int i);
  static word LookUp(char *s);
  static void PushValue(word value);
  static word PopValue();

  static void CreateEnvironment();
public:
  static void Init();
  static char *Interpret(word tree);
};

#endif __TEST__STORE__SCHEME__INTERPRETER_HH__
