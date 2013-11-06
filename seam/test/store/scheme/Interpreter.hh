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

class Interpreter {
private:
  static const u_int STACK_SIZE    = 16;
  static const u_int DICT_SIZE     = 64;
  static const u_int ROOTSET_SIZE  = 7;
  static const u_int TASK_STACK    = 0;
  static const u_int EVAL_STACK    = 1;
  static const u_int FRAME_STACK   = 2;
  static const u_int CLOSURE_STACK = 3;
  static const u_int GLB_ENV_LIST  = 4;
  static const u_int GLB_ENV       = 5;
  static const u_int ATOM_DICT     = 6;
protected:
  static word root;
  static u_int allowGC;

  static word GetRoot(u_int pos);
  static int HaveTask();
  static void PushTask(word task);
  static word PopTask();
  static void PushValue(word value);
  static word PopValue();
  static void PushFrame(u_int i);
  static void PushTailFrame(u_int i);
  static void PopFrame();
  static void PushClosure(word closure);
  static void PushTailClosure(word closure);
  static void PopClosure();
  static void PushClosureArg(u_int i);
  static void PushFrameArg(u_int i);
  static void PushGlobalArg(u_int i);
  static void AssignFrameArg(u_int i);
  static void AssignClosureArg(u_int i);
  static void AssignGlobalArg(u_int i);
  static word FetchSemiValue(IdNode *node);

  static void CreateId(u_int i, PrimType type);
  static void CreateDefaultEnv();
  
  static void InterpretDeclArr(Block *instr);
  static void InterpretDefine(Block *instr);
  static void InterpretAssign(Block *instr);
  static void InterpretIf(Block *instr);
  static void InterpretSelection(Block *instr);
  static void InterpretValue(Block *instr);
  static void InterpretId(Block *instr);
  static void InterpretLet(Block *instr);
  static void InterpretLambda(Block *instr);
  static void InterpretApplication(Block *instr);
  static void InterpretBegin(Block *instr);
  static void InterpretSetQ(Block *instr);
  static void InterpretSetCxr(NodeType type, Block *instr);
  static void InterpretTime();
  static void InterpretRemove();
  static char *InterpretOp(Block *p);
public:
  static void Init();
  static u_int RegisterAtom(const char *s);
  static char *AtomToString(u_int name);
  static u_int GlobalAlloc(u_int name);
  static u_int GlobalAlloc(const char *s);
  static int SearchGlobal(u_int name);
  static char *Interpret(word tree);
};

#endif __TEST__STORE__SCHEME__INTERPRETER_HH__
