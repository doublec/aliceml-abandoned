//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "store/JITStore.hh"
#endif

#include "store/JITStore.hh"

// This is the only instance of lightning state
jit_state lightning;

//
// Logging Helper Functions
//
#if defined(JIT_ASSERT_INDEX)
word JITStore::loadedWord;
#endif

#if defined(JIT_STORE_DEBUG)
#include <string.h>

static FILE *execLog;

static const char *RegToString(u_int Reg) {
  switch (Reg) {
  case JIT_R0:
    return "R0";
  case JIT_R1:
    return "R1";
  case JIT_R2:
    return "R2";
  case JIT_V0:
    return "V0";
  case JIT_V1:
    return "V1";
  case JIT_V2:
    return "V2";
  case JIT_SP:
    return "SP";
  case JIT_FP:
    return "FP";
  default:
  return "INVALID_REGISTER";
  }
}

static void ShowMessage(const char *info) {
  fprintf(execLog, info);
  fflush(execLog);
}

static void CompileMessage(const char *info) {
  jit_movi_p(JIT_R0, info);
  jit_pushr_ui(JIT_R0);
  JITStore::Call(1, (void *) ShowMessage);
}

static void ShowRegister(const char *info, word value) {
  fprintf(execLog, info, value);
  fflush(execLog);
}

static void CompileRegister(u_int Reg) {
  static char buffer[256];
  sprintf(buffer, "%s = %%p\n", RegToString(Reg));
  jit_pushr_ui(Reg);
  jit_movi_p(JIT_R0, strdup(buffer));
  jit_pushr_ui(JIT_R0);
  JITStore::Call(2, (void *) ShowRegister);
}
#endif

//
// JITStore Functions
//
void JITStore::InitLoggging() {
#if defined(JIT_STORE_DEBUG)
  if ((execLog = fopen("execlog.txt", "w")) == NULL)
    Error("unable to open exec log");
#endif
}

#if defined(JIT_STORE_DEBUG)
u_int jitDebug = 0;

#define JIT_BEG_COND() \
  jit_pushr_ui(JIT_R0); \
  jit_ldi_ui(JIT_R0, &jitDebug); \
  jit_insn *no_debug = jit_beqi_ui(jit_forward(), JIT_R0, 0); \

#define JIT_END_COND() \
  jit_patch(no_debug); \
  jit_popr_ui(JIT_R0);
#endif
  

void JITStore::LogMesg(const char *info) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileMessage(info);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  // Avoid compiler warnings
  info = info;
#endif
}

void JITStore::LogReg(u_int Value) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileRegister(Value);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  // Avoid compiler warnings
  Value = Value;
#endif
}

void JITStore::DumpReg(u_int Value, value_plotter plotter) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileRegister(Value);
  jit_pushr_ui(Value);
  Call(1, (void *) plotter);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  // Avoid compiler warnings
  Value = Value;
  plotter = plotter;
#endif
}

void JITStore::LogRead(u_int Dest, u_int Ptr, u_int Index) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  static char buffer[256];
  JITStore::SaveAllRegs();
  CompileRegister(Ptr);
  sprintf(buffer, "%s <- %s[%d]...",
	  RegToString(Dest), RegToString(Ptr), Index);
  CompileMessage(strdup(buffer));
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  // Avoid Compiler warnings
  Dest  = Dest;
  Ptr   = Ptr;
  Index = Index;
#endif
}

void JITStore::LogWrite(u_int Ptr, u_int index, u_int Value) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  static char buffer[256];
  JITStore::SaveAllRegs();
  CompileMessage("---\n");
  jit_pushr_ui(Value);
  CompileRegister(Ptr);
  jit_popr_ui(Value);
  CompileRegister(Value);
  sprintf(buffer, "%s[%d] <- %s...",
	  RegToString(Ptr), index, RegToString(Value));
  CompileMessage(strdup(buffer));
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  // Avoid compiler warnings
  Ptr   = Ptr;
  index = index;
  Value = Value;
#endif
}

void JITStore::LogSetArg(u_int pos, u_int Value) {
#if defined(JIT_STORE_DEBUG)
  JIT_BEG_COND();
  static char buffer[256];
  JITStore::SaveAllRegs();
  CompileMessage("---\n");
  CompileRegister(Value);
  sprintf(buffer, "Scheduler::currentArgs[%d] = %s\n",
	  pos, RegToString(Value));
  CompileMessage(strdup(buffer));
  JITStore::RestoreAllRegs();
  JIT_END_COND();
#else
  pos = pos;
  Value = Value;
#endif
}
