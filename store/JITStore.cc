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

#if HAVE_LIGHTNING

#if defined(INTERFACE)
#pragma implementation "store/JITStore.hh"
#endif

#include "store/JITStore.hh"

//
// Logging Helper Functions
//
#if defined(JIT_ASSERT_INDEX)
word JITStore::loadedWord;
#endif

#if defined(JIT_STORE_DEBUG)

#include <string.h>

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
#ifdef JIT_FP
  case JIT_FP:
    return "FP";
#endif
  default:
  return "INVALID_REGISTER";
  }
}

static FILE *execLog;

static void ShowMessage(const char *info) {
  fprintf(execLog, info);
  fflush(execLog);
}

static void ShowRegister(const char *info, word value) {
  fprintf(execLog, info, value);
  fflush(execLog);
}

//
// JITStore Logging Functions
//
void JITStore::InitLoggging() {
  if ((execLog = fopen("execlog.txt", "w")) == NULL)
    Error("unable to open exec log");
}

SeamDll u_int jitDebug = 0;

#define JIT_BEG_COND() \
  jit_pushr_ui(JIT_R0); \
  jit_ldi_ui(JIT_R0, &jitDebug); \
  jit_insn *no_debug = jit_beqi_ui(jit_forward(), JIT_R0, 0); \

#define JIT_END_COND() \
  jit_patch(no_debug); \
  jit_popr_ui(JIT_R0);


void JITStore::CompileMessage(const char *info) {
  JITStore::Prepare(1);
  jit_movi_p(JIT_R0, info);
  jit_pusharg_ui(JIT_R0);
  JITStore::Finish((void *) ShowMessage);
}

void JITStore::CompileRegister(u_int Reg) {
  static char buffer[256];
  sprintf(buffer, "%s = %%p\n", RegToString(Reg));
  JITStore::Prepare(2);
  jit_pusharg_ui(Reg);
  jit_movi_p(JIT_R0, strdup(buffer));
  jit_pusharg_ui(JIT_R0);
  JITStore::Finish((void *) ShowRegister);
}

void JITStore::LogMesg(const char *info) {
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileMessage(info);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
}

void JITStore::LogReg(u_int Value) {
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileRegister(Value);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
}

void JITStore::DumpReg(u_int Value, value_plotter plotter) {
  JIT_BEG_COND();
  JITStore::SaveAllRegs();
  CompileRegister(Value);
  JITStore::Prepare(1);
  jit_pusharg_ui(Value);
  JITStore::Finish((void *) plotter);
  JITStore::RestoreAllRegs();
  JIT_END_COND();
}

void JITStore::LogRead(u_int Dest, u_int Ptr, u_int Index) {
  JIT_BEG_COND();
  static char buffer[256];
  JITStore::SaveAllRegs();
  CompileRegister(Ptr);
  sprintf(buffer, "%s <- %s[%d]...",
	  RegToString(Dest), RegToString(Ptr), Index);
  CompileMessage(strdup(buffer));
  JITStore::RestoreAllRegs();
  JIT_END_COND();
}

void JITStore::LogWrite(u_int Ptr, u_int index, u_int Value) {
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
}

void JITStore::LogSetArg(u_int pos, u_int Value) {
  JIT_BEG_COND();
  static char buffer[256];
  JITStore::SaveAllRegs();
  jit_pushr_ui(Value);
  CompileMessage("---\n");
  jit_popr_ui(Value);
  CompileRegister(Value);
  sprintf(buffer, "Scheduler::currentArgs[%d] = %s\n",
	  pos, RegToString(Value));
  CompileMessage(strdup(buffer));
  JITStore::RestoreAllRegs();
  JIT_END_COND();
}

#endif

#endif
