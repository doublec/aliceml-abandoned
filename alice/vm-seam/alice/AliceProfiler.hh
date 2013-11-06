
#ifndef __ALICE_PROFILER_HH__
#define __ALICE_PROFILER_HH__

#if defined(INTERFACE)
#pragma inferface "alice/AliceProfiler.hh"
#endif

#if PROFILE
#include "alice/Base.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteConcreteCode.hh"

/**
 * Serves a similar function to the SEAM Profiler, but records
 * only Alice-specific data.
 */
class AliceProfiler {
public:
  
  // called after each bytecode compilation
  static void ByteCodeCompiled(ByteConcreteCode *bcc, double elapsedMicroseconds);
  
  // called for each bytecode instruction executed
  static void ByteCodeExecuted(ByteCodeInstr::instr ins);
  
  // Dump profiling information to a log file defined by ALICE_PROFILE_LOG
  static void DumpInfo();
  
};

#endif

#endif
