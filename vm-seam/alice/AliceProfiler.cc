
#if defined(INTERFACE)
#pragma implementation "alice/AliceProfiler.hh"
#endif

#if PROFILE
#include <fstream>
#include <iostream>
#include <limits>
#include "alice/AbstractCode.hh"
#include "alice/AliceProfiler.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeSourceLocations.hh"


namespace {

  template<typename A>
  class Stat {
  private:
    u_int samples;
    A min, max, total;
  public:
    
    Stat() :
      samples(0),
      min(std::numeric_limits<A>::max()),
      max(std::numeric_limits<A>::min()),
      total(0) {}
    
    void Sample(A x) {
      if (x < min) {
	min = x;
      }
      if (x > max) {
	max = x;
      }
      samples++;
      total += x;
    }
    
    void Dump(std::ostream& out, const char* name) {
      // statistic, samples, total, min, max, mean
      out << name << ", "
          << samples << ", "
          << total << ", "
	  << min << ", "
	  << max << ", "
	  << (static_cast<float>(total) / static_cast<float>(samples)) << "\n";
    }
  };

  u_int executedInstrCounts[ByteCodeInstr::NUMBER_OF_INSTRS];
  Stat<u_int> numRegs, numInstrs, numBytes, immediateSize,
    sourceLocsSize, numInlineAppVars, nonSubstClosureSize,
    closeReturnLength;
  Stat<double> compilationMilliseconds;

}


void AliceProfiler::ByteCodeCompiled(ByteConcreteCode *bcc, double elapsedMicroseconds) {
  TagVal *abstractCode = bcc->GetAbstractCode();
  
  numRegs.Sample(bcc->GetNLocals());
  numBytes.Sample(bcc->GetByteCode()->GetSize());
  numInstrs.Sample(ByteCode::NumInstrs(bcc->GetByteCode()));
  immediateSize.Sample(reinterpret_cast<Block*>(bcc->GetImmediateArgs())->GetSize());
  nonSubstClosureSize.Sample(AbstractCode::GetNonSubstClosureSize(abstractCode));
  sourceLocsSize.Sample(ByteCodeSourceLocations::Size(bcc->GetSourceLocations()));
  numInlineAppVars.Sample(bcc->GetInlineInfo()->NumInlinedAppVars());
  closeReturnLength.Sample(AbstractCode::GetCloseReturnLength(abstractCode));
  compilationMilliseconds.Sample(elapsedMicroseconds / 1000.0);
}


void AliceProfiler::ByteCodeExecuted(ByteCodeInstr::instr ins) {
  executedInstrCounts[ins]++;
}


void AliceProfiler::DumpInfo() {
  const char *apf = getenv("ALICE_PROFILE_LOG");
  if (apf != NULL) {
    
    const char* file =
      *apf == '\0' ? "alice_profile_log.csv" : apf;
    
    std::ofstream out(file, std::ios::out);
    if (!out.is_open()) {
      Error("AliceProfiler::DumpInfo: unable to open log file");
    }
    
    out << "instruction name, execution count\n";
    u_int total = 0;
    for (u_int i=0; i<ByteCodeInstr::NUMBER_OF_INSTRS; i++) {
      total += executedInstrCounts[i];
      out << ByteCode::LookupName(static_cast<ByteCodeInstr::instr>(i)) << ", "
          << executedInstrCounts[i] << "\n";
    }
    out << "total, " << total << "\n\n";
    
    out << "per-bytecode statistic, samples, total, min, max, mean\n";
    compilationMilliseconds.Dump(out, "compilation time (milliseconds)");
    numRegs.Dump(out, "registers required");
    numBytes.Dump(out, "bytecode size (bytes)");
    numInstrs.Dump(out, "bytecode size (instrs)");
    nonSubstClosureSize.Dump(out, "non-subst closure size (values)");
    immediateSize.Dump(out, "immediate size (values)");
    sourceLocsSize.Dump(out, "source locations size (nodes)");
    numInlineAppVars.Dump(out, "inlined app vars (recursive)");
    closeReturnLength.Dump(out, "close-return length");
  }
}


#endif

  