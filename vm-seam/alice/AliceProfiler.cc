
#if defined(INTERFACE)
#pragma implementation "alice/AliceProfiler.hh"
#endif

#if PROFILE
#include "alice/AbstractCode.hh"
#include "alice/AliceProfiler.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeSourceLocations.hh"


namespace {

  class UIntStat {
  private:
    u_int samples, min, max, total;
  public:
    
    UIntStat() : samples(0), min(0xffffffff), max(0), total(0) {}
    
    void Sample(u_int x) {
      if (x < min) {
	min = x;
      }
      if (x > max) {
	max = x;
      }
      samples++;
      total += x;
    }
    
    void Dump(FILE *file, const char* name) {
      // statistic, samples, total, min, max, mean
      std::fprintf(file, "%s, %"U_INTF", %"U_INTF", %"U_INTF", %"U_INTF", %.2f\n",
        name, samples, total, min, max, static_cast<float>(total) / static_cast<float>(samples));
    }
  };

  u_int executedInstrCounts[ByteCodeInstr::NUMBER_OF_INSTRS];
  UIntStat numRegs, numInstrs, numBytes, immediateSize,
    sourceLocsSize, numInlineAppVars, nonSubstClosureSize,
    closeReturnLength;

}


void AliceProfiler::ByteCodeCompiled(ByteConcreteCode *bcc) {
  TagVal *abstractCode = bcc->GetAbstractCode();
  
  numRegs.Sample(bcc->GetNLocals());
  numBytes.Sample(bcc->GetByteCode()->GetSize());
  numInstrs.Sample(ByteCode::NumInstrs(bcc->GetByteCode()));
  immediateSize.Sample(reinterpret_cast<Block*>(bcc->GetImmediateArgs())->GetSize());
  nonSubstClosureSize.Sample(AbstractCode::GetNonSubstClosureSize(abstractCode));
  sourceLocsSize.Sample(ByteCodeSourceLocations::Size(bcc->GetSourceLocations()));
  numInlineAppVars.Sample(bcc->GetInlineInfo()->NumInlinedAppVars());
  closeReturnLength.Sample(AbstractCode::GetCloseReturnLength(abstractCode));
}


void AliceProfiler::ByteCodeExecuted(ByteCodeInstr::instr ins) {
  executedInstrCounts[ins]++;
}


void AliceProfiler::DumpInfo() {
  const char *apf = getenv("ALICE_PROFILE_LOG");
  if (apf != NULL) {
    
    const char* file =
      *apf == '\0' ? "alice_profile_log.csv" : apf;
    
    FILE *logFile;
    if ((logFile = std::fopen(file, "w")) == NULL) {
      Error("AliceProfiler::DumpInfo: unable to open log file");
    }
    
    std::fprintf(logFile, "instruction name, execution count\n");
    u_int total = 0;
    for (u_int i=0; i<ByteCodeInstr::NUMBER_OF_INSTRS; i++) {
      total += executedInstrCounts[i];
      std::fprintf(logFile, "%s, %"U_INTF"\n",
        ByteCode::LookupName(static_cast<ByteCodeInstr::instr>(i)), executedInstrCounts[i]);
    }
    std::fprintf(logFile, "total, %"U_INTF"\n\n", total);
    
    std::fprintf(logFile, "per-bytecode statistic, samples, total, min, max, mean\n");
    numRegs.Dump(logFile, "registers required");
    numBytes.Dump(logFile, "bytecode size (bytes)");
    numInstrs.Dump(logFile, "bytecode size (instrs)");
    nonSubstClosureSize.Dump(logFile, "non-subst closure size (values)");
    immediateSize.Dump(logFile, "immediate size (values)");
    sourceLocsSize.Dump(logFile, "source locations size (nodes)");
    numInlineAppVars.Dump(logFile, "inlined app vars (recursive)");
    closeReturnLength.Dump(logFile, "close-return length");
    
    std::fclose(logFile);
  }
}


#endif

  