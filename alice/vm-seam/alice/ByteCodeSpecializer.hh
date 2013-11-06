
#ifndef __ALICE_BYTE_CODE_SPECIALIZER_HH__
#define __ALICE_BYTE_CODE_SPECIALIZER_HH__

#if defined(INTERFACE)
#pragma inferface "alice/ByteCodeSpecializer.hh"
#endif

#include "alice/Base.hh"
#include "alice/ByteConcreteCode.hh"


class ByteCodeSpecializer {
private:
  static const u_int RECORD_MASK = 0xff;
  static const u_int REMOVE_OLD_MASK = 0xffff;
  static const u_int DUMP_MASK = 0xfffff;

  static const u_int RECORDS_PER_SPECIALIZE = 8;

  static u_int calls, numSpecialized;
  static word closures;
  static bool trace;
  
  static bool RecordCall(Closure *c);
  static bool CanBeSpecialized(ByteConcreteCode *bcc);
  static bool Specialize(Closure *c);
  static void RemoveOldStats();

public:
  
  static void Init();
  
  
  /**
   * Record a call to the specified closure, and possibly replace
   * the ConcreteCode in the closure with a specialized version.
   */
  static void IncCalls(Closure *c) {
      calls++;
      if ((calls & RECORD_MASK) == 0) {
        RecordCall(c);
      }
  }
  
  
  /**
   * Dump internal information to stderr
   */
  static void DumpStats();
  
};


#endif