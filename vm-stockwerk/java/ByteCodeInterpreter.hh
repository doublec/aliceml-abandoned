#ifndef __JAVA__BYTE_CODE_INTERPRETER_HH__
#define __JAVA__BYTE_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "java/ByteCodeInterpreter.hh"
#endif

class DllExport ByteCodeInterpreter : public Interpreter {
public:
  // Exported ByteCodeIntpreter Instance
  static ByteCodeInterpreter *self;
  // ByteCodeInterpreter Constructor
  ByteCodeIntepreter : Interpreter () {}
  // ByteCodeInterpreter Static Constructor
  static void Init() {
    self = new ByteCodeInterpreter();
  }
  // Handler Methods
  virtual Block *GetAbstractRepresentation(ConcreteRepresentation *);
  // Frame Handling
  virtual Result Run();
  virtual Result Handle();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
