//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/AliceLanguageLayer.hh"
#endif

#include "alice/Types.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/Guid.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/AliceDebuggerEvent.hh"
#if DEBUGGER
#include "alice/DebugEnvironment.hh"
#include "alice/AliceDebuggerEvent.hh"
#endif
#if DEBUG_CHECK
#include "alice/AbstractCodeFrame.hh"
#endif

word AliceLanguageLayer::TransformNames::primitiveValue;
word AliceLanguageLayer::TransformNames::primitiveFunction;
word AliceLanguageLayer::TransformNames::function;
word AliceLanguageLayer::TransformNames::constructor;
word AliceLanguageLayer::aliceHome;
word AliceLanguageLayer::rootUrl;
word AliceLanguageLayer::commandLineArguments;
word AliceLanguageLayer::remoteCallback;
word AliceLanguageLayer::undefinedValue;

concrete_constructor AliceLanguageLayer::concreteCodeConstructor;

static word AlicePrimitiveValueHandler(word x) {
  return PrimitiveTable::LookupValue(Chunk::FromWordDirect(x));
}

static word AlicePrimitiveFunctionHandler(word x) {
  return PrimitiveTable::LookupFunction(Chunk::FromWordDirect(x));
}

static word AliceFunctionHandler(word x) {
  TagVal *abstractCode = TagVal::FromWordDirect(x);
  return AliceLanguageLayer::concreteCodeConstructor(abstractCode);
}

static word AliceConstructorHandler(word x) {
  Tuple *tuple = Tuple::FromWordDirect(x);
  tuple->AssertWidth(2);
  String *name = String::FromWordDirect(tuple->Sel(0));
  Block *guid = Store::DirectWordToBlock(tuple->Sel(1));
  Constructor *constructor = Constructor::New(name, guid);
  return constructor->ToWord();
}

void AliceLanguageLayer::Init(const char *home, int argc, const char *argv[]) {
  String *alicePrimitiveValue = String::New("Alice.primitive.value");
  TransformNames::primitiveValue = alicePrimitiveValue->ToWord();
  RootSet::Add(TransformNames::primitiveValue);
  Unpickler::RegisterHandler(alicePrimitiveValue, AlicePrimitiveValueHandler);

  String *alicePrimitiveFunction = String::New("Alice.primitive.function");
  TransformNames::primitiveFunction = alicePrimitiveFunction->ToWord();
  RootSet::Add(TransformNames::primitiveFunction);
  Unpickler::RegisterHandler(alicePrimitiveFunction,
			     AlicePrimitiveFunctionHandler);

  String *aliceFunction = String::New("Alice.function");
  TransformNames::function = aliceFunction->ToWord();
  RootSet::Add(TransformNames::function);
  Unpickler::RegisterHandler(aliceFunction, AliceFunctionHandler);

  String *aliceConstructor = String::New("Alice.constructor");
  TransformNames::constructor = aliceConstructor->ToWord();
  RootSet::Add(TransformNames::constructor);
  Unpickler::RegisterHandler(aliceConstructor, AliceConstructorHandler);

  // Initialize aliceHome, making sure it ends in a trailing slash:
  u_int n = std::strlen(home);
  if (n != 0 && home[n - 1] == '/') {
    aliceHome = String::New(home, n)->ToWord();
  } else {
    String *homeString = String::New(n + 1);
    u_char *p = homeString->GetValue();
    std::memcpy(p, home, n);
    p[n] = '/';
    aliceHome = homeString->ToWord();
  }
  RootSet::Add(aliceHome);

  // Initialize rootUrl:
  if (argc < 2) {
    rootUrl = Store::IntToWord(0);
  } else {
    rootUrl = String::New(argv[1])->ToWord();
    argv++; argc--;
  }
  RootSet::Add(rootUrl);

  // Initialize commandLineArguments:
  commandLineArguments = Store::IntToWord(Types::nil);
  argv++; argc--;
  for (u_int i = argc; i--; ) {
    TagVal *cons = TagVal::New(Types::cons, 2);
    cons->Init(0, String::New(argv[i])->ToWord());
    cons->Init(1, commandLineArguments);
    commandLineArguments = cons->ToWord();
  }
  RootSet::Add(commandLineArguments);

  // Initialize remoteCallback:
  remoteCallback = Store::IntToWord(0); //--** needs to be preregistered
  RootSet::Add(remoteCallback);

  Constructor::Init();
  Guid::Init();
  LazySelInterpreter::Init();
  AbstractCodeInterpreter::Init();
  PrimitiveTable::Init();
#if HAVE_LIGHTNING
  NativeCodeInterpreter::Init();
  // to be done: Memory should be enlarged dynamically
#if defined(JIT_STORE_DEBUG)
  u_int codeSizeInChunks = 100;
#else
  u_int codeSizeInChunks = 100; // was 40
#endif
  NativeCodeJitter::Init(codeSizeInChunks * STORE_MEMCHUNK_SIZE);

  const char *jitMode = std::getenv("ALICE_JIT_MODE");
  if (jitMode != NULL && !strcmp(jitMode, "0"))
    concreteCodeConstructor = AliceConcreteCode::New;
  else
    concreteCodeConstructor = NativeConcreteCode::New;
#else
  concreteCodeConstructor = AliceConcreteCode::New;
#endif
#if DEBUG_CHECK
  AbstractCodeFrame::Init();
#endif
#if DEBUGGER
  // Initialize Debugger Components
  DebugEnvironment::Init();
  AliceEventAccessor::Init();
#endif
  undefinedValue = Store::IntToWord(42);
  RootSet::Add(undefinedValue);

}
