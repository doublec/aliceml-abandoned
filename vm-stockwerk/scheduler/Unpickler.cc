//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "Unpickler.hh"
#endif

#include "adt/HashTable.hh"
#include "scheduler/TaskStack.hh"
#include "scheduler/Transients.hh"
#include "Unpickler.hh"

//
// The transformer interpreter
//

//--** this has a lot in common with the PrimitiveInterpreter!

ConcreteCode *TransformerInterpreter::Prepare(word /*abstractCode*/) {
  Error("TransformerInterpreter::Prepare: must never be called");
}

void TransformerInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  Assert(closure->GetConcreteCode()->GetInterpreter() == this);
  taskStack->PushFrame(2);
  taskStack->PutWord(1, closure->GetConcreteCode()->GetAbstractCode());
  taskStack->PutUnmanagedPointer(0, this);
}

void TransformerInterpreter::PopFrame(TaskStack *taskStack) {
  taskStack->PopFrame(2);
}

Interpreter::Result
TransformerInterpreter::Run(TaskStack *taskStack, int nargs) {
  Assert(nargs == 0 || nargs == -1);
  if (nargs == -1)
    taskStack->PopFrame(2);
  else
    taskStack->PopFrame(1);
  //--** generalize this so that function can return any Interpreter::Result
  taskStack->PutWord(0, function(taskStack->GetWord(0)));
  return Result(Result::CONTINUE, 1);
}

//
// The unpickler
//
//    pickle    ::= int | chunk | block | closure | transform
//    int       ::= POSINT <uint> | NEGINT <uint>
//    chunk     ::= CHUNK size <byte>*size
//    size      ::= <uint>
//    block     ::= BLOCK label size field*size
//    closure   ::= CLOSURE size field*size
//    label     ::= <uint>
//    field     ::= pickle | reference
//    reference ::= REF id
//    id        ::= <uint>
//    transform ::= TRANSFORM (chunk|reference) field
//
// Well-formedness conditions:
// -- Only backward references are allowed.
// -- The reference within a transform must not reference another transform.
//

u_int Unpickler::ReadInt() {
  int shift = 0;
  int freeBits = sizeof(u_int) * 8 - 1;
  u_int value = 0;
  byte b = inputStream->ReadByte();
  while (b & 0x80) {
    b ^= 0x80;
    if (b >= (1 << freeBits))
      Error("Unpickler::ReadInt: integer too large for this platform");
    value |= (b & 0x7F) << shift;
    shift += 7;
    freeBits -= 7;
    b = inputStream->ReadByte();
  }
  return value;
}

//--** raise exceptions instead of Error
//--** PickleInputStream::ReadByte can block on I/O or raise an exception

word Unpickler::Read() {
  //--** do not use the C stack but the TaskStack instead
  switch (inputStream->ReadByte()) {
  case POSINT: // POSINT <uint>
    return Store::IntToWord(ReadInt());
  case NEGINT: // NEGINT <uint>
    return Store::IntToWord(~ReadInt());
  case CHUNK: // CHUNK size <byte>*size
    {
      u_int size = ReadInt();
      Chunk *chunk = Store::AllocChunk(size);
      word w = chunk->ToWord();
      refs->SlowPush(w);
      char *base = chunk->GetBase();
      while (size--)
	*base++ = inputStream->ReadByte();
      return w;
    }
  case BLOCK: // BLOCK label size field*size
    {
      u_int label = ReadInt();
      if (label > MAX_DATA_LABEL)
	Error("Unpickler::Read: BLOCK label too large for this platform");
      u_int size = ReadInt();
      Block *block = Store::AllocBlock(Store::MakeLabel(label), size);
      word w = block->ToWord();
      refs->SlowPush(w);
      for (u_int i = 0; i < size; i++)
	block->InitArg(i, Read());
      return w;
    }
  case CLOSURE: // CLOSURE size field*size
    {
      u_int size = ReadInt();
      Block *block = Store::AllocBlock(CLOSURE_LABEL, size);
      word w = block->ToWord();
      refs->SlowPush(w);
      for (u_int i = 0; i < size; i++)
	block->InitArg(i, Read());
      return w;
    }
  case REF: // REF id
    {
      u_int id = ReadInt();
      return refs->GetAbsoluteArg(id + 1);
    }
  case TRANSFORM: // TRANSFORM (chunk|reference) field
    {
      Hole *hole = Hole::New();
      word w = hole->ToWord();
      refs->SlowPush(w);
      word transformerName = Read();
      word abstractRepresentation = Read();
      TransformerInterpreter *interpreter =
	static_cast<TransformerInterpreter *>
	(Store::DirectWordToUnmanagedPointer
	 (transformers->GetItem(transformerName)));
      ConcreteCode *concreteCode =
	ConcreteCode::New(abstractRepresentation, interpreter, 0);
      Closure *closure = Closure::New(concreteCode, 0);
      hole->Fill(Byneed::New(closure->ToWord())->ToWord());
      return w;
    }
  default:
    Error("Pickler::Read: invalid tag");
  }
}
