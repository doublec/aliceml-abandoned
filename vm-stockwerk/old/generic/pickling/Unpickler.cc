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
#pragma implementation "generic/pickling/Unpickler.hh"
#endif

#include "generic/pickling/PickleInputStream.hh"
#include "generic/pickling/Unpickler.hh"

u_int ReadInt(PickleInputStream *inputStream) {
  int shift = 0;
  int freeBits = sizeof(u_int) * 8 - 1;
  u_int value = 0;
  PrimPickle::byte b = inputStream->ReadByte();
  while (b & 0x80) {
    b ^= 0x80;
    if (b >= (1 << freeBits))
      return static_cast<u_int>(INVALID_INT); //--** not nice
    value |= (b & 0x7F) << shift;
    shift += 7;
    freeBits -= 7;
    b = inputStream->ReadByte();
  }
  return value;
}

//--** raise exceptions instead of Error
//--** PickleInputStream::ReadByte can block on I/O or raise an exception

/*
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
*/
