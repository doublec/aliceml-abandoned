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

#ifndef __ALICE__PRIMITIVES__PRIMITIVE_TABLE_HH__
#define __ALICE__PRIMITIVES__PRIMITIVE_TABLE_HH__

#if defined(INTERFACE)
#pragma interface "alice/primitives/PrimitiveTable.hh"
#endif

#include "generic/TaskManager.hh"

class TaskStack;

class PrimitiveTable {
private:
  static word table;

  typedef TaskManager::Result (*function)(TaskStack *);

  static void Register(const char *name, word value);
  static void Register(const char *name, function value, u_int arity);
  static void Register(const char *name, function value, u_int arity,
		       u_int frameSize);
  static void RegisterUniqueConstructor(const char *name);

  static void RegisterUnqualified();
  static void RegisterArray();
  static void RegisterChar();
  static void RegisterFuture();
  static void RegisterGeneral();
  static void RegisterGlobalStamp();
  static void RegisterHole();
  static void RegisterInt();
  static void RegisterList();
  static void RegisterMath();
  static void RegisterOption();
  static void RegisterReal();
  static void RegisterString();
  static void RegisterThread();
  static void RegisterUnsafe();
  static void RegisterVector();
  static void RegisterWord();
public:
  static word Future_Future;
  static word Future_await;
  static word General_Chr;
  static word General_Div;
  static word General_Overflow;
  static word General_Size;
  static word General_Subscript;
  static word Hole_Cyclic;
  static word Hole_Hole;
  static word Vector_tabulate_cont;

  static void Init();
  static word Lookup(Chunk *name);
};

#endif __ALICE__PRIMITIVES__PRIMITIVE_TABLE_HH__
