//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __JAVA__CLASS_LOADER_HH__
#define __JAVA__CLASS_LOADER_HH__

#if defined(INTERFACE)
#pragma interface "java/ClassLoader.hh"
#endif

#include "java/Data.hh"

class ClassTable;

class JavaDll ClassLoader: private Block {
protected:
  enum { CLASS_TABLE_POS, SIZE };
private:
  static word wBootstrapClassLoader;

  ClassTable *GetClassTable();
public:
  using Block::ToWord;

  static void Init();

  static ClassLoader *New();
  static ClassLoader *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::ClassLoader);
    return static_cast<ClassLoader *>(b);
  }
  static ClassLoader *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ClassLoader);
    return static_cast<ClassLoader *>(b);
  }

  static ClassLoader *GetBootstrapClassLoader() {
    return ClassLoader::FromWordDirect(wBootstrapClassLoader);
  }
  static word PreloadClass(const char *name); // Hole
  static void PushPreloadFrame(Thread *thread);

  word ResolveClass(JavaString *name); // Class or Future
  word ResolveFieldRef(word theClass, JavaString *name,
		       JavaString *descriptor); // FieldRef or Future
  word ResolveMethodRef(word theClass, JavaString *name,
			JavaString *descriptor); // MethodRef or Future
  word ResolveInterfaceMethodRef(word theClass, JavaString *name,
				 JavaString *descriptor);
    // InterfaceMethodRef or Future
};

#endif
