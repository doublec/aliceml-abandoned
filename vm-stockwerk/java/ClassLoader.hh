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

class DllExport ClassLoader: private Block {
protected:
  enum { CLASS_TABLE_POS, SIZE };
private:
  static const u_int initialTableSize = 19; //--** to be determined
public:
  using Block::ToWord;

  static ClassLoader *New();

  word ResolveClass(JavaString *name); // Class or Future
  word ResolveFieldRef(JavaString *className, JavaString *name,
		       JavaString *descriptor); // FieldRef or Future
  word ResolveMethodRef(JavaString *className, JavaString *name,
			JavaString *descriptor); // MethodRef or Future
  word ResolveInterfaceMethodRef(JavaString *className, JavaString *name,
				 JavaString *descriptor);
    // InterfaceMethodRef or Future
};

#endif
