//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2001
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

using System.IO;
using Alice;
using Alice.Values;

class OS_FileSys_getDir: Procedure0 {
    public override object Apply() {
	return Directory.GetCurrentDirectory();
    }
}

public class Execute {
    public static object Main(object obj) {
	return new object[1] {
	    new object[1] {                // FileSys$
		new OS_FileSys_getDir()    // FileSys$.getDir
	    }
	};
    }
}
