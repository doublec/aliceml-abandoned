//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt, 2000
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

using System;
using Alice;
using Alice.Values;

class OS_Process_system: Procedure {
    public override object Apply(object a) {
	// to be determined
	return a;
    }
}

class OS_Process_exit: Procedure {
    public override object Apply(object obj) {
	Environment.Exit((Int32) CommonOp.Sync(obj));
	return Prebound.unit;
    }
}

class OS_Process_getEnv: Procedure {
    public override object Apply(object a) {
	string res =
	    Environment.GetEnvironmentVariable((string) CommonOp.Sync(a));
	if (res == null) {
	    return Prebound.Option_NONE;
	} else {
	    return new TagVal(1, res);
	}
    }
}

public class Execute {
    public static object Main(object obj) {
	return new object[3] {
	    null,                              //--** $OS$
	    null,                              //--** $OS_PROCESS$
	    new object[1] {                    // OS$
		new object[6] {                // OS$.Process$
		    null,                      //--** OS$.Process$.$status
		    new OS_Process_exit(),     // OS$.Process$.exit
		    1,                         // OS$.Process$.failure
		    new OS_Process_getEnv(),   // OS$.Process$.getEnv
		    0,                         // OS$.Process$.success
		    new OS_Process_system(),   // OS$.Process$.system
		}
	    }
	};
    }
}
