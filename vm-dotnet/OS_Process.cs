//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt and Andreas Rossberg, 2000-2001
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
	//--** to be determined
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
	return new object[1] {
	    new object[7] {                // Process$
		null,                      //--** Process$.$status
		null,                      //--** Process$.atExn
		new OS_Process_exit(),     // Process$.exit
		1,                         // Process$.failure
		new OS_Process_getEnv(),   // Process$.getEnv
		0,                         // Process$.success
		new OS_Process_system(),   // Process$.system
	    }
	};
    }
}
