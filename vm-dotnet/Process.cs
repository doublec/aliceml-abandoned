using System;
using Alice;
using Alice.Values;

class OS_Process_system : Procedure {
    public static Object StaticApply(Object a) {
	// to be determined
	return a;
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class OS_Process_exit : Procedure {
    public static Object StaticApply(Object obj) {
	Environment.Exit((Int32) CommonOp.Sync(obj));
	return Prebound.unit;
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

class OS_Process_getEnv : Procedure {
    public static Object StaticApply(Object a) {
	try {
	    return new TagVal(1,
			      (System.String)
			      Environment.GetEnvironmentVariable((System.String)
								 CommonOp.Sync(a)));
	}
	catch (System.Exception) {
	    return Prebound.Option_NONE;
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

public class Execute {
    public static Object Main(Object obj) {
	Object[] res = new Object[1];

	Object[] OS = new Object[1];

	Object[] OS_Process = new Object[5];
	OS_Process[0] = new OS_Process_exit();     // exit
	OS_Process[1] = (Int32) 1;                 // failure
	OS_Process[2] = new OS_Process_getEnv();   // getEnv
	OS_Process[3] = (Int32) 0;                 // success
	OS_Process[4] = new OS_Process_system();   // system

	OS[0] = OS_Process;                        // $Process

	res[0] = OS;                               // $OS

	return res;
    }
}
