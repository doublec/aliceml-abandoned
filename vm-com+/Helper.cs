using System;

class CommonOp {
    public static Object Sync(Object obj) {
	if (obj is Alice.Values.Transient) {
	    return ((Alice.Values.Transient) obj).Await();
	}
	else {
	    return obj;
	}
    }
}

class IntFromString : Alice.Values.Procedure {
    public static Object StaticApply(Object obj) {
	return (Int32) System.Int32.Parse((System.String) CommonOp.Sync(obj));
    }
    public override Object Apply(Object obj) {
	return StaticApply(obj);
    }
}

class StringTokens : Alice.Values.Procedure2 {
    public static Object StaticApply(Object a, Object b) {
	System.String[] subs = ((System.String) CommonOp.Sync(a)).
	    Split(((System.String) CommonOp.Sync(b)).ToCharArray());
	Object val = (Int32) 1;
	
	for (int i = (subs.Length - 1); i >= 0; i--) {
	    Object[] cell = new Object[2];

	    cell[0] = subs[i];
	    cell[1] = val;
	    val = new Alice.Values.TagVal(0, cell);
	}

	return val;
    }
    public override Object Apply(Object a, Object b) {
	return StaticApply(a, b);
    }
}

public class Execute {
    public static Object Main(Object obj) {
	Object[] val = new Object[2];

	val[0] = new IntFromString();
	val[1] = new StringTokens();

	return val;
    }
}
