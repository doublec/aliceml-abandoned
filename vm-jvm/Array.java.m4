package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class Array {
    /** <code>val maxLen : int </code>*/
    final public static Int maxLen = new Int(DMLArray.maxLen);

    final public static class ArraY extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"Array.array");
	    DMLValue arg=args[0].request();
	    int ar=0;
	    if (arg instanceof Int)
		ar = ((Int) arg).getInt();
	    else
		error("argument #1 not Int",val);
	    return new DMLArray(ar,args[1]);
	}
    }
    /** <code>val array : (int * 'a) -> 'a array </code>*/
    final public static ArraY array = new ArraY();

    final public static class FromList extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.fromList");
	    DMLValue arg=args[0].request();
	    return new DMLArray(arg);
	}
    }
    /** <code>val fromList : 'a list -> 'a array </code>*/
    final public static FromList fromList = new FromList();

    final public static class Tabulate extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"Array.tabulate");
	    int ar=0;
	    DMLValue arg=args[0].request();
	    if (arg instanceof Int)
		ar = ((Int) arg).getInt();
	    else
		error("argument #1 not DMLArray",val);
	    return new DMLArray(args[1],ar);
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a array </code>*/
    final public static Tabulate tabulate = new Tabulate();

    final public static class Length extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.length");
	    DMLValue arg=args[0].request();
	    if (arg instanceof DMLArray)
		return new Int(((DMLArray) arg).array.length);
	    else
		return error("argument #1 not Int",val);
	}
    }
    /** <code>val length : 'a array -> int </code>*/
    final public static Length length = new Length();

    final public static class Sub extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"Array.sub");
	    DMLValue array = args[0].request();
	    if (array instanceof DMLArray) {
		DMLValue idx = args[1].request();
		if (idx instanceof Int)
		    return ((DMLArray) array).sub(((Int) idx).getInt());
		else
		    return error("argument #2 not Int",val);
	    }
	    else
		return error("argument #1 not DMLArray",val);
	}
    }
    /** <code>val sub : ('a array * int) -> 'a </code>*/
    final public static Sub sub = new Sub();

    final public static class Update extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,3,"Array.update");
	    DMLValue array = args[0].request();
	    if (array instanceof DMLArray) {
		DMLValue idx = args[1].request();
		if (idx instanceof Int)
		    return ((DMLArray) array).
			update(((Int) idx).getInt(),args[2]);
		else
		    return error("argument #2 not Int",val);
	    }
	    else
		return error("argument #1 not DMLArray",val);
	}
    }
    /** <code>val update : ('a array * int * 'a) -> unit </code>*/
    final public static Update update = new Update();

    final public static class Extract extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,3,"Array.extract");
	    DMLValue array = args[0].request();
	    if (array instanceof DMLArray) {
		DMLValue fr = args[1].request();
		if (fr instanceof Int) {
		    DMLValue to = args[2].request();
		    if (to==Option.NONE) {
			DMLArray a=(DMLArray) array;
			return a.extract(((Int) fr).getInt(),
					 a.array.length);
		    }
		    if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (cv.getConstructor()==Option.SOME) {
			    to=cv.getContent();
			    if (to instanceof Int)
				return ((DMLArray) array).
				    extract(((Int) fr).getInt(),
					    ((Int) to).getInt());
			}
		    }
		    return error("argument #3 not Int option",val);
		}
		else
		    return error("argument #2 not Int",val);
	    }
	    else
		return error("argument #1 not DMLArray",val);
	}
    }
    /** <code>val extract : ('a array * int * int option) -> 'a vector </code>*/
    final public static Extract extract = new Extract();

    final public static class Copy extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,5,"Array.copy");
	    DMLValue array = args[0].request();
	    if (array instanceof DMLArray) {
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return error("argument #2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue len = args[2].request();
		int le = 0;
		if (len==Option.NONE) {
		    le = ((DMLArray) array).array.length - from;
		} else if (len instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) len;
		    DMLValue c = cv.getConstructor();
		    if (c==Option.SOME) {
			len=cv.getContent();
			if (len instanceof Int)
			    le = ((Int) len).getInt();
		    }
		}
		else
		    return error("argument #3 not Int option",val);
		DMLValue dest = args[3].request();
		if (!(dest instanceof DMLArray))
		    return error("argument #4 not DMLArray",val);
		DMLValue di = args[4].request();
		if (!(di instanceof Int))
		    return error("argument #5 not Int",val);
		return ((DMLArray) array)
		    .copy(from,
			  le,
			  (DMLArray) dest,
			  ((Int) di).getInt());
	    } else
		return error("argument #1 not DMLArray",val);
	}
    }
    /** <code>val copy : {src : 'a array, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    final public static Copy copy = new Copy();

    final public static class CopyVec extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,5,"Array.copyVec");
	    DMLValue vector = args[0].request();
	    //	    System.err.println("vector: "+vector.getClass());
	    if (vector instanceof DMLVector) {
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return error("argument #2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue len = args[2].request();
		int le = 0;
		if (len==Option.NONE) {
		    le = ((DMLVector) vector).vector.length - from;
		} else if (len instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) len;
		    DMLValue c = cv.getConstructor();
		    if (c==Option.SOME) {
			len=cv.getContent();
			if (len instanceof Int)
			    le = ((Int) len).getInt();
		    }
		}
		else
		    return error("argument #3 not Int option",val);
		DMLValue dest = args[3].request();
		if (!(dest instanceof DMLArray))
		    return error("argument #4 not DMLArray",val);
		DMLValue di = args[4].request();
		if (!(di instanceof Int))
		    return error("argument #5 not Int",val);
		return ((DMLVector) vector)
		    .copyVec(from,
			     le,
			     (DMLArray) dest,
			     ((Int) di).getInt());
	    } else
		return error("argument #1 not DMLVector",val);
	}
    }
    /** <code>val copyVec : {src : 'a vector, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    final public static CopyVec copyVec = new CopyVec();

    final public static class Appi extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.appi");
	    return new Appi1(args[0].request());
	}
	final public static class Appi1 extends Builtin {
	    public DMLValue fun = null;
	    public Appi1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,3,"Array.appi1");
		DMLValue array = args[0].request();
		if (!(array instanceof DMLArray))
		    return error("argument #1 not DMLArray",val);
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return error("argument #2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLArray) array).array.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return error("argument #3 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return error("argument #3 not Int option",val);
		return ((DMLArray) array).
		    appi(from,
			 toint,
			 fun);
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit </code>*/
    final public static Appi appi = new Appi();

    final public static class App extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.app");
	    return new App1(args[0].request());
	}
	final public static class App1 extends Builtin {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.app1");
		DMLValue array = args[0].request();
		if (!(array instanceof DMLArray))
		    return error("argument not DMLArray",val);
		return ((DMLArray) array).app(fun);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a array -> unit </code>*/
    final public static App app = new App();

    final public static class Foldli extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.foldli");
	    return new Foldli1(args[0].request());
	}
	final public static class Foldli1 extends Builtin {
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.foldli1");
		return new Foldli2(fun, args[0].request());
	    }
	    final public static class Foldli2 extends Builtin {
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,3,"Array.foldli2");
		DMLValue array = args[0].request();
		if (!(array instanceof DMLArray))
		    return error("argument #1 not DMLArray",val);
		DMLValue from = args[1].request();
		if (!(from instanceof Int))
		    return error("argument #2 not Int",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLArray) array).array.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return error("argument #3 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return error("argument #3 not Int option",val);
		return ((DMLArray) array).
		    foldli(fun,
			   init,
			   ((Int) from).getInt(),
			   toint);
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    final public static Foldli foldli = new Foldli();

    final public static class Foldri extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.foldri");
	    return new Foldri1(args[0].request());
	}
	final public static class Foldri1 extends Builtin {
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.foldri1");
		return new Foldri2(fun, args[0].request());
	    }
	    final public static class Foldri2 extends Builtin {
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,3,"Array.foldri2");
		DMLValue array = args[0].request();
		if (!(array instanceof DMLArray))
		    return error("argument #1 not DMLArray",val);
		DMLValue from = args[1].request();
		if (!(from instanceof Int))
		    return error("argument #2 not Int",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLArray) array).array.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return error("argument #3 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return error("argument #3 not Int option",val);
		return ((DMLArray) array).
		    foldri(fun,
			   init,
			   ((Int) from).getInt(),
			   toint);
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    final public static Foldri foldri = new Foldri();

    final public static class Foldl extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.foldl");
	    return new Foldl1(args[0].request());
	}
	final public static class Foldl1 extends Builtin {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.foldl1");
		return new Foldl2(fun, args[0]);
	    }
	    final public static class Foldl2 extends Builtin {
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		    DMLValue[] args=fromTuple(val,1,"Array.foldl2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof DMLArray))
			return error("argument not DMLArray",val);
		    return ((DMLArray) array).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    final public static Foldl foldl = new Foldl();

    final public static class Foldr extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.foldr");
	    return new Foldr1(args[0].request());
	}
	final public static class Foldr1 extends Builtin {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.foldr1");
		return new Foldr2(fun, args[0]);
	    }
	    final public static class Foldr2 extends Builtin {
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		    DMLValue[] args=fromTuple(val,1,"Array.foldr2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof DMLArray))
			return error("argument not DMLArray",val);
 		    return ((DMLArray) array).foldr(fun,init);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    final public static Foldr foldr = new Foldr();

    final public static class Modifyi extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.modifyi");
	    return new Modifyi1(args[0].request());
	}
	final public static class Modifyi1 extends Builtin {
	    DMLValue fun = null;
	    Modifyi1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,3,"Array.modifyi1");
		DMLValue array = args[0].request();
		if (!(array instanceof DMLArray))
		    return error("argument #1 not DMLArray",val);
		DMLValue from = args[1].request();
		if (!(from instanceof Int))
		    return error("argument #2 not Int",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLArray) array).array.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return error("argument #2 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return error("argument #2 not Int option",val);
		return ((DMLArray) array).
		    modifyi(fun,
			    ((Int) from).getInt(),
			    toint);
	    }
	}
    }
    /** <code>val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit </code>*/
    final public static Modifyi modifyi = new Modifyi();

    final public static class Modify extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Array.modify");
	    return new Modify1(args[0]);
	}
	final public static class Modify1 extends Builtin {
	    DMLValue fun = null;
	    Modify1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
		DMLValue[] args=fromTuple(val,1,"Array.modify1");
		DMLValue array = args[0].request();
	    if (!(array instanceof DMLArray))
		return error("argument not DMLArray",val);
	    else
		return ((DMLArray) array).modify(fun);
	    }
	}
    }
    /** <code>val modify : ('a -> 'a) -> 'a array -> unit</code>*/
    final public static Modify modify = new Modify();

    // Hilfsfunktionen
    final public static DMLValue[] fromTuple
	(DMLValue v, /** <code>value-Tuple</code>*/
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) throws java.rmi.RemoteException {
	v=v.request();
	if (v instanceof DMLTuple) {
	    DMLTuple t=(DMLTuple) v;
	    if (t.getArity()==ea) {
		DMLValue[] vals = new DMLValue[ea];
		for(int i=0; i<ea; i++)
		    vals[i]=t.getByIndex(i);
		return vals;
	    }
	    else
		error("wrong number of arguments in "+errMsg, v);
	}
	else
	    error("wrong argument type for "+errMsg,v);
	return null;
    }

    final protected static DMLValue error
	(java.lang.String msg, DMLValue v) throws java.rmi.RemoteException {
	// sonst: Fehler
	DMLValue[] err = {
	    new de.uni_sb.ps.dml.runtime.String(msg),
	    v};
	return Constants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
