package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class Vector {
    /** <code>val maxLen : int </code>*/
    final public static DMLInt maxLen = new DMLInt(DMLVector.maxLen);

    final protected static class FromList extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.fromList");
	    DMLValue arg=args[0].request();
	    return new DMLVector(arg);
	}
    }
    /** <code>val fromList : 'a list -> 'a vector </code>*/
    final public static FromList fromList = new FromList();

    final protected static class Tabulate extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,2,"Vector.tabulate");
	    int ar=0;
	    DMLValue arg=args[0].request();
	    if (arg instanceof DMLInt)
		ar = ((DMLInt) arg).getInt();
	    else
		error("argument #1 not DMLArray",val);
	    return new DMLVector(args[1],ar);
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a vector </code>*/
    final public static Tabulate tabulate = new Tabulate();

    final protected static class Length extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.length");
	    DMLValue arg=args[0].request();
	    if (arg instanceof DMLVector)
		return new DMLInt(((DMLVector) arg).vector.length);
	    else
		return error("argument #1 not DMLInt",val);
	}
    }
    /** <code>val length : 'a vector -> int </code>*/
    final public static Length length = new Length();

    final protected static class Sub extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,2,"Vector.sub");
	    DMLValue vector = args[0].request();
	    if (vector instanceof DMLVector) {
		DMLValue idx = args[1].request();
		if (idx instanceof DMLInt)
		    return ((DMLVector) vector).sub(((DMLInt) idx).getInt());
		else
		    return error("argument #2 not DMLInt",val);
	    }
	    else
		return error("argument #1 not DMLVector",val);
	}
    }
    /** <code>val sub : ('a vector * int) -> 'a </code>*/
    final public static Sub sub = new Sub();

    final private static class Extract extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,3,"Vector.extract");
	    DMLValue vector = args[0].request();
	    if (vector instanceof DMLVector) {
		DMLValue fr = args[1].request();
		if (fr instanceof DMLInt) {
		    DMLValue to = args[2].request();
		    if (to==Option.NONE) {
			DMLVector a=(DMLVector) vector;
			return a.extract(((DMLInt) fr).getInt(),
					 a.vector.length);
		    }
		    if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (cv.getConstructor()==Option.SOME) {
			    to=cv.getContent();
			    if (to instanceof DMLInt)
				return ((DMLVector) vector).
				    extract(((DMLInt) fr).getInt(),
					    ((DMLInt) to).getInt());
			}
		    }
		    return error("argument #3 not DMLInt option",val);
		}
		else
		    return error("argument #2 not DMLInt",val);
	    }
	    else
		return error("argument #1 not DMLVector",val);
	}
    }
    /** <code>val extract : ('a vector * int * int option) -> 'a vector </code>*/
    final public static Extract extract = new Extract();

    final protected static class Concat extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.fromList");
	    DMLValue arg=args[0].request();
	    if (arg instanceof DMLList)
		return DMLVector.concat((DMLList) arg);
	    else
		return error("argument not DMLList",val);
	}
    }
    /** <code>val concat : 'a vector list -> 'a vector </code>*/
    final public static Concat concat = new Concat();

    final private static class Mapi extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.mapi");
	    return new Mapi1(args[0].request());
	}
	final private static class Mapi1 extends DMLBuiltin {
	    private DMLValue fun = null;
	    private Mapi1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,3,"Vector.mapi1");
		DMLValue vector = args[0].request();
		if (!(vector instanceof DMLVector))
		    return error("argument #1 not DMLVector",val);
		DMLValue from = args[1].request();
		if (!(from instanceof DMLInt))
		    return error("argument #2 not DMLInt",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLVector) vector).vector.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof DMLInt))
			    return error("argument #3 not DMLInt option",val);
			toint=((DMLInt) iv).getInt();
		    }
		} else
		    return error("argument #3 not DMLInt option",val);
		return ((DMLVector) vector).
		    mapi(fun,
			 ((DMLInt) from).getInt(),
			 toint);
	    }
	}
    }
    /** <code>val mapi : ((int * 'a) -> 'b) -> ('a vector * int * int option) -> 'b vector </code>*/
    final public static Mapi mapi = new Mapi();

    final private static class Map extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.map");
	    return new Map1(args[0].request());
	}
	final private static class Map1 extends DMLBuiltin {
	    DMLValue fun = null;
	    Map1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.map1");
		DMLValue vector = args[0].request();
		if (!(vector instanceof DMLVector))
		    return error("argument not DMLVector",val);
		return ((DMLVector) vector).map(fun);
	    }
	}
    }
    /** <code>val map : ('a -> 'b) -> 'a vector -> 'b vector </code>*/
    final public static Map map = new Map();

    final private static class Appi extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.appi");
	    return new Appi1(args[0].request());
	}
	final private static class Appi1 extends DMLBuiltin {
	    private DMLValue fun = null;
	    private Appi1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,3,"Vector.appi1");
		DMLValue vector = args[0].request();
		if (!(vector instanceof DMLVector))
		    return error("argument #1 not DMLVector",val);
		DMLValue from = args[1].request();
		if (!(from instanceof DMLInt))
		    return error("argument #2 not DMLInt",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((DMLVector) vector).vector.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof DMLInt))
			    return error("argument #3 not DMLInt option",val);
			toint=((DMLInt) iv).getInt();
		    }
		} else
		    return error("argument #3 not DMLInt option",val);
		return ((DMLVector) vector).
		    appi(((DMLInt) from).getInt(),
			 toint,
			 fun);
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a vector * int * int option) -> unit </code>*/
    final public static Appi appi = new Appi();

    final private static class App extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.app");
	    return new App1(args[0].request());
	}
	final private static class App1 extends DMLBuiltin {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.app1");
		DMLValue vector = args[0].request();
		if (!(vector instanceof DMLVector))
		    return error("argument not DMLVector",val);
		return ((DMLVector) vector).app(fun);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a vector -> unit </code>*/
    final public static App app = new App();

    final private static class Foldli extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,5,"Vector.foldli");
	    return new Foldli1(args[0].request());
	}
	final private static class Foldli1 extends DMLBuiltin {
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.foldli1");
		return new Foldli2(fun, args[0].request());
	    }
	    final private static class Foldli2 extends DMLBuiltin {
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		final public DMLValue apply(DMLValue val) {
		    DMLValue[] args=fromTuple(val,3,"Vector.foldli2");
		    DMLValue vector = args[0].request();
		    if (!(vector instanceof DMLVector))
			return error("argument #1 not DMLVector",val);
		    DMLValue from = args[1].request();
		    if (!(from instanceof DMLInt))
			return error("argument #2 not DMLInt",val);
		    DMLValue to = args[2].request();
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((DMLVector) vector).vector.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof DMLInt))
				return error("argument #3 not DMLInt option",val);
			    toint=((DMLInt) iv).getInt();
			}
		    } else
			return error("argument #3 not DMLInt option",val);
		    return ((DMLVector) vector).
			foldli(fun,
			       init,
			       ((DMLInt) from).getInt(),
			       toint);
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b </code>*/
    final public static Foldli foldli = new Foldli();

    final private static class Foldri extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,5,"Vector.foldri");
	    return new Foldri1(args[0].request());
	}
	final private static class Foldri1 extends DMLBuiltin {
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.foldri1");
		return new Foldri2(fun, args[0].request());
	    }
	    final private static class Foldri2 extends DMLBuiltin {
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) {
		    DMLValue[] args=fromTuple(val,3,"Vector.foldri2");
		    DMLValue vector = args[0].request();
		    if (!(vector instanceof DMLVector))
			return error("argument #1 not DMLVector",val);
		    DMLValue from = args[1].request();
		    if (!(from instanceof DMLInt))
			return error("argument #2 not DMLInt",val);
		    DMLValue to = args[2].request();
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((DMLVector) vector).vector.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof DMLInt))
				return error("argument #3 not DMLInt option",val);
			    toint=((DMLInt) iv).getInt();
			}
		    } else
			return error("argument #3 not DMLInt option",val);
		    return ((DMLVector) vector).
			foldri(fun,
			       init,
			       ((DMLInt) from).getInt(),
			       toint);
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b </code>*/
    final public static Foldri foldri = new Foldri();

    final private static class Foldl extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.foldl");
	    return new Foldl1(args[0].request());
	}
	final private static class Foldl1 extends DMLBuiltin {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.foldl1");
		return new Foldl2(fun, args[0]);
	    }
	    final private static class Foldl2 extends DMLBuiltin {
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) {
		    DMLValue[] args=fromTuple(val,1,"Vector.foldl2");
		    DMLValue vector = args[0].request();
		    if (!(vector instanceof DMLVector))
			return error("argument not DMLVector",val);
		    return ((DMLVector) vector).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b </code>*/
    final public static Foldl foldl = new Foldl();

    final private static class Foldr extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Vector.foldr");
	    return new Foldr1(args[0].request());
	}
	final private static class Foldr1 extends DMLBuiltin {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    final public DMLValue apply(DMLValue val) {
		DMLValue[] args=fromTuple(val,1,"Vector.foldr1");
		return new Foldr2(fun, args[0]);
	    }
	    final private static class Foldr2 extends DMLBuiltin {
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		final public DMLValue apply(DMLValue val) {
		    DMLValue[] args=fromTuple(val,1,"Vector.foldr2");
		    DMLValue vector = args[0].request();
		    if (!(vector instanceof DMLVector))
			return error("argument not DMLVector",val);
 		    return ((DMLVector) vector).foldr(fun,init);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b</code>*/
    final public static Foldr foldr = new Foldr();

    // Hilfsfunktionen
    final protected static DMLValue[] fromTuple
	(DMLValue v, /** <code>value-Tuple</code>*/
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) {
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
	(java.lang.String msg, DMLValue v) {
	// sonst: Fehler
	DMLValue[] err = {
	    new DMLString(msg),
	    v};
	return DMLConstants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
