/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class Array implements DMLValue {

    private NoGood ng = null;
    /** nachschauen wieviel in Java machbar ist*/
    public final static int maxLength = 65535;

    /** das Array mit den Werten */
    protected DMLValue[] arr = null;

    /** diesen da für <code>fromList</code>*/
    protected Array(DMLValue list) throws java.rmi.RemoteException {
	DMLValue li = list;
	if (list==List.nil)
	    arr = new DMLValue[0];
	else if (list instanceof Cons) {
	    int le = 0;
	    while (li!=List.nil) {
		if (li instanceof Cons) {
		    le++;
		    li = ((Cons) li).cdr;
		} else
		    _error("argument not DMLList",list);
	    }
	    arr = new DMLValue[le];
	    int index = 0;
	    while (list!=List.nil) {
		Cons l = (Cons) list;
		arr[index++]=l.car;
		list = l.cdr;
	    }
	} else
	    _error("argument not DMLList",list);
    }

    /** das ist tabulate */
    public Array(DMLValue f, int n)  throws java.rmi.RemoteException {
	if (n<0 || maxLength<n) {
	    General.Size.raise();
	} else {
	    arr = new DMLValue[n];
	    for(int i=0; i<n; i++) {
		arr[i]=f.apply(new Tuple1(new Int(i)));
	    }
	}
    }
    /** entspricht <code>array</code>*/
    public Array(int len, DMLValue init) {
	if (len<0 || maxLength<len) {
	    General.Size.raise();
	} else {
	    arr = new DMLValue[len];
	    for(int i=0; i<len; i++)
		arr[i] = init;
	}
    }

    public Array(DMLValue[] arr) {
	int size=arr.length;
	arr=new DMLValue[size];
	for(int i=0; i<size; i++)
	    arr[i]=arr[i];
    }

    final public int length() {
	return arr.length;
    }

    final public DMLValue sub(int index) {
	if (index<0 || arr.length<=index) {
	    return General.Subscript.raise();
	} else {
	    return arr[index];
	}
    }

    final public DMLValue update(int index, DMLValue x) {
	if (index<0 || arr.length<=index)
	    return General.Subscript.raise();
	else {
	    arr[index] = x;
	    return Constants.dmlunit;
	}
    }

    final public DMLValue app(DMLValue f) throws java.rmi.RemoteException {
	int length = arr.length;
	for(int i=0; i<length; i++)
	    f.apply(new Tuple1(arr[i]));
	return Constants.dmlunit;
    }

    final public DMLValue appi(int from, int to, DMLValue f)  throws java.rmi.RemoteException {
	if (to<0 || from<0 || arr.length<to || to<from)
	    return General.Subscript.raise();
	else {
	    for(int i=from; i<to; i++) {
		f.apply(new Tuple2(new Int(i),arr[i]));
	    }
	    return Constants.dmlunit;
	}
    }

    final public DMLValue foldl(DMLValue f, DMLValue init)  throws java.rmi.RemoteException {
	int length=arr.length;
	DMLValue buff = init;
	for(int i=0; i<length; i++) {
	    buff=f.apply(new Tuple2(arr[i],buff));
	}
	return buff;
    }

    final public DMLValue foldr(DMLValue f, DMLValue init) throws java.rmi.RemoteException {
	DMLValue buff = init;
	for(int i=arr.length-1; i>=0; i--) {
	    buff=f.apply(new Tuple2(arr[i],buff));
	}
	return buff;
    }

    final public DMLValue foldli(DMLValue f, DMLValue init, int from, int to) throws java.rmi.RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from)
	    return General.Subscript.raise();
	else {
	    DMLValue buff=init;
	    for(int i=from; i<to; i++) {
		buff=f.apply(new Tuple3(new Int(i),arr[i],buff));
	    }
	    return buff;
	}
    }

    final public DMLValue foldri(DMLValue f, DMLValue init, int from, int to) throws java.rmi.RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from)
	    return General.Subscript.raise();
	else {
	    DMLValue buff = init;
	    for(int i=to-1; i>=from; i--) {
		buff=f.apply(new Tuple3(new Int(i), arr[i], buff));
	    }
	    return buff;
	}
    }

    final public DMLValue modify(DMLValue f) throws java.rmi.RemoteException {
	int length=arr.length;
	for(int i=0; i<length; i++)
	    arr[i]=f.apply(new Tuple1(arr[i]));
	return Constants.dmlunit;
    }

    final public DMLValue modifyi(DMLValue f, int from, int to) throws java.rmi.RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from)
	    return General.Subscript.raise();
	else {
	    for(int i=from; i<to; i++) {
		arr[i]=f.apply(new Tuple2(new Int(i), arr[i]));
	    }
	    return Constants.dmlunit;
	}
    }

    final public DMLValue copy(int from, int len, Array dest, int dfrom) {
	DMLValue[] destArray = dest.arr;
	int dlength = destArray.length;
	if (from<0 || dfrom<0 ||
	    arr.length<from+len || dlength<dfrom+len)
	    {
		// System.err.println("from: "+from+" len: "+len+" dfrom: "+dfrom);
		return General.Subscript.raise();
	    }
	else {
	    // hier vielleicht System.arrayCopy ?
	    for(int i=0; i<len; i++)
		destArray[dfrom+i]=arr[from+i];
	    return Constants.dmlunit;
	}
    }

    final public DMLValue tabulate(int n, DMLValue f) throws java.rmi.RemoteException {
	return new Array(f,n);
    }

    final public DMLValue extract(int from, int to) {
	if (to<0 || from<0 || to<from || arr.length<to)
	    return General.Subscript.raise();
	else {
	    return new Vector(arr,from,to);
	}
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;

    final public java.lang.String toString() {
	java.lang.String s="["+arr[0];
	int l=arr.length;
	for(int i=1; i<l; i++)
	    s+=", "+arr[i];
	return s+"] : Array";
    }

    /** Arrays können dann nicht serialisiert werden.
     */
    private Object writeReplace()
	throws java.io.ObjectStreamException {
	if (ng==null) { // falls zum ersten Mal serialisiert
	    GName gn=new GName();
	    ng=new NoGood(gn);
	    GName.gNames.put(gn,ng);
	    return ng;
	} else {
	    return ng;
	}
    }

    /***********************************************************************/
    /* Part 2: the builtin functions                                       */
    /***********************************************************************/

    /** <code>val maxLen : int </code>*/
    final public static Int maxLen = new Int(Array.maxLength);

    _BUILTIN(ArraY) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.array");
	    DMLValue arg=args[0].request();
	    int ar=0;
	    if (arg instanceof Int)
		ar = ((Int) arg).getInt();
	    else
		_error("argument 1 not Int",val);
	    return new Array(ar,args[1]);
	}
    }
    /** <code>val array : (int * 'a) -> 'a array </code>*/
    final public static ArraY array = new ArraY();

    _BUILTIN(FromList) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.fromList");
	    DMLValue arg=args[0].request();
	    return new Array(arg);
	}
    }
    /** <code>val fromList : 'a list -> 'a array </code>*/
    _FIELD(fromList);

    _BUILTIN(Tabulate) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.tabulate");
	    int ar=0;
	    DMLValue arg=args[0].request();
	    if (arg instanceof Int)
		ar = ((Int) arg).getInt();
	    else
		_error("argument 1 not Array",val);
	    return new Array(args[1],ar);
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a array </code>*/
    _FIELD(tabulate);

    _BUILTIN(Length) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.length");
	    DMLValue arg=args[0].request();
	    if (arg instanceof Array)
		return new Int(((Array) arg).arr.length);
	    else
		return _error("argument 1 not Int",val);
	}
    }
    /** <code>val length : 'a array -> int </code>*/
    _FIELD(length);

    _BUILTIN(Sub) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.sub");
	    DMLValue array = args[0].request();
	    if (array instanceof Array) {
		DMLValue idx = args[1].request();
		if (idx instanceof Int)
		    return ((Array) array).sub(((Int) idx).getInt());
		else
		    return _error("argument 2 not Int",val);
	    }
	    else
		return _error("argument 1 not Array",val);
	}
    }
    /** <code>val sub : ('a array * int) -> 'a </code>*/
    _FIELD(sub);

    _BUILTIN(Update) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.update");
	    DMLValue array = args[0].request();
	    if (array instanceof Array) {
		DMLValue idx = args[1].request();
		if (idx instanceof Int)
		    return ((Array) array).
			update(((Int) idx).getInt(),args[2]);
		else
		    return _error("argument 2 not Int",val);
	    }
	    else
		return _error("argument 1 not Array",val);
	}
    }
    /** <code>val update : ('a array * int * 'a) -> unit </code>*/
    _FIELD(update);

    _BUILTIN(Extract) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.extract");
	    DMLValue array = args[0].request();
	    if (array instanceof Array) {
		DMLValue fr = args[1].request();
		if (fr instanceof Int) {
		    DMLValue to = args[2].request();
		    if (to==Option.NONE) {
			Array a=(Array) array;
			return a.extract(((Int) fr).getInt(),
					 a.arr.length);
		    }
		    if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (cv.getConstructor()==Option.SOME) {
			    to=cv.getContent();
			    if (to instanceof Int)
				return ((Array) array).
				    extract(((Int) fr).getInt(),
					    ((Int) to).getInt());
			}
		    }
		    return _error("argument 3 not Int option",val);
		}
		else
		    return _error("argument 2 not Int",val);
	    }
	    else
		return _error("argument 1 not Array",val);
	}
    }
    /** <code>val extract : ('a array * int * int option) -> 'a vector </code>*/
    _FIELD(extract);

    _BUILTIN(Copy) {
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copy");
	    DMLValue array = args[0].request();
	    if (array instanceof Array) {
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return _error("argument 2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue len = args[2].request();
		int le = 0;
		if (len==Option.NONE) {
		    le = ((Array) array).arr.length - from;
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
		    return _error("argument 3 not Int option",val);
		DMLValue dest = args[3].request();
		if (!(dest instanceof Array))
		    return _error("argument 4 not Array",val);
		DMLValue di = args[4].request();
		if (!(di instanceof Int))
		    return _error("argument 5 not Int",val);
		return ((Array) array)
		    .copy(from,
			  le,
			  (Array) dest,
			  ((Int) di).getInt());
	    } else
		return _error("argument 1 not Array",val);
	}
    }
    /** <code>val copy : {src : 'a array, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(copy);

    _BUILTIN(CopyVec) {
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copyVec");
	    DMLValue vector = args[0].request();
	    //	    System.err.println("vector: "+vector.getClass());
	    if (vector instanceof Vector) {
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return _error("argument 2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue len = args[2].request();
		int le = 0;
		if (len==Option.NONE) {
		    le = ((Vector) vector).vec.length - from;
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
		    return _error("argument 3 not Int option",val);
		DMLValue dest = args[3].request();
		if (!(dest instanceof Array))
		    return _error("argument 4 not Array",val);
		DMLValue di = args[4].request();
		if (!(di instanceof Int))
		    return _error("argument 5 not Int",val);
		return ((Vector) vector).copyVec(from,
						 le,
						 (Array) dest,
						 ((Int) di).getInt());
	    } else
		return _error("argument 1 not Vector",val);
	}
    }
    /** <code>val copyVec : {src : 'a vector, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(copyVec);

    _BUILTIN(Appi) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.appi");
	    return new Appi1(args[0].request());
	}
	_BUILTIN(Appi1) {
	    public DMLValue fun = null;
	    public Appi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.appi1");
		DMLValue array = args[0].request();
		if (!(array instanceof Array))
		    return _error("argument 1 not Array",val);
		DMLValue fr = args[1].request();
		if (!(fr instanceof Int))
		    return _error("argument 2 not Int",val);
		int from = ((Int) fr).getInt();
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Array) array).arr.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return _error("argument 3 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return _error("argument 3 not Int option",val);
		return ((Array) array).
		    appi(from,
			 toint,
			 fun);
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(appi);

    _BUILTIN(App) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.app");
	    return new App1(args[0].request());
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.app1");
		DMLValue array = args[0].request();
		if (!(array instanceof Array))
		    return _error("argument not Array",val);
		return ((Array) array).app(fun);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a array -> unit </code>*/
    _FIELD(app);

    _BUILTIN(Foldli) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.foldli");
	    return new Foldli1(args[0].request());
	}
	_BUILTIN(Foldli1) {
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.foldli1");
		return new Foldli2(fun, args[0].request());
	    }
	    _BUILTIN(Foldli2) {
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldli2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof Array))
			return _error("argument 1 not Array",val);
		    DMLValue from = args[1].request();
		    if (!(from instanceof Int))
			return _error("argument 2 not Int",val);
		    DMLValue to = args[2].request();
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int))
				return _error("argument 3 not Int option",val);
			    toint=((Int) iv).getInt();
			}
		    } else
			return _error("argument 3 not Int option",val);
		    return ((Array) array).
			foldli(fun,
			       init,
			       ((Int) from).getInt(),
			       toint);
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(foldli);

    _BUILTIN(Foldri) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.foldri");
	    return new Foldri1(args[0].request());
	}
	_BUILTIN(Foldri1) {
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.foldri1");
		return new Foldri2(fun, args[0].request());
	    }
	    _BUILTIN(Foldri2) {
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldri2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof Array))
			return _error("argument 1 not Array",val);
		    DMLValue from = args[1].request();
		    if (!(from instanceof Int))
			return _error("argument 2 not Int",val);
		    DMLValue to = args[2].request();
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int))
				return _error("argument 3 not Int option",val);
			    toint=((Int) iv).getInt();
			}
		    } else
			return _error("argument 3 not Int option",val);
		    return ((Array) array).
			foldri(fun,
			       init,
			       ((Int) from).getInt(),
			       toint);
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(foldri);

    _BUILTIN(Foldl) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.foldl");
	    return new Foldl1(args[0].request());
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.foldl1");
		return new Foldl2(fun, args[0]);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    _fromTuple(args,val,1,"Array.foldl2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof Array))
			return _error("argument not Array",val);
		    return ((Array) array).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(foldl);

    _BUILTIN(Foldr) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.foldr");
	    return new Foldr1(args[0].request());
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.foldr1");
		return new Foldr2(fun, args[0]);
	    }
	    _BUILTIN(Foldr2) {
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    _fromTuple(args,val,1,"Array.foldr2");
		    DMLValue array = args[0].request();
		    if (!(array instanceof Array))
			return _error("argument not Array",val);
 		    return ((Array) array).foldr(fun,init);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(foldr);

    _BUILTIN(Modifyi) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.modifyi");
	    return new Modifyi1(args[0].request());
	}
	_BUILTIN(Modifyi1) {
	    DMLValue fun = null;
	    Modifyi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.modifyi1");
		DMLValue array = args[0].request();
		if (!(array instanceof Array))
		    return _error("argument 1 not Array",val);
		DMLValue from = args[1].request();
		if (!(from instanceof Int))
		    return _error("argument 2 not Int",val);
		DMLValue to = args[2].request();
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Array) array).arr.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int))
			    return _error("argument 2 not Int option",val);
			toint=((Int) iv).getInt();
		    }
		} else
		    return _error("argument 2 not Int option",val);
		return ((Array) array).
		    modifyi(fun,
			    ((Int) from).getInt(),
			    toint);
	    }
	}
    }
    /** <code>val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(modifyi);

    _BUILTIN(Modify) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Array.modify");
	    return new Modify1(args[0]);
	}
	_BUILTIN(Modify1) {
	    DMLValue fun = null;
	    Modify1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"Array.modify1");
		DMLValue array = args[0].request();
		if (!(array instanceof Array))
		    return _error("argument not Array",val);
		else
		    return ((Array) array).modify(fun);
	    }
	}
    }
    /** <code>val modify : ('a -> 'a) -> 'a array -> unit</code>*/
    _FIELD(modify);
}
