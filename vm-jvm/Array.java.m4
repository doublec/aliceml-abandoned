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
		} else {
		    _error("argument not DMLList",list);
		}
	    }
	    arr = new DMLValue[le];
	    int index = 0;
	    while (list!=List.nil) {
		Cons l = (Cons) list;
		arr[index++]=l.car;
		list = l.cdr;
	    }
	} else {
	    _error("argument not DMLList",list);
	}
    }

    /** das ist tabulate */
    public Array(DMLValue f, int n)  throws java.rmi.RemoteException {
	if (n<0 || maxLength<n) {
	    _RAISENAME(General.Size);
	} else {
	    arr = new DMLValue[n];
	    for(int i=0; i<n; i++) {
		arr[i]=f.apply(new Int(i));
	    }
	}
    }
    /** entspricht <code>array</code>*/
    public Array(int len, DMLValue init) {
	if (len<0 || maxLength<len) {
	    _RAISENAME(General.Size);
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
	    _RAISENAME(General.Subscript);
	} else {
	    return arr[index];
	}
    }

    final public DMLValue update(int index, DMLValue x) {
	if (index<0 || arr.length<=index) {
	    _RAISENAME(General.Subscript);
	} else {
	    arr[index] = x;
	    return Constants.dmlunit;
	}
    }

    final public DMLValue app(DMLValue f) throws java.rmi.RemoteException {
	int length = arr.length;
	for(int i=0; i<length; i++)
	    f.apply(arr[i]);
	return Constants.dmlunit;
    }

    final public DMLValue appi(int from, int to, DMLValue f)  throws java.rmi.RemoteException {
	if (to<0 || from<0 || arr.length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
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
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    DMLValue buff=init;
	    for(int i=from; i<to; i++) {
		buff=f.apply(new Tuple3(new Int(i),arr[i],buff));
	    }
	    return buff;
	}
    }

    final public DMLValue foldri(DMLValue f, DMLValue init, int from, int to) throws java.rmi.RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
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
	    arr[i]=f.apply(arr[i]);
	return Constants.dmlunit;
    }

    final public DMLValue modifyi(DMLValue f, int from, int to) throws java.rmi.RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
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
		_RAISENAME(General.Subscript);
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
	if (to<0 || from<0 || to<from || arr.length<to) {
	    _RAISENAME(General.Subscript);
	} else {
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
	    _REQUESTDEC(DMLValue arg,args[0]);
	    int ar=0;
	    if (arg instanceof Int) {
		ar = ((Int) arg).value;
	    } else {
		_error("argument 1 not Int",val);
	    }
	    return new Array(ar,args[1]);
	}
    }
    /** <code>val array : (int * 'a) -> 'a array </code>*/
    final public static ArraY array = new ArraY();

    _BUILTIN(FromList) {
	_APPLY(val) {
	    _fromSingle(val,"Array.fromList");
	    return new Array(val);
	}
    }
    /** <code>val fromList : 'a list -> 'a array </code>*/
    _FIELD(Array,fromList);

    _BUILTIN(Tabulate) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.tabulate");
	    int ar=0;
	    _REQUESTDEC(DMLValue arg,args[0]);
	    if (arg instanceof Int) {
		ar = ((Int) arg).value;
	    } else {
		_error("argument 1 not Array",val);
	    }
	    return new Array(args[1],ar);
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a array </code>*/
    _FIELD(Array,tabulate);

    _BUILTIN(Length) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.length");
	    if (val instanceof Array) {
		return new Int(((Array) val).arr.length);
	    } else {
		_error("argument 1 not Int",val);
	    }
	}
    }
    /** <code>val length : 'a array -> int </code>*/
    _FIELD(Array,length);

    _BUILTIN(Sub) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.sub");
	    _REQUESTDEC(DMLValue array,args[0]);
	    if (array instanceof Array) {
		_REQUESTDEC(DMLValue idx,args[1]);
		if (idx instanceof Int) {
		    return ((Array) array).sub(((Int) idx).value);
		} else {
		    _error("argument 2 not Int",val);
		}
	    } else {
		_error("argument 1 not Array",val);
	    }
	}
    }
    /** <code>val sub : ('a array * int) -> 'a </code>*/
    _FIELD(Array,sub);

    _BUILTIN(Update) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.update");
	    _REQUESTDEC(DMLValue array,args[0]);
	    if (array instanceof Array) {
		_REQUESTDEC(DMLValue idx,args[1]);
		if (idx instanceof Int) {
		    return ((Array) array).
			update(((Int) idx).value,args[2]);
		} else {
		    _error("argument 2 not Int",val);
		}
	    } else {
		_error("argument 1 not Array",val);
	    }
	}
    }
    /** <code>val update : ('a array * int * 'a) -> unit </code>*/
    _FIELD(Array,update);

    _BUILTIN(Extract) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.extract");
	    _REQUESTDEC(DMLValue array,args[0]);
	    if (array instanceof Array) {
		_REQUESTDEC(DMLValue fr,args[1]);
		if (fr instanceof Int) {
		    _REQUESTDEC(DMLValue to,args[2]);
		    if (to==Option.NONE) {
			Array a=(Array) array;
			return a.extract(((Int) fr).value,
					 a.arr.length);
		    }
		    if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (cv.getConstructor()==Option.SOME) {
			    to=cv.getContent();
			    if (to instanceof Int)
				return ((Array) array).
				    extract(((Int) fr).value,
					    ((Int) to).value);
			}
		    }
		    _error("argument 3 not Int option",val);
		} else {
		    _error("argument 2 not Int",val);
		}
	    } else {
		_error("argument 1 not Array",val);
	    }
	}
    }
    /** <code>val extract : ('a array * int * int option) -> 'a vector </code>*/
    _FIELD(Array,extract);

    _BUILTIN(Copy) {
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copy");
	    _REQUESTDEC(DMLValue array,args[0]);
	    if (array instanceof Array) {
		_REQUESTDEC(DMLValue fr,args[1]);
		if (!(fr instanceof Int)) {
		    _error("argument 2 not Int",val);
		}
		int from = ((Int) fr).value;
		_REQUESTDEC(DMLValue len,args[2]);
		int le = 0;
		if (len==Option.NONE) {
		    le = ((Array) array).arr.length - from;
		} else if (len instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) len;
		    DMLValue c = cv.getConstructor();
		    if (c==Option.SOME) {
			len=cv.getContent();
			if (len instanceof Int)
			    le = ((Int) len).value;
		    }
		} else {
		    _error("argument 3 not Int option",val);
		}
		_REQUESTDEC(DMLValue dest,args[3]);
		if (!(dest instanceof Array)) {
		    _error("argument 4 not Array",val);
		}
		_REQUESTDEC(DMLValue di,args[4]);
		if (!(di instanceof Int)) {
		    _error("argument 5 not Int",val);
		}
		return ((Array) array)
		    .copy(from,
			  le,
			  (Array) dest,
			  ((Int) di).value);
	    } else {
		_error("argument 1 not Array",val);
	    }
	}
    }
    /** <code>val copy : {src : 'a array, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(Array,copy);

    _BUILTIN(CopyVec) {
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copyVec");
	    _REQUESTDEC(DMLValue vector,args[0]);
	    //	    System.err.println("vector: "+vector.getClass());
	    if (vector instanceof Vector) {
		_REQUESTDEC(DMLValue fr,args[1]);
		if (!(fr instanceof Int)) {
		    _error("argument 2 not Int",val);
		}
		int from = ((Int) fr).value;
		_REQUESTDEC(DMLValue len,args[2]);
		int le = 0;
		if (len==Option.NONE) {
		    le = ((Vector) vector).vec.length - from;
		} else if (len instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) len;
		    DMLValue c = cv.getConstructor();
		    if (c==Option.SOME) {
			len=cv.getContent();
			if (len instanceof Int)
			    le = ((Int) len).value;
		    }
		} else {
		    _error("argument 3 not Int option",val);
		}
		_REQUESTDEC(DMLValue dest,args[3]);
		if (!(dest instanceof Array)) {
		    _error("argument 4 not Array",val);
		}
		_REQUESTDEC(DMLValue di,args[4]);
		if (!(di instanceof Int)) {
		    _error("argument 5 not Int",val);
		}
		return ((Vector) vector).copyVec(from,
						 le,
						 (Array) dest,
						 ((Int) di).value);
	    } else {
		_error("argument 1 not Vector",val);
	    }
	}
    }
    /** <code>val copyVec : {src : 'a vector, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(Array,copyVec);

    _BUILTIN(Appi) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.appi");
	    return new Appi1(val);
	}
	_BUILTIN(Appi1) {
	    public DMLValue fun = null;
	    public Appi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.appi1");
		_REQUESTDEC(DMLValue array,args[0]);
		if (!(array instanceof Array)) {
		    _error("argument 1 not Array",val);
		}
		_REQUESTDEC(DMLValue fr,args[1]);
		if (!(fr instanceof Int)) {
		    _error("argument 2 not Int",val);
		}
		int from = ((Int) fr).value;
		_REQUESTDEC(DMLValue to,args[2]);
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Array) array).arr.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int)) {
			    _error("argument 3 not Int option",val);
			}
			toint=((Int) iv).value;
		    }
		} else {
		    _error("argument 3 not Int option",val);
		}
		return ((Array) array).
		    appi(from,
			 toint,
			 fun);
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(Array,appi);

    _BUILTIN(App) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.app");
	    return new App1(val);
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.app1");
		if (!(val instanceof Array)) {
		    _error("argument not Array",val);
		}
		return ((Array) val).app(fun);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a array -> unit </code>*/
    _FIELD(Array,app);

    _BUILTIN(Foldli) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldli");
	    return new Foldli1(val);
	}
	_BUILTIN(Foldli1) {
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldli1");
		return new Foldli2(fun,val);
	    }
	    _BUILTIN(Foldli2) {
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldli2");
		    _REQUESTDEC(DMLValue array,args[0]);
		    if (!(array instanceof Array)) {
			_error("argument 1 not Array",val);
		    }
		    _REQUESTDEC(DMLValue from,args[1]);
		    if (!(from instanceof Int)) {
			_error("argument 2 not Int",val);
		    }
		    _REQUESTDEC(DMLValue to,args[2]);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int)) {
				_error("argument 3 not Int option",val);
			    }
			    toint=((Int) iv).value;
			}
		    } else {
			_error("argument 3 not Int option",val);
		    }
		    return ((Array) array).
			foldli(fun,
			       init,
			       ((Int) from).value,
			       toint);
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(Array,foldli);

    _BUILTIN(Foldri) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldri");
	    return new Foldri1(val);
	}
	_BUILTIN(Foldri1) {
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldri1");
		return new Foldri2(fun,val);
	    }
	    _BUILTIN(Foldri2) {
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldri2");
		    _REQUESTDEC(DMLValue array,args[0]);
		    if (!(array instanceof Array)) {
			_error("argument 1 not Array",val);
		    }
		    _REQUESTDEC(DMLValue from,args[1]);
		    if (!(from instanceof Int)) {
			_error("argument 2 not Int",val);
		    }
		    _REQUESTDEC(DMLValue to,args[2]);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int)) {
				_error("argument 3 not Int option",val);
			    }
			    toint=((Int) iv).value;
			}
		    } else {
			_error("argument 3 not Int option",val);
		    }
		    return ((Array) array).
			foldri(fun,
			       init,
			       ((Int) from).value,
			       toint);
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(Array,foldri);

    _BUILTIN(Foldl) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldl");
	    return new Foldl1(val);
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldl1");
		return new Foldl2(fun,val);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    // _FROMSINGLE(val,"Array.foldl2");
		    if (!(val instanceof Array)) {
			_error("argument not Array",val);
		    }
		    return ((Array) val).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(Array,foldl);

    _BUILTIN(Foldr) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldr");
	    return new Foldr1(val);
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldr1");
		return new Foldr2(fun,val);
	    }
	    _BUILTIN(Foldr2) {
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    // _FROMSINGLE(val,"Array.foldr2");
		    if (!(val instanceof Array)) {
			_error("argument not Array",val);
		    }
 		    return ((Array) val).foldr(fun,init);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(Array,foldr);

    _BUILTIN(Modifyi) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.modifyi");
	    return new Modifyi1(val);
	}
	_BUILTIN(Modifyi1) {
	    DMLValue fun = null;
	    Modifyi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.modifyi1");
		_REQUESTDEC(DMLValue array,args[0]);
		if (!(array instanceof Array)) {
		    _error("argument 1 not Array",val);
		}
		_REQUESTDEC(DMLValue from,args[1]);
		if (!(from instanceof Int)) {
		    _error("argument 2 not Int",val);
		}
		_REQUESTDEC(DMLValue to,args[2]);
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Array) array).arr.length;
		else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (!(cv.getConstructor()==Option.SOME)) {
			DMLValue iv= cv.getContent();
			if (!(iv instanceof Int)) {
			    _error("argument 2 not Int option",val);
			}
			toint=((Int) iv).value;
		    }
		} else {
		    _error("argument 2 not Int option",val);
		}
		return ((Array) array).
		    modifyi(fun,
			    ((Int) from).value,
			    toint);
	    }
	}
    }
    /** <code>val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(Array,modifyi);

    _BUILTIN(Modify) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.modify");
	    return new Modify1(val);
	}
	_BUILTIN(Modify1) {
	    DMLValue fun = null;
	    Modify1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.modify1");
		if (!(val instanceof Array)) {
		    _error("argument not Array",val);
		}
		else {
		    return ((Array) val).modify(fun);
		}
	    }
	}
    }
    /** <code>val modify : ('a -> 'a) -> 'a array -> unit</code>*/
    _FIELD(Array,modify);
}
