/*
 * Author:
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 *
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 *
 */
package de.uni_sb.ps.dml.runtime;

import java.rmi.RemoteException;

final public class Array implements DMLValue {

    private NoGood ng = null;
    /** nachschauen wieviel in Java machbar ist*/
    public final static int maxLength = 65535;

    /** das Array mit den Werten */
    protected DMLValue[] arr = null;

    /** diesen da für <code>fromList</code>*/
    protected Array(DMLValue list) throws RemoteException {
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
		    _RAISENAME(General.Match);
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
	    _RAISENAME(General.Match);
	}
    }

    /** das ist tabulate */
    public Array(DMLValue f, int n)  throws RemoteException {
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

    final public DMLValue app(DMLValue f) throws RemoteException {
	int length = arr.length;
	for(int i=0; i<length; i++)
	    f.apply(arr[i]);
	return Constants.dmlunit;
    }

    final public DMLValue appi(int from, int to, DMLValue f)  throws RemoteException {
	if (to<0 || from<0 || arr.length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    for(int i=from; i<to; i++) {
		f.apply2(new Int(i),arr[i]);
	    }
	    return Constants.dmlunit;
	}
    }

    final public DMLValue foldl(DMLValue f, DMLValue init)  throws RemoteException {
	int length=arr.length;
	DMLValue buff = init;
	for(int i=0; i<length; i++) {
	    buff=f.apply2(arr[i],buff);
	}
	return buff;
    }

    final public DMLValue foldr(DMLValue f, DMLValue init) throws RemoteException {
	DMLValue buff = init;
	for(int i=arr.length-1; i>=0; i--) {
	    buff=f.apply2(arr[i],buff);
	}
	return buff;
    }

    final public DMLValue foldli(DMLValue f, DMLValue init, int from, int to) throws RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    DMLValue buff=init;
	    for(int i=from; i<to; i++) {
		buff=f.apply3(new Int(i),arr[i],buff);
	    }
	    return buff;
	}
    }

    final public DMLValue foldri(DMLValue f, DMLValue init, int from, int to) throws RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    DMLValue buff = init;
	    for(int i=to-1; i>=from; i--) {
		buff=f.apply3(new Int(i), arr[i], buff);
	    }
	    return buff;
	}
    }

    final public DMLValue modify(DMLValue f) throws RemoteException {
	int length=arr.length;
	for(int i=0; i<length; i++)
	    arr[i]=f.apply(arr[i]);
	return Constants.dmlunit;
    }

    final public DMLValue modifyi(DMLValue f, int from, int to) throws RemoteException {
	int length = arr.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    for(int i=from; i<to; i++) {
		arr[i]=f.apply2(new Int(i), arr[i]);
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

    final public DMLValue tabulate(int n, DMLValue f) throws RemoteException {
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
	if (ng == null) { // falls zum ersten Mal serialisiert
	    GName gn = new GName();
	    ng = new NoGood(gn);
	    GName.gNames.put(gn, this);
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
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.array");
	}

	_SAPPLY2(v) {
	    _REQUEST(v1,v1);
	    try {
		int ar = ((Int) v1).value;
		return new Array(ar,v2);
	    } catch (ClassCastException x) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val array : (int * 'a) -> 'a array </code>*/
    final public static ArraY array = new ArraY();
    static {
	Builtin.builtins.put("Array.array",array);
    }

    _BUILTIN(FromList) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromSingle(val,"Array.fromList");
	    return new Array(val);
	}
    }
    /** <code>val fromList : 'a list -> 'a array </code>*/
    _FIELD(Array,fromList);

    _BUILTIN(Tabulate) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.tabulate");
	}
	_SAPPLY2(v) {
	    try {
		_REQUEST(v1,v1);
		int ar = ((Int) v1).value;
		return new Array(v2,ar);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a array </code>*/
    _FIELD(Array,tabulate);

    _BUILTIN(Length) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.length");
	    try {
		return new Int(((Array) val).arr.length);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val length : 'a array -> int </code>*/
    _FIELD(Array,length);

    _BUILTIN(Sub) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Array.sub");
	}
	_SAPPLY2(v) {
	    _REQUEST(v1,v1);
	    _REQUEST(v2,v2);
	    try {
		return ((Array) v1).sub(((Int) v2).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val sub : ('a array * int) -> 'a </code>*/
    _FIELD(Array,sub);

    _BUILTIN(Update) {
	_NOAPPLY0;_NOAPPLY2;_APPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.update");
	}
	_SAPPLY3(v) {
	    _REQUESTDEC(DMLValue array,v1);
	    _REQUESTDEC(DMLValue idx,v2);
	    try {
		return ((Array) array).update(((Int) idx).value,v3);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val update : ('a array * int * 'a) -> unit </code>*/
    _FIELD(Array,update);

    _BUILTIN(Extract) {
	_NOAPPLY0;_NOAPPLY2;_APPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,3,"Array.extract");
	}
	_SAPPLY3(v) {
	    try {
		_REQUESTDEC(DMLValue array,v1);
		_REQUESTDEC(DMLValue fr,v2);
		_REQUESTDEC(DMLValue to,v3);
		if (to==Option.NONE) {
		    Array a=(Array) array;
		    return a.extract(((Int) fr).value,
				     a.arr.length);
		} else if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (cv.getConstructor() == Option.SOME) {
			to=cv.getContent();
			if (to instanceof Int)
			    return ((Array) array).
				extract(((Int) fr).value,
					((Int) to).value);
		    }
		}
		_RAISENAME(General.Match);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val extract : ('a array * int * int option) -> 'a vector </code>*/
    _FIELD(Array,extract);

    _BUILTIN(Copy) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copy");
	    _REQUESTDEC(DMLValue array,args[0]);
	    if (array instanceof Array) {
		_REQUESTDEC(DMLValue fr,args[1]);
		if (!(fr instanceof Int)) {
		    _RAISENAME(General.Match);
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
		    _RAISENAME(General.Match);
		}
		_REQUESTDEC(DMLValue dest,args[3]);
		if (!(dest instanceof Array)) {
		    _RAISENAME(General.Match);
		}
		_REQUESTDEC(DMLValue di,args[4]);
		if (!(di instanceof Int)) {
		    _RAISENAME(General.Match);
		}
		return ((Array) array)
		    .copy(from,
			  le,
			  (Array) dest,
			  ((Int) di).value);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val copy : {src : 'a array, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(Array,copy);

    _BUILTIN(CopyVec) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,5,"Array.copyVec");
	    _REQUESTDEC(DMLValue vector,args[0]);
	    //      System.err.println("vector: "+vector.getClass());
	    if (vector instanceof Vector) {
		_REQUESTDEC(DMLValue fr,args[1]);
		if (!(fr instanceof Int)) {
		    _RAISENAME(General.Match);
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
		    _RAISENAME(General.Match);
		}
		_REQUESTDEC(DMLValue dest,args[3]);
		if (!(dest instanceof Array)) {
		    _RAISENAME(General.Match);
		}
		_REQUESTDEC(DMLValue di,args[4]);
		if (!(di instanceof Int)) {
		    _RAISENAME(General.Match);
		}
		return ((Vector) vector).copyVec(from,
						 le,
						 (Array) dest,
						 ((Int) di).value);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val copyVec : {src : 'a vector, si : int, len : int option, dst : 'a array, di : int} -> unit </code>*/
    _FIELD(Array,copyVec);

    _BUILTIN(Appi) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.appi");
	    return new Appi1(val);
	}
	_BUILTIN(Appi1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
	    public DMLValue fun = null;
	    public Appi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.appi1");
	    }
	    _VAPPLY3(v) {
		try {
		    _REQUESTDEC(DMLValue array,v1);
		    _REQUESTDEC(DMLValue fr,v2);
		    int from = ((Int) fr).value;
		    _REQUESTDEC(DMLValue to,v3);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int)) {
				_RAISENAME(General.Match);
			    }
			    toint=((Int) iv).value;
			}
		    } else {
			_RAISENAME(General.Match);
		    }
		    return ((Array) array).
			appi(from,
			     toint,
			     fun);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(Array,appi);

    _BUILTIN(App) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.app");
	    return new App1(val);
	}
	_BUILTIN(App1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.app1");
		if (!(val instanceof Array)) {
		    _RAISENAME(General.Match);
		}
		return ((Array) val).app(fun);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a array -> unit </code>*/
    _FIELD(Array,app);

    _BUILTIN(Foldli) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldli");
	    return new Foldli1(val);
	}
	_BUILTIN(Foldli1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldli1");
		return new Foldli2(fun,val);
	    }
	    _BUILTIN(Foldli2) {
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldli2");
		}
		_VAPPLY3(v) {
		    try {
			_REQUESTDEC(DMLValue array,v1);
			_REQUESTDEC(DMLValue from,v2);
			_REQUESTDEC(DMLValue to,v3);
			int toint = 0;
			if (to==Option.NONE)
			    toint = ((Array) array).arr.length;
			else if (to instanceof DMLConVal) {
			    DMLConVal cv = (DMLConVal) to;
			    if (!(cv.getConstructor()==Option.SOME)) {
				DMLValue iv= cv.getContent();
				if (!(iv instanceof Int)) {
				    _RAISENAME(General.Match);
				}
				toint=((Int) iv).value;
			    }
			} else {
			    _RAISENAME(General.Match);
			}
			return ((Array) array).
			    foldli(fun,
				   init,
				   ((Int) from).value,
				   toint);
		    } catch (ClassCastException c) {
			_RAISENAME(General.Match);
		    }
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(Array,foldli);

    _BUILTIN(Foldri) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldri");
	    return new Foldri1(val);
	}
	_BUILTIN(Foldri1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldri1");
		return new Foldri2(fun,val);
	    }
	    _BUILTIN(Foldri2) {
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    _fromTuple(args,val,3,"Array.foldri2");
		}
		_VAPPLY3(v) {
		    try {
			_REQUESTDEC(DMLValue array,v1);
			_REQUESTDEC(DMLValue from,v2);
			_REQUESTDEC(DMLValue to,v3);
			int toint = 0;
			if (to==Option.NONE)
			    toint = ((Array) array).arr.length;
			else if (to instanceof DMLConVal) {
			    DMLConVal cv = (DMLConVal) to;
			    if (!(cv.getConstructor()==Option.SOME)) {
				DMLValue iv= cv.getContent();
				if (!(iv instanceof Int)) {
				    _RAISENAME(General.Match);
				}
				toint=((Int) iv).value;
			    }
			} else {
			    _RAISENAME(General.Match);
			}
			return ((Array) array).
			    foldri(fun,
				   init,
				   ((Int) from).value,
				   toint);
		    } catch (ClassCastException c) {
			_RAISENAME(General.Match);
		    }
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b </code>*/
    _FIELD(Array,foldri);

    _BUILTIN(Foldl) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldl");
	    return new Foldl1(val);
	}
	_BUILTIN(Foldl1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldl1");
		return new Foldl2(fun,val);
	    }
	    _BUILTIN(Foldl2) {
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    // _FROMSINGLE(val,"Array.foldl2");
		    if (!(val instanceof Array)) {
			_RAISENAME(General.Match);
		    }
		    return ((Array) val).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(Array,foldl);

    _BUILTIN(Foldr) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.foldr");
	    return new Foldr1(val);
	}
	_BUILTIN(Foldr1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.foldr1");
		return new Foldr2(fun,val);
	    }
	    _BUILTIN(Foldr2) {
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_APPLY(val) {
		    // _FROMSINGLE(val,"Array.foldr2");
		    if (!(val instanceof Array)) {
			_RAISENAME(General.Match);
		    }
		    return ((Array) val).foldr(fun,init);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b </code>*/
    _FIELD(Array,foldr);

    _BUILTIN(Modifyi) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.modifyi");
	    return new Modifyi1(val);
	}
	_BUILTIN(Modifyi1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
	    DMLValue fun = null;
	    Modifyi1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,3,"Array.modifyi1");
	    }
	    _VAPPLY3(v) {
		try {
		    _REQUESTDEC(DMLValue array,v1);
		    _REQUESTDEC(DMLValue from,v2);
		    _REQUESTDEC(DMLValue to,v3);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Array) array).arr.length;
		    else if (to instanceof DMLConVal) {
			DMLConVal cv = (DMLConVal) to;
			if (!(cv.getConstructor()==Option.SOME)) {
			    DMLValue iv= cv.getContent();
			    if (!(iv instanceof Int)) {
				_RAISENAME(General.Match);
			    }
			    toint=((Int) iv).value;
			}
		    } else {
			_RAISENAME(General.Match);
		    }
		    return ((Array) array).
			modifyi(fun,
				((Int) from).value,
				toint);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit </code>*/
    _FIELD(Array,modifyi);

    _BUILTIN(Modify) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Array.modify");
	    return new Modify1(val);
	}
	_BUILTIN(Modify1) {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    DMLValue fun = null;
	    Modify1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"Array.modify1");
		if (!(val instanceof Array)) {
		    _RAISENAME(General.Match);
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
