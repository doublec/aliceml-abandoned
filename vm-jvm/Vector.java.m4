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

/** This class represents the builtin type <code>vector</code>.
 *  @see Array
 */
final public class Vector implements DMLValue {

    private NoGood ng = null;

    /** the maximum length of a vector */
    public final static int maxLength = 65535;

    /** the java array that contains the vector entries */
    public DMLValue[] vec = null;

    protected Vector(int n) {
	if (n<0 || maxLength<n) {
	    _RAISENAME(General.Size);
	} else {
	    vec = new DMLValue[n];
	}
    }

    /** The constructor invoked by <code>tabulate</code>.
     *  @param f <code>val f : int -> 'a</code>
     *  @param n integer, to which <code>tabulate</code> is called
     */
    public Vector(DMLValue f, int n)
	throws RemoteException {// das ist tabulate
	if (n<0 || maxLength<n) {
	    _RAISENAME(General.Size);
	} else {
	    vec = new DMLValue[n];
	    for(int i=0; i<n; i++) {
		vec[i] = f.apply(new Int(i));
	    }
	}
    }

    // who needs this one? this is remainder of Array
    public Vector(int len, DMLValue init) {
	if (len<0 || maxLength<len) {
	    _RAISENAME(General.Size);
	} else {
	    vec = new DMLValue[len];
	    for(int i=0; i<len; i++)
		vec[i] = init;
	}
    }

    public Vector(DMLValue[] arr) {
	int size=arr.length;
	vec=new DMLValue[size];
	for(int i=0; i<size; i++)
	    vec[i]=arr[i];
    }

    public Vector(DMLValue[] arr, int from, int to) {
	int size=arr.length;
	if (to<0 || from<0 || to<from || arr.length<to) {
	    _RAISENAME(General.Subscript);
	} else {
	    size=to-from;
	    vec = new DMLValue[size];
	    for(int i=0; i<size; i++)
		vec[i]=arr[from+i];
	}
    }

    protected Vector(DMLValue list)  throws RemoteException {
	DMLValue li = list;
	if (list == List.nil)
	    vec = new DMLValue[0];
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
	    vec = new DMLValue[le];
	    int index = 0;
	    while (list != List.nil) {
		Cons l = (Cons) list;
		vec[index++] = l.car;
		list = l.cdr;
	    }
	} else {
	    _RAISENAME(General.Match);
	}
    }

    final public int length() {
	return vec.length;
    }

    final public DMLValue sub(int index) {
	if (index<0 || vec.length<=index) {
	    _RAISENAME(General.Subscript);
	} else {
	    return vec[index];
	}
    }

    final public DMLValue app(DMLValue f)
	throws RemoteException {
	int length = vec.length;
	for(int i=0; i<length; i++)
	    f.apply(vec[i]);
	return Constants.dmlunit;
    }

    final public DMLValue appi(int from, int to, DMLValue f)
	throws RemoteException {
	if (to<0 || from<0 || vec.length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    for(int i=from; i<to; i++) {
		f.apply2(new Int(i),vec[i]);
	    }
	    return Constants.dmlunit;
	}
    }

    final public DMLValue foldl(DMLValue f, DMLValue init)
	throws RemoteException {
	DMLValue[] args = new DMLValue[2];
	int length=vec.length;
	args[1]=init;
	for(int i=0; i<length; i++) {
	    args[0]=vec[i];
	    args[1]=f.apply(new Tuple(args));
	}
	return args[1];
    }

    final public DMLValue foldr(DMLValue f, DMLValue init)
	throws RemoteException {
	DMLValue[] args = new DMLValue[2];
	args[1]=init;
	for(int i=vec.length-1; i>=0; i--) {
	    args[0]=vec[i];
	    args[1]=f.apply(new Tuple(args));
	}
	return args[1];
    }

    final public DMLValue foldli(DMLValue f, DMLValue init, int from, int to)
	throws RemoteException {
	int length = vec.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    DMLValue[] args = new DMLValue[3];
	    args[2]=init;
	    for(int i=from; i<to; i++) {
		args[0]=new Int(i);
		args[1]=vec[i];
		args[2]=f.apply(new Tuple(args));
	    }
	    return args[2];
	}
    }

    final public DMLValue foldri(DMLValue f, DMLValue init, int from, int to)
	throws RemoteException {
	int length = vec.length;
	if (to<0 || from<0 || length<to || to<from) {
	    _RAISENAME(General.Subscript);
	} else {
	    DMLValue[] args = new DMLValue[3];
	    args[2]=init;
	    for(int i=to-1; i>=from; i--) {
		args[0]=new Int(i);
		args[1]=vec[i];
		args[2]=f.apply(new Tuple(args));
	    }
	    return args[2];
	}
    }

    final public DMLValue tabulate(int n, DMLValue f)
	throws RemoteException {
	return new Vector(f,n);
    }

    final public DMLValue extract(int from, int to) {
	if (to<0 || from<0 || to<from || vec.length<to) {
	    _RAISENAME(General.Subscript);
	} else {
	    return new Vector(vec,from,to);
	}
    }

    final public DMLValue map(DMLValue f)
	throws RemoteException {
	int size = vec.length;
	Vector ret = new Vector(size);
	DMLValue[] val = ret.vec;
	for(int i=0; i<size; i++)
	    val[i] = f.apply(vec[i]);
	return ret;
    }

    /** @return Vector ein neuer Vektor */
    final public DMLValue mapi(DMLValue f, int from, int to)
	throws RemoteException {
	int size = vec.length;
	if (to<0 || from<0 || to<from || vec.length<to) {
	    _RAISENAME(General.Subscript);
	} else {
	    size=to-from;
	    Vector ret=new Vector(size);
	    DMLValue[] val = ret.vec;
	    for(int i=0; i<size; i++)
		val[i] = f.apply(vec[from+i]);
	    return ret;
	}
    }

    final public DMLValue copyVec(int from, int len, Array dest, int dfrom) {
	DMLValue[] destArray = dest.arr;
	int dlength = destArray.length;
	if (from<0 || dfrom<0 ||
	    vec.length<from+len || dlength<dfrom+len) {
	    _RAISENAME(General.Subscript);
	} else {
	    for(int i=0; i<len; i++)
		destArray[dfrom+i] = vec[from+i];
	    return Constants.dmlunit;
	}
    }

    final public static DMLValue concat(DMLValue list)
	throws RemoteException {
	int total = 0;
	if (list == List.nil)
	    return new Vector(0);
	else if (list instanceof Cons) {
	    DMLValue li = list;
	    while (li != List.nil) {
		if (li instanceof Cons) {
		    Cons lc = (Cons) li;
		    DMLValue car = lc.car;
		    if (!(car instanceof Vector)) {
			_RAISENAME(General.Match);
		    }
		    total += ((Vector) car).vec.length;
		    li = lc.cdr;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	    // Ab hier ist klar: Typ ok
	    Vector ret = new Vector(total);
	    DMLValue[] retvector = ret.vec;
	    int offset = 0;
	    li = list;
	    while (li!=List.nil) {
		Cons lc = (Cons) li;
		DMLValue[] car = ((Vector) lc.car).vec;
		int length = car.length;
		for(int j=0; j<length; j++)
		    retvector[j+offset] = car[j];
		offset += length;
		li = lc.cdr;
	    }
	    return ret;
	} else {
	    _RAISENAME(General.Match);
	}
    }

    _apply_fails ;

    final public java.lang.String toString() {
	java.lang.String s="["+vec[0];
	int l = vec.length;
	for(int i=1; i<l; i++)
	    s += ", "+vec[i];
	return s+"] : Vector";
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    java.lang.String s="["+vec[0];
	    int l = vec.length;
	    for(int i=1; i<l; i++)
		s += ", "+vec[i].toString(level-1);
	    return s+"] : Vector";
	}
    }
    //'
    /*************************************************************/
    /* Part 2
     */

    /** <code>val maxLen : int </code>*/
    final public static Int maxLen = new Int(Vector.maxLength);

    _BUILTIN(FromList) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.fromList");
	    return new Vector(val);
	}
    }
    /** <code>val fromList : 'a list -> 'a vector </code>*/
    _FIELD(Vector,fromList);

    _BUILTIN(Tabulate) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"Vector.tabulate");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue arg,v1);
	    int ar = ((Int) arg).value;
	    return new Vector(v2,ar);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a vector </code>*/
    _FIELD(Vector,tabulate);

    _BUILTIN(Length) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.length");
	    try {
		return new Int(((Vector) val).vec.length);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val length : 'a vector -> int </code>*/
    _FIELD(Vector,length);

    _BUILTIN(Sub) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"Vector.sub");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue vector,v1);
	    _REQUESTDEC(DMLValue idx,v2);
	    return ((Vector) vector).sub(((Int) idx).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val sub : ('a vector * int) -> 'a </code>*/
    _FIELD(Vector,sub);

    _BUILTIN(Extract) {
	_NOAPPLY0;_NOAPPLY2;_APPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,3,"Vector.extract");
	}
	_SAPPLY3(v) {
	    try {
		_REQUESTDEC(DMLValue vector,v1);
		_REQUESTDEC(DMLValue fr,v2);
		_REQUESTDEC(DMLValue to,v3);
		if (to==Option.NONE) {
		    Vector a=(Vector) vector;
		    return a.extract(((Int) fr).value,
				     a.vec.length);
		}
		if (to instanceof DMLConVal) {
		    DMLConVal cv = (DMLConVal) to;
		    if (cv.getConstructor()==Option.SOME) {
			to=cv.getContent();
			if (to instanceof Int)
			    return ((Vector) vector).
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
    /** <code>val extract : ('a vector * int * int option) -> 'a vector </code>*/
    _FIELD(Vector,extract);

    _BUILTIN(Concat) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.fromList");
	    return Vector.concat(val);
	}
    }
    /** <code>val concat : 'a vector list -> 'a vector </code>*/
    _FIELD(Vector,concat);

    _BUILTIN(Mapi) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.mapi");
	    return new Mapi1(val);
	}
	_BUILTIN(Mapi1) {
	    public DMLValue fun = null;
	    public Mapi1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
	    _APPLY(val) {
		_fromTuple(args,val,3,"Vector.mapi1");
	    }
	    _VAPPLY3(v) {
		try {
		_REQUESTDEC(DMLValue vector,v1);
		_REQUESTDEC(DMLValue from,v2);
		_REQUESTDEC(DMLValue to,v3);
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Vector) vector).vec.length;
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
		return ((Vector) vector).
		    mapi(fun,
			 ((Int) from).value,
			 toint);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val mapi : ((int * 'a) -> 'b) -> ('a vector * int * int option) -> 'b vector </code>*/
    _FIELD(Vector,mapi);

    _BUILTIN(Map) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.map");
	    return new Map1(val);
	}
	_BUILTIN(Map1) {
	    DMLValue fun = null;
	    Map1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"Vector.map1");
		try {
		    return ((Vector) val).map(fun);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val map : ('a -> 'b) -> 'a vector -> 'b vector </code>*/
    _FIELD(Vector,map);

    _BUILTIN(Appi) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.appi");
	    return new Appi1(val);
	}
	_BUILTIN(Appi1) {
	    public DMLValue fun = null;
	    public Appi1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
	    _APPLY(val) {
		_fromTuple(args,val,3,"Vector.appi1");
	    }
	    _VAPPLY3(v) {
		try {
		_REQUESTDEC(DMLValue vector,v1);
		_REQUESTDEC(DMLValue from,v2);
		_REQUESTDEC(DMLValue to,v3);
		int toint = 0;
		if (to==Option.NONE)
		    toint = ((Vector) vector).vec.length;
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
		return ((Vector) vector).
		    appi(((Int) from).value,
			 toint,
			 fun);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val appi : ((int * 'a) -> unit) -> ('a vector * int * int option) -> unit </code>*/
    _FIELD(Vector,appi);

    _BUILTIN(App) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.app");
	    return new App1(val);
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"Vector.app1");
		try {
		return ((Vector) val).app(fun);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a vector -> unit </code>*/
    _FIELD(Vector,app);

    _BUILTIN(Foldli) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.foldli");
	    return new Foldli1(val);
	}
	_BUILTIN(Foldli1) {
	    DMLValue fun = null;
	    Foldli1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"Vector.foldli1");
		return new Foldli2(fun,val);
	    }
	    _BUILTIN(Foldli2) {
		DMLValue init = null; DMLValue fun = null;
		Foldli2(DMLValue f, DMLValue i) { init=i; fun=f;}
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
		_APPLY(val) {
		    _fromTuple(args,val,3,"Vector.foldli2");
		}
		_VAPPLY3(v) {
		    try {
		    _REQUESTDEC(DMLValue vector,v1);
		    _REQUESTDEC(DMLValue fr,v2);
		    int from = ((Int) fr).value;
		    _REQUESTDEC(DMLValue to,v3);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Vector) vector).vec.length - from;
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
		    return ((Vector) vector).
			foldli(fun,
			       init,
			       from,
			       toint);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
		}
	    }
	}
    }
    /** <code>val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b </code>*/
    _FIELD(Vector,foldli);

    _BUILTIN(Foldri) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.foldri");
	    return new Foldri1(val);
	}
	_BUILTIN(Foldri1) {
	    DMLValue fun = null;
	    Foldri1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"Vector.foldri1");
		return new Foldri2(fun,val);
	    }
	    _BUILTIN(Foldri2) {
		DMLValue init = null; DMLValue fun = null;
		Foldri2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY4;
		_APPLY(val) {
		    _fromTuple(args,val,3,"Vector.foldri2");
		}
		_VAPPLY3(v) {
		    try {
		    _REQUESTDEC(DMLValue vector,v1);
		    _REQUESTDEC(DMLValue fr,v2);
		    int from = ((Int) fr).value;
		    _REQUESTDEC(DMLValue to,v3);
		    int toint = 0;
		    if (to==Option.NONE)
			toint = ((Vector) vector).vec.length - from;
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
		    return ((Vector) vector).
			foldri(fun,
			       init,
			       from,
			       toint);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
		}
	    }
	}
    }
    /** <code>val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int * int option) -> 'b </code>*/
    _FIELD(Vector,foldri);

    _BUILTIN(Foldl) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.foldl");
	    return new Foldl1(val);
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"Vector.foldl1");
		return new Foldl2(fun,val);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue init = null; DMLValue fun = null;
		Foldl2(DMLValue f,DMLValue i) { init=i; fun=f; }
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		_APPLY(val) {
		    // _FROMSINGLE(val,"Vector.foldl2");
		    if (!(val instanceof Vector)) {
			_RAISENAME(General.Match);
		    }
		    return ((Vector) val).foldl(fun,init);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b </code>*/
    _FIELD(Vector,foldl);

    _BUILTIN(Foldr) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Vector.foldr");
	    return new Foldr1(val);
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMTUPLE(args,val,1,"Vector.foldr1");
		return new Foldr2(fun,val);
	    }
	    _BUILTIN(Foldr2) {
		DMLValue init = null; DMLValue fun = null;
		Foldr2(DMLValue f, DMLValue i) { init=i; fun=f; }
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		_APPLY(val) {
		    // _FROMSINGLE(val,"Vector.foldr2");
		    try {
			return ((Vector) val).foldr(fun,init);
		} catch (ClassCastException c) {
		    _RAISENAME(General.Match);
		}
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b</code>*/
    _FIELD(Vector,foldr);
}
