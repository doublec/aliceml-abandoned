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

final public class List {
    UNAME(nil,List.nil);

    final public static class ConsConstructor extends UniqueConstructor {

	public ConsConstructor(java.lang.String n) {
	    super(n);
	}

	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	final public DMLValue apply(DMLValue val)
	    throws RemoteException {
	    _sfromTuple(args,val,2,"List.cons");
	}
	_SAPPLY2(v) {
	    return new Cons(v1,v2);
	}
    }

    final public static Constructor cons;
    static {
	// System.out.println("Builtin cons");
	Object o = GName.gNames.get("List.cons");
	if (o== null) {
	    cons = new ConsConstructor("List.cons");
	} else {
	    cons = (Constructor) o;
	}
    }

    /** <code>exception Empty</code>*/
    UNAME(Empty,List.Empty);

    _BUILTIN(IsNull) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.null");
	    _REQUEST(val,val);
	    if (val instanceof Cons) {
		return Constants.dmlfalse;
	    } else if (val==nil) {
		return Constants.dmltrue;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val null : 'a list -> bool</code>*/
    _FIELD(List,isNull);

    _BUILTIN(Length) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.length");
	    _REQUEST(val,val);
	    int length = 0;
	    while (val instanceof Cons) {
		length++;
		DMLValue co = ((Cons) val).cdr;
		_REQUEST(val,co);
	    }
	    if (val==nil) {
		return new Int(length);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val length : 'a list -> int </code>*/
    _FIELD(List,length);

    _BUILTIN(Append) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.append");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue first,v1);
	    if (first == nil)
		return v2;
	    else if (first instanceof Cons) {
		Cons newList = new Cons(null,null);
		Cons cons = newList;
		while (first != nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons.cdr = new Cons(fc.car,null);
			cons=(Cons) cons.cdr;
			_REQUEST(first,fc.cdr);
		    } else {
			System.err.println(first);
			dnl _RAISENAME(General.Match);
		    }
		}
		cons.cdr = v2;
		return newList.cdr;
	    } else {
		System.err.println(first);
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val @ : ('a list * 'a list) -> 'a list </code>*/
    _FIELD(List,append);

    _BUILTIN(Hd) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.hd");
	    _REQUEST(val,val);
	    if (val instanceof Cons) {
		return ((Cons) val).car;
	    } else if (val==nil) {
		_RAISENAME(List.Empty);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val hd : 'a list -> 'a </code>*/
    _FIELD(List,hd);

    _BUILTIN(Tl) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.tl");
	    _REQUEST(val,val);
	    if (val instanceof Cons) {
		return ((Cons) val).cdr;
	    } else if (val==nil) {
		_RAISENAME(List.Empty);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val tl : 'a list -> 'a list</code>*/
    _FIELD(List,tl);

    _BUILTIN(Last) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.last");
	    _REQUEST(val,val);
	    if (val==nil) {
		_RAISENAME(Empty);
	    } else if (val instanceof Cons) {
		_REQUESTDEC(DMLValue next,((Cons) val).cdr);
		while (next!=nil) {
		    if (next instanceof Cons) {
			val = next;
			DMLValue co = ((Cons) next).cdr;
			_REQUEST(next,co);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		return ((Cons) val).car;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val last : 'a list -> 'a </code>*/
    _FIELD(List,last);

    _BUILTIN(GetItem) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.getItem");
	    _REQUEST(val,val);
	    if (val==nil)
		return Option.NONE;
	    else if (val instanceof Cons) {
		DMLValue car = ((Cons) val).car;
		DMLValue cdr = ((Cons) val).cdr;
		return Option.SOME.apply2(car,cdr);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val getItem : 'a list -> ('a * 'a list) option </code>*/
    _FIELD(List,getItem);

    _BUILTIN(Nth) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.nth");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue n,v2);
	    if (!(n instanceof Int)) {
		_RAISENAME(General.Match);
	    }
	    int le = ((Int) n).value;
	    if (le<0) {
		_RAISENAME(General.Subscript);
	    }
	    _REQUESTDEC(DMLValue first,v1);
	    if (first==nil) {
		_RAISENAME(Empty);
	    } else if (first instanceof Cons) {
		int i=0;
		DMLValue next = first;
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			_REQUEST(next,((Cons) next).cdr);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		if (i<le) {
		    _RAISENAME(General.Subscript);
		} else {
		    return ((Cons) first).car;
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val nth : ('a list * int) -> 'a</code>*/
    _FIELD(List,nth);

    _BUILTIN(Take) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.take");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue n,v2);
	    if (!(n instanceof Int)) {
		_RAISENAME(General.Match);
	    }
	    int le = ((Int) n).value;
	    if (le<0) {
		_RAISENAME(General.Subscript);
	    }
	    _REQUESTDEC(DMLValue first,v2);
	    if (first==nil) {
		_RAISENAME(Empty);
	    } else if (first instanceof Cons) {
		int i=0;
		Cons newList = new Cons(null,null);
		Cons cons = newList;
		while (first!=nil && i<le) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			i++;
			cons.cdr = new Cons(fc.car,null);
			cons=(Cons)cons.cdr;
			_REQUEST(first,fc.cdr);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		if (i>le) {
		    _RAISENAME(General.Subscript);
		} else {
		    cons.cdr=nil;
		    return newList.cdr;
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val take : ('a list * int) -> 'a list </code>*/
    _FIELD(List,take);

    _BUILTIN(Drop) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.drop");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue n,v2);
	    if (!(n instanceof Int)) {
		_RAISENAME(General.Match);
	    }
	    int le = ((Int) n).value;
	    if (le<0) {
		_RAISENAME(General.Subscript);
	    }
	    _REQUESTDEC(DMLValue first,v1);
	    if (first==nil) {
		_RAISENAME(Empty);
	    } else if (first instanceof Cons) {
		int i=0;
		_REQUESTDEC(DMLValue next,((Cons) first).cdr);
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			DMLValue co = ((Cons) next).cdr;
			_REQUEST(next,co);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		if (i<le) {
		    _RAISENAME(General.Subscript);
		} else {
		    return ((Cons) first).cdr;
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val drop : ('a list * int) -> 'a list</code>*/
    _FIELD(List,drop);

    _BUILTIN(Rev) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.rev");
	    _REQUEST(val,val);
	    if (val==nil) {
		_RAISENAME(Empty);
	    }
	    else if (val instanceof Cons) {
		Cons cons = new Cons(((Cons) val).car,nil);
		_REQUEST(val,((Cons) val).cdr);
		while (val!=nil) {
		    if (val instanceof Cons) {
			Cons fc = (Cons) val;
			cons = new Cons(fc.car,cons);
			_REQUEST(val,fc.cdr);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		return cons;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val rev : 'a list -> 'a list </code>*/
    _FIELD(List,rev);

    _BUILTIN(Concat) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.concat");
	    _REQUEST(val,val);
	    if (val==nil)
		return nil;
	    else if (val instanceof Cons) {
		Cons result = new Cons(null,null);
		Cons cons = result;
		while (val!=nil) {
		    DMLValue li = ((Cons) val).car;
		    if (li==nil) {
			_REQUEST(val,((Cons) val).cdr);
			continue;
		    }
		    else if (li instanceof Cons) {
			while (li!=nil) {
			    if (li instanceof Cons) {
				Cons l = (Cons) li;
				cons.cdr = new Cons(l.car,null);
				cons=(Cons) cons.cdr;
				_REQUEST(li,l.cdr);
			    } else {
				_RAISENAME(General.Match);
			    }
			}
		    } else {
			_RAISENAME(General.Match);
		    }
		    _REQUEST(val,((Cons) val).cdr);
		}
		cons.cdr=nil;
		return result.cdr;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val concat : 'a list list -> 'a list </code>*/
    _FIELD(List,concat);

    _BUILTIN(RevAppend) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.revAppend");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue first,v1);
	    if (first==nil)
		return v2;
	    else if (first instanceof Cons) {
		Cons cons = new Cons(((Cons) first).car,v2);
		_REQUEST(first,((Cons) first).cdr);
		while (first!=nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons = new Cons(fc.car,cons);
			_REQUEST(first,fc.cdr);
		    } else {
			_RAISENAME(General.Match);
		    }
		}
		return cons;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val revAppend : ('a list * 'a list) -> 'a list </code>*/
    _FIELD(List,revAppend);

    _BUILTIN(App) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMTUPLE(args,val,1,"List.app");
	    return new App1(val);
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.app1");
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    fun.apply(lc.car);
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Constants.dmlunit;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a list -> unit </code>*/
    _FIELD(List,app);

    _BUILTIN(Map) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //      _FROMTUPLE(args,val,1,"List.map");
	    return new Map1(val);
	}
	_BUILTIN(Map1) {
	    DMLValue fun = null;
	    Map1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.map1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    list.cdr=new Cons(fun.apply(lc.car),
				      null);
		    list=(Cons) list.cdr;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val map : ('a -> 'b) -> 'a list -> 'b list </code>*/
    _FIELD(List,map);

    _BUILTIN(MapPartial) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMTUPLE(args,val,1,"List.mapPartial");
	    return new MapPartial1(val);
	}
	_BUILTIN(MapPartial1) {
	    DMLValue fun = null;
	    MapPartial1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.mapPartial1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res!=Option.NONE) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val mapPartial : ('a -> 'b option) -> 'a list -> 'b list </code>*/
    _FIELD(List,mapPartial);

    _BUILTIN(Find) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //      _FROMTUPLE(args,val,1,"List.find");
	    return new Find1(val);
	}
	_BUILTIN(Find1) {
	    DMLValue fun = null;
	    Find1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.find1");
		if (val==nil)
		    return Option.NONE;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res == Constants.dmltrue) {
			return Option.SOME.apply(lc.car);
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Option.NONE;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val find : ('a -> bool) -> 'a list -> 'a option </code>*/
    _FIELD(List,find);

    _BUILTIN(Filter) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.filter");
	    return new Filter1(val);
	}
	_BUILTIN(Filter1) {
	    DMLValue fun = null;
	    Filter1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.filter1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res==Constants.dmltrue) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val filter : ('a -> bool) -> 'a list -> 'a list </code>*/
    _FIELD(List,filter);

    _BUILTIN(Partition) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.partition");
	    return new Partition1(val);
	}
	_BUILTIN(Partition1) {
	    DMLValue fun = null;
	    Partition1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.partition1");
		if (val==nil)
		    return nil;
		Cons neg = new Cons(null,null);
		Cons nlist = neg;
		Cons pos = new Cons(null,null);
		Cons plist = pos;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res==Constants.dmltrue) {
			plist.cdr = new Cons(lc.car,null);
			plist=(Cons) plist.cdr;
		    }
		    else {
			nlist.cdr = new Cons(lc.car,null);
			nlist=(Cons) nlist.cdr;
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    plist.cdr=nil;
		    nlist.cdr=nil;
		    return new Tuple2(pos.cdr,neg.cdr);
		}
		else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val partition : ('a -> bool) -> 'a list -> ('a list * 'a list) </code>*/
    _FIELD(List,partition);

    _BUILTIN(Foldl) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.foldl");
	    return new Foldl1(val);
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		//              _FROMTUPLE(args,val,1,"List.foldl1");
		return new Foldl2(fun,val);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue fun = null; DMLValue init = null;
		Foldl2(DMLValue f, DMLValue i) {
		    fun=f;
		    init = i;
		}
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		_APPLY(val) {
		    // _FROMSINGLE(val,"List.foldl2");
		    if (val==nil)
			return init;
		    else if (val instanceof Cons) {
			DMLValue result=init;
			while (val instanceof Cons) {
			    Cons lc = (Cons) val;
			    result=fun.apply2(lc.car,result);
			    val = lc.cdr;
			}
			if (val==nil) {
			    return result;
			} else {
			    _RAISENAME(General.Match);
			}
		    } else {
			_RAISENAME(General.Match);
		    }
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b </code>*/
    _FIELD(List,foldl);

    _BUILTIN(Foldr) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //      _FROMTUPLE(args,val,1,"List.foldr");
	    return new Foldr1(val);
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		//_FROMTUPLE(args,val,1,"List.foldr1");
		return new Foldr2(fun,val);
	    }
	    _BUILTIN(Foldr2) {
		DMLValue fun = null; DMLValue init = null;
		Foldr2(DMLValue f, DMLValue i) {
		    fun=f;
		    init = i;
		}
		_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
		_APPLY(val) {
		    // _FROMSINGLE(val,"List.foldr2");
		    if (val==nil)
			return init;
		    else if (val instanceof Cons) {
			Cons cons = new Cons(((Cons) val).car,nil);
			_REQUEST(val,((Cons) val).cdr);
			while (val!=nil) {
			    if (val instanceof Cons) {
				Cons lc = (Cons) val;
				cons = new Cons(lc.car,cons);
				_REQUEST(val,lc.cdr);
			    } else {
				_RAISENAME(General.Match);
			    }
			}
			// in cons ist jetzt die umgedrehte Liste
			DMLValue result=init;
			while (cons instanceof Cons) {
			    Cons cc = (Cons) cons;
			    result=fun.apply2(cc.car,result);
			    if (cc.cdr instanceof Cons)
				cons = (Cons) cc.cdr;
			    else
				break;
			}
			return result;
		    } else {
			_RAISENAME(General.Match);
		    }
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b </code>*/
    _FIELD(List,foldr);

    _BUILTIN(Exists) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //FROMTUPLE(args,val,1,"List.exists");
	    return new Exists1(val);
	}
	_BUILTIN(Exists1) {
	    DMLValue fun = null;
	    Exists1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.exists1");
		if (val==nil)
		    return Constants.dmlfalse;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res==Constants.dmltrue)
			return Constants.dmltrue;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Constants.dmlfalse;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val exists : ('a -> bool) -> 'a list -> bool </code>*/
    _FIELD(List,exists);

    _BUILTIN(All) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _fromTuple(args,val,1,"List.all");
	    return new All1(val);
	}
	_BUILTIN(All1) {
	    DMLValue fun = null;
	    All1(DMLValue f) { fun=f; }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.all1");
		if (val==nil)
		    return Constants.dmltrue;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(lc.car);
		    if (res!=Constants.dmltrue)
			return Constants.dmlfalse;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Constants.dmltrue;
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }
    /** <code>val all : ('a -> bool) -> 'a list -> bool </code>*/
    _FIELD(List,all);

    _BUILTIN(Tabulate) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"List.tabulate");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue n,v1);
	    if (!(n instanceof Int)) {
		_RAISENAME(General.Match);
	    }
	    int k = ((Int) n).value;
	    if (k<0) {
		_RAISENAME(General.Size);
	    }
	    DMLValue fun = v2;
	    Cons first = new Cons(null,null);
	    Cons cons = first;
	    for(int i=0; i<k; i++) {
		cons.cdr=new Cons(fun.apply(new Int(i)),null);
		cons=(Cons)cons.cdr;
	    }
	    cons.cdr=nil;
	    return first.cdr;
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a list }</code>*/
    _FIELD(List,tabulate);
}
