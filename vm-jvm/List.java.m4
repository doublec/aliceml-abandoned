/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class List {
    final public static Name nil = new UniqueName("List.nil");
    final public static Constructor cons = new UniqueConstructor("List.cons");
    /** <code>exception Empty</code>*/
    final public static Name Empty = new UniqueName("List.Empty");

    _BUILTIN(IsNull) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.null");
	    if (val instanceof Cons)
		return Constants.dmlfalse;
	    else if (val==nil)
		return Constants.dmltrue;
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val null : 'a list -> bool</code>*/
    _FIELD(List,isNull);

    _BUILTIN(Length) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.length");
	    int length = 0;
	    while (val instanceof Cons) {
		length++;
		DMLValue co = ((Cons) val).cdr;
		_REQUEST(val,co);
	    }
	    if (val==nil)
		return new Int(length);
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val length : 'a list -> int </code>*/
    _FIELD(List,length);

    _BUILTIN(Append) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.append");
	    _REQUESTDEC(DMLValue first,args[0]);
	    if (first==nil)
		return args[1];
	    else if (first instanceof Cons) {
		Cons newList = new Cons(null,null);
		Cons cons = newList;
		while (first!=nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons.cdr = new Cons(fc.car,null);
			cons=(Cons) cons.cdr;
			_REQUEST(first,fc.cdr);
		    }
		    else
			_error("argument not List",val);
		}
		cons.cdr = args[1];
		return newList.cdr;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val @ : ('a list * 'a list) -> 'a list </code>*/
    _FIELD(List,append);

    _BUILTIN(Hd) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.hd");
	    if (val instanceof Cons)
		return ((Cons) val).car;
	    else if (val==nil)
		_RAISENAME(Empty);
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val hd : 'a list -> 'a </code>*/
    _FIELD(List,hd);

    _BUILTIN(Tl) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.tl");
	    if (val instanceof Cons)
		return ((Cons) val).cdr;
	    else if (val==nil)
		_RAISENAME(Empty);
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val tl : 'a list -> 'a list</code>*/
    _FIELD(List,tl);

    _BUILTIN(Last) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.last");
	    if (val==nil)
		_RAISENAME(Empty);
	    else if (val instanceof Cons) {
		_REQUESTDEC(DMLValue next,((Cons) val).cdr);
		while (next!=nil) {
		    if (next instanceof Cons) {
			val = next;
			DMLValue co = ((Cons) next).cdr;
			_REQUEST(next,co);
		    }
		    else
			_error("argument not List",val);
		}
		return ((Cons) val).car;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val last : 'a list -> 'a </code>*/
    _FIELD(List,last);

    _BUILTIN(GetItem) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.getItem");
	    if (val==nil)
		return Option.NONE;
	    else if (val instanceof Cons) {
		DMLValue car = ((Cons) val).car;
		DMLValue cdr = ((Cons) val).cdr;
		return Option.SOME.apply(new Tuple2(car,cdr));
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val getItem : 'a list -> ('a * 'a list) option </code>*/
    _FIELD(List,getItem);

    _BUILTIN(Nth) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.nth");
	    _REQUESTDEC(DMLValue n,args[1]);
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).value;
	    if (le<0)
		_RAISENAME(General.Subscript);
	    _REQUESTDEC(DMLValue first,args[0]);
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		int i=0;
		DMLValue next = first;
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			_REQUEST(next,((Cons) next).cdr);
		    }
		    else
			_error("argument 1 not List",val);
		}
		if (i<le)
		    _RAISENAME(General.Subscript);
		else
		    return ((Cons) first).car;
	    } else
		_error("argument 1 not List",val);
	}
    }
    /** <code>val nth : ('a list * int) -> 'a</code>*/
    _FIELD(List,nth);

    _BUILTIN(Take) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.take");
	    _REQUESTDEC(DMLValue n,args[1]);
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).value;
	    if (le<0)
		_RAISENAME(General.Subscript);
	    _REQUESTDEC(DMLValue first,args[0]);
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
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
		    }
		    else
			_error("argument 1 not List",val);
		}
		if (i>le)
		    _RAISENAME(General.Subscript);
		else {
		    cons.cdr=nil;
		    return newList.cdr;
		}
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val take : ('a list * int) -> 'a list </code>*/
    _FIELD(List,take);

    _BUILTIN(Drop) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.drop");
	    _REQUESTDEC(DMLValue n,args[1]);
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).value;
	    if (le<0)
		_RAISENAME(General.Subscript);
	    _REQUESTDEC(DMLValue first,args[0]);
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		int i=0;
		_REQUESTDEC(DMLValue next,((Cons) first).cdr);
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			DMLValue co = ((Cons) next).cdr;
			_REQUEST(next,co);
		    }
		    else
			_error("argument 1 not List",val);
		}
		if (i<le)
		    _RAISENAME(General.Subscript);
		else
		    return ((Cons) first).cdr;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val drop : ('a list * int) -> 'a list</code>*/
    _FIELD(List,drop);

    _BUILTIN(Rev) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.rev");
	    if (val==nil)
		_RAISENAME(Empty);
	    else if (val instanceof Cons) {
		Cons cons = new Cons(((Cons) val).car,nil);
		_REQUEST(val,((Cons) val).cdr);
		while (val!=nil) {
		    if (val instanceof Cons) {
			Cons fc = (Cons) val;
			cons = new Cons(fc.car,cons);
			_REQUEST(val,fc.cdr);
		    }
		    else
			_error("argument 1 not List",val);
		}
		return cons;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val rev : 'a list -> 'a list </code>*/
    _FIELD(List,rev);

    _BUILTIN(Concat) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"List.concat");
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
			    } else
				_error("argument not List list",val);
			}
		    } else
			_error("argument not List",val);
		    _REQUEST(val,((Cons) val).cdr);
		}
		cons.cdr=nil;
		return result.cdr;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val concat : 'a list list -> 'a list </code>*/
    _FIELD(List,concat);

    _BUILTIN(RevAppend) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.revAppend");
	    _REQUESTDEC(DMLValue first,args[0]);
	    if (first==nil)
		return args[1];
	    else if (first instanceof Cons) {
		Cons cons = new Cons(((Cons) first).car,args[1]);
		_REQUEST(first,((Cons) first).cdr);
		while (first!=nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons = new Cons(fc.car,cons);
			_REQUEST(first,fc.cdr);
		    }
		    else
			_error("argument not List",val);
		}
		return cons;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val revAppend : ('a list * 'a list) -> 'a list </code>*/
    _FIELD(List,revAppend);

    _BUILTIN(App) {
	_APPLY(val) {
	    // _FROMTUPLE(args,val,1,"List.app");
	    return new App1(val);
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.app1");
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    fun.apply(lc.car);
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil)
		    return Constants.dmlunit;
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val app : ('a -> unit) -> 'a list -> unit </code>*/
    _FIELD(List,app);

    _BUILTIN(Map) {
	_APPLY(val) {
	    //	    _FROMTUPLE(args,val,1,"List.map");
	    return new Map1(val);
	}
	_BUILTIN(Map1) {
	    DMLValue fun = null;
	    Map1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.map1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    list.cdr=new Cons(fun.apply(new Tuple1(lc.car)),
				      null);
		    list=(Cons) list.cdr;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val map : ('a -> 'b) -> 'a list -> 'b list </code>*/
    _FIELD(List,map);

    _BUILTIN(MapPartial) {
	_APPLY(val) {
	    // _FROMTUPLE(args,val,1,"List.mapPartial");
	    return new MapPartial1(val);
	}
	_BUILTIN(MapPartial1) {
	    DMLValue fun = null;
	    MapPartial1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.mapPartial1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res!=Option.NONE) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val mapPartial : ('a -> 'b option) -> 'a list -> 'b list </code>*/
    _FIELD(List,mapPartial);

    _BUILTIN(Find) {
	_APPLY(val) {
	    //	    _FROMTUPLE(args,val,1,"List.find");
	    return new Find1(val);
	}
	_BUILTIN(Find1) {
	    DMLValue fun = null;
	    Find1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.find1");
		if (val==nil)
		    return Constants.dmlfalse;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue)
			return Option.SOME.apply(lc.car);
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Option.NONE;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val find : ('a -> bool) -> 'a list -> 'a option </code>*/
    _FIELD(List,find);

    _BUILTIN(Filter) {
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.filter");
	    return new Filter1(val);
	}
	_BUILTIN(Filter1) {
	    DMLValue fun = null;
	    Filter1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.filter1");
		if (val==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    list.cdr=nil;
		    return first.cdr;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val filter : ('a -> bool) -> 'a list -> 'a list </code>*/
    _FIELD(List,filter);

    _BUILTIN(Partition) {
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.partition");
	    return new Partition1(val);
	}
	_BUILTIN(Partition1) {
	    DMLValue fun = null;
	    Partition1(DMLValue f) { fun=f; }
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
		    DMLValue res=fun.apply(new Tuple1(lc.car));
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
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val partition : ('a -> bool) -> 'a list -> ('a list * 'a list) </code>*/
    _FIELD(List,partition);

    _BUILTIN(Foldl) {
	_APPLY(val) {
	    //_FROMTUPLE(args,val,1,"List.foldl");
	    return new Foldl1(val);
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		//		_FROMTUPLE(args,val,1,"List.foldl1");
		return new Foldl2(fun,val);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue fun = null; DMLValue init = null;
		Foldl2(DMLValue f, DMLValue i) {
		    fun=f;
		    init = i;
		}
		_APPLY(val) {
		    // _FROMSINGLE(val,"List.foldl2");
		    if (val==nil)
			return init;
		    else if (val instanceof Cons) {
			DMLValue result=init;
			while (val instanceof Cons) {
			    Cons lc = (Cons) val;
			    result=fun.apply(new Tuple2(lc.car,result));
			    val = lc.cdr;
			}
			if (val==nil)
			    return result;
			else
			    _error("argument not List",val);
		    } else
			_error("argument not List",val);
		}
	    }
	}
    }
    /** <code>val foldl : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b </code>*/
    _FIELD(List,foldl);

    _BUILTIN(Foldr) {
	_APPLY(val) {
	    //	    _FROMTUPLE(args,val,1,"List.foldr");
	    return new Foldr1(val);
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
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
			    }
			    else
				_error("argument 1 not List",val);
			}
			// in cons ist jetzt die umgedrehte Liste
			DMLValue result=init;
			while (cons instanceof Cons) {
			    Cons cc = (Cons) cons;
			    result=fun.apply(new Tuple2(cc.car,result));
			    if (cc.cdr instanceof Cons)
				cons = (Cons) cc.cdr;
			    else
				break;
			}
			return result;
		    } else
			_error("argument not List",val);
		}
	    }
	}
    }
    /** <code>val foldr : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b </code>*/
    _FIELD(List,foldr);

    _BUILTIN(Exists) {
	_APPLY(val) {
	    //FROMTUPLE(args,val,1,"List.exists");
	    return new Exists1(val);
	}
	_BUILTIN(Exists1) {
	    DMLValue fun = null;
	    Exists1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.exists1");
		if (val==nil)
		    return Constants.dmlfalse;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue)
			return Constants.dmltrue;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Constants.dmlfalse;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val exists : ('a -> bool) -> 'a list -> bool </code>*/
    _FIELD(List,exists);

    _BUILTIN(All) {
	_APPLY(val) {
	    // _fromTuple(args,val,1,"List.all");
	    return new All1(val);
	}
	_BUILTIN(All1) {
	    DMLValue fun = null;
	    All1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		// _FROMSINGLE(val,"List.all1");
		if (val==nil)
		    return Constants.dmltrue;
		while (val instanceof Cons) {
		    Cons lc = (Cons) val;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res!=Constants.dmltrue)
			return Constants.dmlfalse;
		    _REQUEST(val,lc.cdr);
		}
		if (val==nil) {
		    return Constants.dmltrue;
		}
		else
		    _error("argument not List",val);
	    }
	}
    }
    /** <code>val all : ('a -> bool) -> 'a list -> bool </code>*/
    _FIELD(List,all);

    _BUILTIN(Tabulate) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"List.tabulate");
	    _REQUESTDEC(DMLValue n,args[0]);
	    if (!(n instanceof Int))
		_error("argument 1 not Int",val);
	    int k = ((Int) n).value;
	    if (k<0)
		_RAISENAME(General.Size);
	    DMLValue fun = args[1];
	    Cons first = new Cons(null,null);
	    Cons cons = first;
	    for(int i=0; i<k; i++) {
		cons.cdr=new Cons(fun.apply(new Tuple1(new Int(i))),null);
		cons=(Cons)cons.cdr;
	    }
	    cons.cdr=nil;
	    return first.cdr;
	}
    }
    /** <code>val tabulate : (int * (int -> 'a)) -> 'a list }</code>*/
    _FIELD(List,tabulate);
}
