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
	    _fromTuple(args,val,1,"List.null");
	    DMLValue l = args[0].request();
	    if (l instanceof Cons)
		return Constants.dmlfalse;
	    else if (l==nil)
		return Constants.dmltrue;
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val null : 'a list -> bool</code>*/
    _FIELD(List,isNull);

    _BUILTIN(Length) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"List.length");
	    DMLValue l = args[0].request();
	    int length = 0;
	    while (l instanceof Cons) {
		length++;
		l = ((Cons) l).cdr.request();
	    }
	    if (l==nil)
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
	    DMLValue first = args[0].request();
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
			first = fc.cdr.request();
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
	    _fromTuple(args,val,1,"List.hd");
	    DMLValue first = args[0].request();
	    if (first instanceof Cons)
		return ((Cons) first).car;
	    else if (first==nil)
		_RAISENAME(Empty);
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val hd : 'a list -> 'a </code>*/
    _FIELD(List,hd);

    _BUILTIN(Tl) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"List.tl");
	    DMLValue first = args[0].request();
	    if (first instanceof Cons)
		return ((Cons) first).cdr;
	    else if (first==nil)
		_RAISENAME(Empty);
	    else
		_error("argument not List",val);
	}
    }
    /** <code>val tl : 'a list -> 'a list</code>*/
    _FIELD(List,tl);

    _BUILTIN(Last) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"List.last");
	    DMLValue first = args[0].request();
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		DMLValue next = ((Cons) first).cdr.request();
		while (next!=nil) {
		    if (next instanceof Cons) {
			first = next;
			next = ((Cons) next).cdr.request();
		    }
		    else
			_error("argument not List",val);
		}
		return ((Cons) first).car;
	    } else
		_error("argument not List",val);
	}
    }
    /** <code>val last : 'a list -> 'a </code>*/
    _FIELD(List,last);

    _BUILTIN(GetItem) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"List.getItem");
	    DMLValue first = args[0].request();
	    if (first==nil)
		return Option.NONE;
	    else if (first instanceof Cons) {
		DMLValue car = ((Cons) first).car;
		DMLValue cdr = ((Cons) first).cdr;
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
	    DMLValue n = args[1].request();
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).getInt();
	    if (le<0)
		_RAISENAME(General.Subscript);
	    DMLValue first = args[0].request();
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		int i=0;
		DMLValue next = first;
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			next = ((Cons) next).cdr.request();
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
	    DMLValue n = args[1].request();
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).getInt();
	    if (le<0)
		_RAISENAME(General.Subscript);
	    DMLValue first = args[0].request();
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
			first = fc.cdr.request();
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
	    DMLValue n = args[1].request();
	    if (!(n instanceof Int))
		_error("argument 2 not Int",val);
	    int le = ((Int) n).getInt();
	    if (le<0)
		_RAISENAME(General.Subscript);
	    DMLValue first = args[0].request();
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		int i=0;
		DMLValue next = ((Cons) first).cdr.request();
		while (next!=nil && i<le) {
		    if (next instanceof Cons) {
			i++;
			first = next;
			next = ((Cons) next).cdr.request();
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
	    _fromTuple(args,val,1,"List.rev");
	    DMLValue first = args[0].request();
	    if (first==nil)
		_RAISENAME(Empty);
	    else if (first instanceof Cons) {
		Cons cons = new Cons(((Cons) first).car,nil);
		first = ((Cons) first).cdr.request();
		while (first!=nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons = new Cons(fc.car,cons);
			first = fc.cdr.request();
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
	    _fromTuple(args,val,1,"List.concat");
	    DMLValue first = args[0].request();
	    if (first==nil)
		return nil;
	    else if (first instanceof Cons) {
		Cons result = new Cons(null,null);
		Cons cons = result;
		while (first!=nil) {
		    DMLValue li = ((Cons) first).car;
		    if (li==nil) {
			first=((Cons) first).cdr.request();
			continue;
		    }
		    else if (li instanceof Cons) {
			while (li!=nil) {
			    if (li instanceof Cons) {
				Cons l = (Cons) li;
				cons.cdr = new Cons(l.car,null);
				cons=(Cons) cons.cdr;
				li = l.cdr.request();
			    } else
				_error("argument not List list",val);
			}
		    } else
			_error("argument not List",val);
		    first=((Cons) first).cdr.request();
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
	    DMLValue first = args[0].request();
	    if (first==nil)
		return args[1];
	    else if (first instanceof Cons) {
		Cons cons = new Cons(((Cons) first).car,args[1]);
		first = ((Cons) first).cdr.request();
		while (first!=nil) {
		    if (first instanceof Cons) {
			Cons fc = (Cons) first;
			cons = new Cons(fc.car,cons);
			first = fc.cdr.request();
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
	    _fromTuple(args,val,1,"List.app");
	    return new App1(args[0]);
	}
	_BUILTIN(App1) {
	    DMLValue fun = null;
	    App1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.app1");
		DMLValue l = args[0].request();
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    fun.apply(lc.car);
		    l = lc.cdr.request();
		}
		if (l==nil)
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
	    _fromTuple(args,val,1,"List.map");
	    return new Map1(args[0]);
	}
	_BUILTIN(Map1) {
	    DMLValue fun = null;
	    Map1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.map1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    list.cdr=new Cons(fun.apply(new Tuple1(lc.car)),
				      null);
		    list=(Cons) list.cdr;
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.mapPartial");
	    return new MapPartial1(args[0]);
	}
	_BUILTIN(MapPartial1) {
	    DMLValue fun = null;
	    MapPartial1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.mapPartial1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res!=Option.NONE) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.find");
	    return new Find1(args[0]);
	}
	_BUILTIN(Find1) {
	    DMLValue fun = null;
	    Find1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.find1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return Constants.dmlfalse;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue)
			return Option.SOME.apply(lc.car);
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.filter");
	    return new Filter1(args[0]);
	}
	_BUILTIN(Filter1) {
	    DMLValue fun = null;
	    Filter1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.filter1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return nil;
		Cons first = new Cons(null,null);
		Cons list = first;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue) {
			list.cdr = new Cons(lc.car,null);
			list=(Cons)list.cdr;
		    }
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.partition");
	    return new Partition1(args[0]);
	}
	_BUILTIN(Partition1) {
	    DMLValue fun = null;
	    Partition1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.partition1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return nil;
		Cons neg = new Cons(null,null);
		Cons nlist = neg;
		Cons pos = new Cons(null,null);
		Cons plist = pos;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue) {
			plist.cdr = new Cons(lc.car,null);
			plist=(Cons) plist.cdr;
		    }
		    else {
			nlist.cdr = new Cons(lc.car,null);
			nlist=(Cons) nlist.cdr;
		    }
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.foldl");
	    return new Foldl1(args[0]);
	}
	_BUILTIN(Foldl1) {
	    DMLValue fun = null;
	    Foldl1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.foldl1");
		return new Foldl2(fun,args[0]);
	    }
	    _BUILTIN(Foldl2) {
		DMLValue fun = null; DMLValue init = null;
		Foldl2(DMLValue f, DMLValue i) {
		    fun=f;
		    init = i;
		}
		_APPLY(val) {
		    _fromTuple(args,val,1,"List.foldl2");
		    DMLValue li = args[0].request();
		    if (li==nil)
			return init;
		    else if (li instanceof Cons) {
			DMLValue result=init;
			while (li instanceof Cons) {
			    Cons lc = (Cons) li;
			    result=fun.apply(new Tuple2(lc.car,result));
			    li = lc.cdr;
			}
			if (li==nil)
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
	    _fromTuple(args,val,1,"List.foldr");
	    return new Foldr1(args[0]);
	}
	_BUILTIN(Foldr1) {
	    DMLValue fun = null;
	    Foldr1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.foldr1");
		return new Foldr2(fun,args[0]);
	    }
	    _BUILTIN(Foldr2) {
		DMLValue fun = null; DMLValue init = null;
		Foldr2(DMLValue f, DMLValue i) {
		    fun=f;
		    init = i;
		}
		_APPLY(val) {
		    _fromTuple(args,val,1,"List.foldr2");
		    DMLValue li = args[0].request();
		    if (li==nil)
			return init;
		    else if (li instanceof Cons) {
			Cons cons = new Cons(((Cons) li).car,nil);
			li = ((Cons) li).cdr.request();
			while (li!=nil) {
			    if (li instanceof Cons) {
				Cons lc = (Cons) li;
				cons = new Cons(lc.car,cons);
				li = lc.cdr.request();
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
	    _fromTuple(args,val,1,"List.exists");
	    return new Exists1(args[0]);
	}
	_BUILTIN(Exists1) {
	    DMLValue fun = null;
	    Exists1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.exists1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return Constants.dmlfalse;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res==Constants.dmltrue)
			return Constants.dmltrue;
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    _fromTuple(args,val,1,"List.all");
	    return new All1(args[0]);
	}
	_BUILTIN(All1) {
	    DMLValue fun = null;
	    All1(DMLValue f) { fun=f; }
	    _APPLY(val) {
		_fromTuple(args,val,1,"List.all1");
		DMLValue l = args[0].request();
		if (l==nil)
		    return Constants.dmltrue;
		while (l instanceof Cons) {
		    Cons lc = (Cons) l;
		    DMLValue res=fun.apply(new Tuple1(lc.car));
		    if (res!=Constants.dmltrue)
			return Constants.dmlfalse;
		    l = lc.cdr.request();
		}
		if (l==nil) {
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
	    DMLValue n = args[0].request();
	    if (!(n instanceof Int))
		_error("argument 1 not Int",val);
	    int k = ((Int) n).getInt();
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
