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

final public class General {
    /** <code>datatype order = LESS | EQUAL | GREATER</code>*/
    final public static Name LESS = new UniqueName("order.LESS");
    final public static Name EQUAL = new UniqueName("order.EQUAL");
    final public static Name GREATER = new UniqueName("order.GREATER");
    /** <code>exception Bind</code>*/
    final public static Name Bind = new UniqueName("General.Bind");
    /** <code>exception Chr</code>*/
    final public static Name Chr = new UniqueName("General.Chr");
    /** <code>exception Div</code>*/
    final public static Name Div = new UniqueName("General.Div");
    /** <code>exception Domain</code>*/
    final public static Name Domain = new UniqueName("General.Domain");
    /** <code>exception Fail of string </code>*/
    final public static Constructor Fail = new UniqueConstructor("General.Fail");
    /** <code>exception Match</code>*/
    final public static Name Match = new UniqueName("General.Match");
    /** <code>exception Overflow</code>*/
    final public static Name Overflow = new UniqueName("General.Overflow");
    /** <code>exception Size</code>*/
    final public static Name Size = new UniqueName("General.Size");
    /** <code>exception Span</code>*/
    final public static Name Span = new UniqueName("General.Span");
    /** <code>exception Subscript</code>*/
    final public static Name Subscript = new UniqueName("General.Subscript");

    _BUILTIN(Deref) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"deref");
	    if (val instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) val;
		if (cv.getConstructor()==Constants.reference)
		    return cv.getContent();
	    }
	    _error("wrong argument 1 for deref",val);
	}
    }
    /** <code>val ! : 'a ref -> 'a</code>*/
    _FIELD(General,deref);

    _BUILTIN(Assign) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"assign");
	    _REQUESTDEC(DMLValue car,args[0]);
	    if (car instanceof Reference) {
		return ((Reference) car).assign(args[1]);
	    } else {
		_error("wrong argument 1 for assign",val);
	    }
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    _FIELD(General,assign);
    static {
	Builtin.builtins.put(":=",assign);
    }

    _BUILTIN(Bind) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.bind");
	    if (args[0] instanceof DMLLVar) {
		return ((DMLLVar) args[0]).bind(args[1]);
	    } else {
		_error("wrong argument 1 for bind",val);
	    }
	}
    }
    /** <code>val bind : (lvar * 'a) -> unit</code>*/
    _FIELD(General,bind);

    _BUILTIN(O) {
	final public class CO extends Builtin {
	    public DMLValue f,g;
	    public CO(DMLValue f, DMLValue g) {
		this.f=f;
		this.g=g;
	    }
	    _APPLY(v) {
		return f.apply(g.apply(v));
	    }
	}
	_APPLY(val) {
	    _fromTuple(args,val,2,"compose");
	    _REQUESTDEC(DMLValue f,args[0]);
	    _REQUESTDEC(DMLValue g,args[1]);
	    return new CO(f,g);
	}
    }
    /** <code>val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c </code>*/
    _FIELD(General,o);

    _BUILTIN(Before) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"before");
	    return args[0];
	}
    }
    /** <code>val before : ('a * unit) -> 'a</code>*/
    // wirft einfach das zweite Argument weg
    _FIELD(General,before);

    _BUILTIN(Ignore) {
	_APPLY(_) {
	    return Constants.dmlunit;
	}
    }
    /** <code>val ignore : 'a -> unit </code>*/
    _FIELD(General,ignore);

    _BUILTIN(Lvar) {
	_APPLY(_) {
	    return new LVar();
	}
    }
    /** <code>val lvar : _ -> lvar</code>*/
    _FIELD(General,lvar);

    /** Ref-Zellen-Konstruktor, entspricht etwa NewCell oder so.*/
    _BUILTIN(Ref) {
	_APPLY(val) {
	    // FROMTUPLE(args,val,1,"General.ref");
	    return new Reference(val);
	}
    }
    /** <code>val ref : 'a -> ref 'a</code>*/
    _FIELD(General,ref);
    static {
	Builtin.builtins.put("ref",ref);
    }

    _BUILTIN(Spawn) {
	_APPLY(val) {
	    // FROMTUPLE(args,val,1,"spawn");
	    de.uni_sb.ps.dml.runtime.Thread t=new de.uni_sb.ps.dml.runtime.Thread(val);
	    t.start();
	    return Constants.dmlunit;
	}
    }
    /** <code>val spawn : (_ -> 'a) -> unit</code>
     *  spawn startet einen neuen de.uni_sb.ps.dml.runtime.Thread, der das Argument
     *  von <code>apply</code> appliziert.
     */
    _FIELD(General,spawn);

    _BUILTIN(Equals) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"equals");
	    _REQUESTDEC(DMLValue car,args[0]);
	    _REQUESTDEC(DMLValue cdr,args[1]);
	    if (car.equals(cdr))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
    }
    /** <code>val equals : ('a * 'b) -> bool</code>*/
    _FIELD(General,equals);

    _BUILTIN(Pickle) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.pickle");
	    _REQUESTDEC(DMLValue fst,args[0]);
	    if (!(fst instanceof STRING)) {
		_error("argument 1 not STRING ",val);
	    }
	    java.lang.String whereto=((STRING) fst).value;
	    return apply(whereto,null,args[1]);
	}

	final public static DMLValue apply(java.lang.String name,
					   DMLValue val1,
					   DMLValue val2) throws java.rmi.RemoteException {
	    ExceptionWrapper ex=null;
	    java.io.FileOutputStream outf=null;
	    PickleOutputStream out=null;
	    try{
		outf=new java.io.FileOutputStream(name);
		out=new PickleOutputStream(outf);
		out.writeObject(val1);
		out.writeObject(val2);
		outf.flush();
		out.flush();
	    } catch (Exception e) {
		System.err.println(e);
		e.printStackTrace();
		ex = new ExceptionWrapper(Constants.runtimeError.apply(new STRING (e.getMessage())));
	    }
	    finally {
		try {
		    outf.close();
		} catch (Exception e) {
		    System.err.println(e);
		    e.printStackTrace();
		}
		if (ex != null)
		    throw ex;
	    }
	    System.out.println("Pickled "+out.objectcounter+" objects.");
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,pickle);

    _BUILTIN(Unpickle) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.unpickle");
	    _REQUESTDEC(DMLValue fst,args[0]);
	    if (!(fst instanceof STRING)) {
		_error("argument 1 not STRING ",val);
	    }
	    java.lang.String wherefrom=((STRING) fst).value;
	    ExceptionWrapper ex=null;
	    java.io.FileInputStream inf=null;
	    PickleInputStream in=null;
	    DMLValue result = null;
	    try{
		inf=new java.io.FileInputStream(wherefrom);
		in=new PickleInputStream(inf);
		in.readObject();
		result=(DMLValue) in.readObject();
	    } catch (Exception e) {
		System.err.println(e);
		e.printStackTrace();
		ex = new ExceptionWrapper(Constants.runtimeError.apply(new STRING (e.getMessage())));
	    }
	    finally {
		try {
		    inf.close();
		} catch (Exception e) {
		    System.err.println(e);}
		if (ex != null)
		    throw ex;
	    }
	    return result;
	}
    }
    _FIELD(General,unpickle);


    _BUILTIN(Uminus) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"General.~");
	    if (val instanceof Int) {
		return new Int(-((Int) val).value);
	    } else if (val instanceof Word) {
		return new Word(-((Word) val).value);
	    } else if (val instanceof Real) {
		return new Real(-((Real) val).value);
	    } else {
		_error("argument not Number",val);
	    }
	}
    }
    _FIELD(General,uminus);
    static {
	Builtin.builtins.put("~",uminus);
    }

    _BINOP(plus,+);
    static {
	Builtin.builtins.put("+",plus);
    }
    _BINOP(minus,-);
    static {
	Builtin.builtins.put("-",minus);
    }
    _BINOP(div,/);
    static {
	Builtin.builtins.put("div",div);
    }
    _BINOP(mod,%);
    static {
	Builtin.builtins.put("mod",mod);
    }
    _BINOP(mult,*);
    static {
	Builtin.builtins.put("*",mult);
    }

    _COMPARE(greater,>);
    _COMPARE(geq,>=);
    _COMPARE(less,<);
    _COMPARE(leq,<=);

    _BUILTIN(Neq) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.<>");
	    _REQUESTDEC(DMLValue l, args[0]);
	    _REQUESTDEC(DMLValue r, args[1]);
	    if (l.equals(r)) {
		return Constants.dmlfalse;
	    } else {
		return Constants.dmltrue;
	    }
	}
    }
    _FIELD(General,neq);
    static {
	Builtin.builtins.put("<>",neq);
    }
    _BUILTIN(Sel) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"General.sel");
	    if (val instanceof Int) {
		return new SelFunInt(((Int) val).value);
	    } else if (val instanceof STRING) {
		return new SelFunString(((STRING) val).value);
	    } else {
		_error("argument not string or int",val);
	    }
	}
	_BUILTIN(SelFunInt) {
	    int i = -1;
	    public SelFunInt(int idx) {
		i = idx;
	    }
	    _APPLY(val) {
		// _FROMSINGLE(val,"General.sel "+i);
		if (val instanceof DMLTuple) {
		    switch (i) {
		    case 1: return ((DMLTuple) val).get0();
		    case 2: return ((DMLTuple) val).get1();
		    case 3: return ((DMLTuple) val).get2();
		    case 4: return ((DMLTuple) val).get3();
		    case 5: return ((DMLTuple) val).get4();
		    default: return ((DMLTuple) val).get(i);
		    }
		} else {
		    _error("argument not tuple",val);
		}
	    }
	}
	_BUILTIN(SelFunString) {
	    java.lang.String lab = null;
	    public SelFunString(java.lang.String str) {
		lab = str;
	    }
	    _APPLY(val) {
		// _FROMSINGLE(val,"General.sel "+lab);
		if (val instanceof Record) {
		    return ((Record) val).get(lab);
		} else {
		    _error("argument not record",val);
		}
	    }
	}
    }

    _BUILTIN(Print) {
	_APPLY(val) {
	    System.out.println(val);
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,print);
    static {
	Builtin.builtins.put("print",print);
	Builtin.builtins.put("TextIO.print",print);
    }

    _BUILTIN(Bi) {
	_APPLY(val) {
	    if (val instanceof STRING) {
		return Builtin.getBuiltin((STRING) val);
	    } else {
		_error("argument not string",val);
	    }
	}
    }
    _FIELD(General,bi);
    static {
	Builtin.builtins.put("builtin",bi);
	Builtin.builtins.put("General.builtin",bi);
    }
    // val exnName : exn -> string
    // val exnMessage : exn -> string
}
