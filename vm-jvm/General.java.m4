/*
 * $Date$
 * $Revision$
 * $Author$
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
	    _fromTuple(args,val,1,"deref");
	    _REQUESTDEC(DMLValue arg,args[0]);
	    if (arg instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) arg;
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
	    if (car instanceof DMLConVal) {
		return ((DMLConVal) car).assign(args[1]);
	    }
	    else
		_error("wrong argument 1 for assign",val);
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    _FIELD(General,assign);

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
	    _fromTuple(args,val,1,"General.ref");
	    return new Reference(args[0]);
	}
    }
    /** <code>val ref : 'a -> ref 'a</code>*/
    _FIELD(General,ref);

    _BUILTIN(Spawn) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"spawn");
	    de.uni_sb.ps.dml.runtime.Thread t=new de.uni_sb.ps.dml.runtime.Thread(args[0]);
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
	    if (!(fst instanceof STRING))
		_error("argument 1 not STRING ",val);
	    java.lang.String whereto=((STRING) fst).getString();
	    ExceptionWrapper ex=null;
	    java.io.FileOutputStream outf=null;
	    PickleOutputStream out=null;
	    try{
		outf=new java.io.FileOutputStream(whereto);
		out=new PickleOutputStream(outf);
		out.writeObject(args[1]);
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
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,pickle);

    _BUILTIN(Unpickle) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.unpickle");
	    _REQUESTDEC(DMLValue fst,args[0]);
	    if (!(fst instanceof STRING))
		_error("argument 1 not STRING ",val);
	    java.lang.String wherefrom=((STRING) fst).getString();
	    ExceptionWrapper ex=null;
	    java.io.FileInputStream inf=null;
	    PickleInputStream in=null;
	    DMLValue result = null;
	    try{
		inf=new java.io.FileInputStream(wherefrom);
		in=new PickleInputStream(inf);
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
	    _fromTuple(args,val,1,"General.~");
	    _REQUESTDEC(DMLValue number,args[0]);
	    if (number instanceof Int) {
		return new Int(-((Int) number).getInt());
	    } else if (number instanceof Word) {
		return new Word(-((Word) number).getLong());
	    } else if (number instanceof Real) {
		return new Real(-((Real) number).getFloat());
	    } else {
		_error("argument not Number",val);
	    }
	}
    }
    _FIELD(General,uminus);

    _BINOP(plus,+);
    _BINOP(minus,-);
    _BINOP(div,/);
    _BINOP(mod,%);
    _BINOP(mult,*);
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
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    _FIELD(General,neq);

    _BUILTIN(Sel) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"General.sel");
	    _REQUESTDEC(DMLValue sel,args[0]);
	    if (sel instanceof Int) {
		return new SelFunInt(((Int) sel).value);
	    } else if (sel instanceof STRING) {
		return new SelFunString(((STRING) sel).value);
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
		_fromTuple(args,val,1,"General.sel "+i);
		_REQUESTDEC(DMLValue tup, args[0]);
		if (tup instanceof DMLTuple) {
		    switch (i) {
		    case 1: return ((DMLTuple) tup).get0();
		    case 2: return ((DMLTuple) tup).get1();
		    case 3: return ((DMLTuple) tup).get2();
		    case 4: return ((DMLTuple) tup).get3();
		    case 5: return ((DMLTuple) tup).get4();
		    default: return ((DMLTuple) tup).get(i);
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
		_fromTuple(args,val,1,"General.sel "+lab);
		_REQUESTDEC(DMLValue tup, args[0]);
		if (tup instanceof Record) {
		    return ((Record) tup).get(lab);
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
    }
    // val exnName : exn -> string 
    // val exnMessage : exn -> string
}
