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
    UNAME(LESS,order.LESS);
    UNAME(EQUAL,order.EQUAL);
    UNAME(GREATER,order.GREATER);
    /** <code>exception Bind</code>*/
    UNAME(Bind,General.Bind);
    /** <code>exception Chr</code>*/
    UNAME(Chr,General.Chr);
    /** <code>exception Div</code>*/
    UNAME(Div,General.Div);
    /** <code>exception Domain</code>*/
    UNAME(Domain,General.Domain);
    /** <code>exception Fail of string </code>*/
    UCONS(Fail,General.Fail);
    /** <code>exception Match</code>*/
    UNAME(Match,General.Match);
    /** <code>exception Overflow</code>*/
    UNAME(Overflow,General.Overflow);
    /** <code>exception Size</code>*/
    UNAME(Size,General.Size);
    /** <code>exception Span</code>*/
    UNAME(Span,General.Span);
    /** <code>exception Subscript</code>*/
    UNAME(Subscript,General.Subscript);

    _BUILTIN(Deref) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"deref");
	    if (val instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) val;
		if (cv.getConstructor()==Constants.reference)
		    return cv.getContent();
	    }
	    _RAISENAME(General.Match);
	}
    }
    /** <code>val ! : 'a ref -> 'a</code>*/
    _FIELD(General,deref);

    _BUILTIN(Assign) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"assign");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue car,v1);
	    try {
		return ((Reference) car).assign(v2);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    _FIELD(General,assign);
    static {
	Builtin.builtins.put(":=",assign);
    }

    _BUILTIN(Exchange) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"exchange");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue car,v1);
	    try {
		return ((Reference) car).exchange(v2);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val exchange : ('a ref * 'a) -> 'a</code>*/
    _FIELD(General,exchange);

    _BUILTIN(Bind) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"General.bind");
	}
	_SAPPLY2(v) {
	    if (v1 instanceof DMLTransient) {
		return ((DMLTransient) v1).bind(v2);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val bind : (lvar * 'a) -> unit</code>*/
    _FIELD(General,bind);

    _BUILTIN(O) {
	final public static class CO extends Builtin {
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    public DMLValue f,g;
	    public CO(DMLValue f, DMLValue g) {
		this.f=f;
		this.g=g;
	    }
	    _APPLY(v) {
		return f.apply(g.apply(v));
	    }
	}
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"compose");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue f,v1);
	    _REQUESTDEC(DMLValue g,v2);
	    return new CO(f,g);
	}
    }
    /** <code>val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c </code>*/
    _FIELD(General,o);

    _BUILTIN(Before) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"before");
	}
	_SAPPLY2(v) {
	    return v1;
	}
    }
    /** <code>val before : ('a * unit) -> 'a</code>*/
    // wirft einfach das zweite Argument weg
    _FIELD(General,before);

    _BUILTIN(Ignore) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    return Constants.dmlunit;
	}
    }
    /** <code>val ignore : 'a -> unit </code>*/
    _FIELD(General,ignore);

    _BUILTIN(ByNeed) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(fun) {
	    return new ByNeedFuture(fun);
	}
    }
    /** <code>val byNeed : (unit -> 'a) -> unit</code>*/
    _FIELD(General,byNeed);

    _BUILTIN(Future) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(lvar) {
	    return new de.uni_sb.ps.dml.runtime.Future(lvar);
	}
    }
    /** <code>val future : 'a -> 'a</code>*/
    _FIELD(General,future);

    _BUILTIN(Wait) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    if (val instanceof DMLTransient) {
		return ((DMLTransient) val).request();
	    } else {
		return val;
	    }
	}
    }
    /** <code> val wait : lvar -> 'a</code>*/
    _FIELD(General,wait);

    _BUILTIN(Lvar) {
	final public DMLValue apply0() throws java.rmi.RemoteException {
	    return new LVar();
	}
	_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    return new LVar();
	}
    }
    /** <code>val lvar : _ -> lvar</code>*/
    _FIELD(General,lvar);

    /** Ref-Zellen-Konstruktor, entspricht etwa NewCell oder so.*/
    _BUILTIN(Ref) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
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
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
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
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"equals");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue car,v1);
	    _REQUESTDEC(DMLValue cdr,v2);
	    if (car.equals(cdr))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
    }
    /** <code>val equals : ('a * 'b) -> bool</code>*/
    _FIELD(General,equals);
    static {
	Builtin.builtins.put("=",equals);
    }
    _BUILTIN(Pickle) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"General.pickle");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue fst,v1);
	    if (!(fst instanceof STRING)) {
		_RAISENAME(General.Match);
	    }
	    java.lang.String whereto=((STRING) fst).value;
	    return apply(whereto,v2);
	}

	final public static DMLValue apply(java.lang.String name,
					   DMLValue val2)
	    throws java.rmi.RemoteException {
	    java.util.zip.GZIPOutputStream zip = null;
	    ExceptionWrapper ex=null;
	    java.io.FileOutputStream outf=null;
	    PickleOutputStream out=null;
	    try{
		outf=new java.io.FileOutputStream(name);
		zip = new java.util.zip.GZIPOutputStream(outf);
		out=new PickleOutputStream(zip);
		out.writeObject(val2);
		outf.flush();
		out.flush();
		zip.close();
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
	    // System.out.println("Pickled "+out.objectcounter+" objects.");
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,pickle);

    _BUILTIN(Unpickle) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _REQUESTDEC(DMLValue fst,val);
	    if (!(fst instanceof STRING)) {
		_RAISENAME(General.Match);
	    }
	    java.lang.String wherefrom=((STRING) fst).value;
	    ExceptionWrapper ex=null;
	    java.util.zip.GZIPInputStream zip = null;
	    java.io.FileInputStream inf=null;
	    PickleInputStream in=null;
	    DMLValue result = null;
	    try{
		inf = new java.io.FileInputStream(wherefrom);
		zip = new java.util.zip.GZIPInputStream(inf);
		in=new PickleInputStream(zip);
		result=(DMLValue) in.readObject();
	    } catch (Exception e) {
		System.err.println(e);
		e.printStackTrace();
		ex = new ExceptionWrapper(Constants.runtimeError.apply(new STRING (e.getMessage())));
	    }
	    finally {
		try {
		    inf.close();
		    zip.close();
		} catch (Exception e) {
		    System.err.println(e);}
		if (ex != null)
		    throw ex;
	    }
	    return result;
	}
    }
    _FIELD(General,unpickle);
    static {
	Builtin.builtins.put("Komponist.import",unpickle);
    }

    _BUILTIN(Uminus) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"General.~");
	    if (val instanceof Int) {
		return new Int(-((Int) val).value);
	    } else if (val instanceof Word) {
		return new Word(-((Word) val).value);
	    } else if (val instanceof Real) {
		return new Real(-((Real) val).value);
	    } else {
		_RAISENAME(General.Match);
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
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _sfromTuple(args,val,2,"General.<>");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue l, v1);
	    _REQUESTDEC(DMLValue r, v2);
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
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"General.sel");
	    if (val instanceof Int) {
		return new SelFunInt(((Int) val).value);
	    } else if (val instanceof STRING) {
		return new SelFunString(((STRING) val).value);
	    } else {
		_RAISENAME(General.Match);
	    }
	}
	_BUILTIN(SelFunInt) {
	    int i = -1;
	    public SelFunInt(int idx) {
		i = idx - 1;
	    }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"General.sel "+i);
		_REQUEST(val,val);
		if (val instanceof DMLTuple) {
		    switch (i) {
		    case 0: return ((DMLTuple) val).get0();
		    case 1: return ((DMLTuple) val).get1();
		    case 2: return ((DMLTuple) val).get2();
		    case 3: return ((DMLTuple) val).get3();
		    default: return ((DMLTuple) val).get(i);
		    }
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
	_BUILTIN(SelFunString) {
	    java.lang.String lab = null;
	    public SelFunString(java.lang.String str) {
		lab = str;
	    }
	    _NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	    _APPLY(val) {
		// _FROMSINGLE(val,"General.sel "+lab);
		if (val instanceof Record) {
		    return ((Record) val).get(lab);
		} else {
		    _RAISENAME(General.Match);
		}
	    }
	}
    }

    _BUILTIN(Show) {
	_BUILTTUP;
	_APPLY(val) {
	    System.out.print(val.toString(10));
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,show);
    static {
	Builtin.builtins.put("show",show);
    }

    _BUILTIN(StringHash) {
	_BUILTTUP;
	_APPLY(val) {
	    try {
		return new Int(java.lang.Math.
			       abs(((STRING) val).value.hashCode()));
	    } catch (ClassCastException c) {
		return new Int(java.lang.Math.
			       abs(val.hashCode()));
	    }
	}
    }
    _FIELD(General,stringHash);

    _BUILTIN(Print) {
	_BUILTTUP;
	_APPLY(val) {
	    System.out.print(val);
	    return Constants.dmlunit;
	}
    }
    _FIELD(General,print);
    static {
	Builtin.builtins.put("print",print);
	Builtin.builtins.put("TextIO.print",print);
    }

    _BUILTIN(Bi) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    if (val instanceof STRING) {
		return Builtin.getBuiltin((STRING) val);
	    } else {
		_RAISENAME(General.Match);
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

    _BUILTIN(Terminate) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    _REQUEST(_,_);
	    try {
		int i = ((Int) _).value;
		System.exit(i);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	    return Constants.dmlunit;
	}
    }
    /** <code>val terminate : int -> unit</code>*/
    _FIELD(General,terminate);

    _BUILTIN(Readln) {
	final public DMLValue apply0() throws java.rmi.RemoteException {
	    java.lang.String s="";
	    char ch;
	    try {
		while ("\n".indexOf((ch = (char) System.in.read())) == -1) {
		    s+=ch;
		}
	    } catch (java.io.IOException i) {
		System.err.println(i);
	    }
	    return new STRING (s);
	}
	_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    java.lang.String s="";
	    char ch;
	    try {
		while ("\n".indexOf((ch = (char) System.in.read())) == -1) {
		    s+=ch;
		}
	    } catch (java.io.IOException i) {
		System.err.println(i);
	    }
	    return new STRING (s);
	}
    }
    /** <code>val readln : unit -> string</code>*/
    _FIELD(General,readln);

    _BUILTIN(Read) {
	final public DMLValue apply0() throws java.rmi.RemoteException {
	    char ch = '0';
	    try {
		ch = (char) System.in.read();
	    } catch (java.io.IOException i) {
		System.err.println(i);
	    }
	    return new STRING (java.lang.String.valueOf(ch));
	}
	_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    char ch = '0';
	    try {
		ch = (char) System.in.read();
	    } catch (java.io.IOException i) {
		System.err.println(i);
	    }
	    return new STRING (java.lang.String.valueOf(ch));
	}
    }
    /** <code>val read : unit -> string</code>*/
    _FIELD(General,read);

    _BUILTIN(Rand) {
	final static java.util.Random rand =
	    new java.util.Random((new java.util.Date()).getTime());
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(_) {
	    _REQUEST(_,_);
	    int max = 0;
	    try {
		max = ((Int) _).value;
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	    return new Int(rand.nextInt(max));
	}
    }
    /** <code>val read : unit -> string</code>*/
    _FIELD(General,rand);
}
