package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class General {
    /** <code>datatype order = LESS | EQUAL | GREATER</code>*/
    final public static Name LESS = new Name("order.LESS");
    final public static Name EQUAL = new Name("order.EQUAL");
    final public static Name GREATER = new Name("order.GREATER");
    /** <code>exception Bind</code>*/
    final public static Name Bind = new Name("General.Bind");
    /** <code>exception Chr</code>*/
    final public static Name Chr = new Name("General.Chr");
    /** <code>exception Div</code>*/
    final public static Name Div = new Name("General.Div");
    /** <code>exception Domain</code>*/
    final public static Name Domain = new Name("General.Domain");
    /** <code>exception Fail of string </code>*/
    final public static Constructor Fail = new Constructor("General.Fail");
    /** <code>exception Match</code>*/
    final public static Name Match = new Name("General.Match");
    /** <code>exception Overflow</code>*/
    final public static Name Overflow = new Name("General.Overflow");
    /** <code>exception Size</code>*/
    final public static Name Size = new Name("General.Size");
    /** <code>exception Span</code>*/
    final public static Name Span = new Name("General.Span");
    /** <code>exception Subscript</code>*/
    final public static Name Subscript = new Name("General.Subscript");

    final public static class Deref extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args = fromTuple(val,1,"deref");
	    DMLValue arg = args[0].request();
	    if (arg instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) arg;
		if (cv.getConstructor()==Constants.reference)
		    return cv.getContent();
	    }
	    return error("wrong argument #1 for deref",val);
	}
    }
    /** <code>val ! : 'a ref -> 'a</code>*/
    final public static Deref deref  = new Deref();

    final public static class Assign extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args = fromTuple(val,2,"assign");
	    DMLValue car=args[0].request();
	    if (car instanceof DMLConVal) {
		return ((DMLConVal) car).assign(args[1]);
	    }
	    else
		return error("wrong argument #1 for assign",val);
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    final public static Assign assign = new Assign();

    final public static class Compose extends Builtin {
	final public class Composer extends Builtin {
	    public DMLValue f,g;
	    public Composer(DMLValue f, DMLValue g) {
		this.f=f;
		this.g=g;
	    }
	    final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException{
		return f.apply(g.apply(v));
	    }
	}
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args = fromTuple(val,2,"assign");
	    DMLValue f = args[0].request();
	    DMLValue g = args[1].request();
	    return new Composer(f,g);
	}
    }
    /** <code>val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c </code>*/
    final public static Compose o = new Compose();

    final public static class Before extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args  = fromTuple(val,2,"before");
	    return args[0];
	}
    }
    /** <code>val before : ('a * unit) -> 'a</code>*/
    // wirft einfach das zweite Argument weg
    final public static Before before = new Before();

    final public static class Ignore extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    return Constants.dmlunit;
	}
    }
    /** <code>val ignore : 'a -> unit </code>*/
    final public static Ignore ignore = new Ignore();

    final public static class LVar extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    return new LVar();
	}
    }
    /** <code>val lvar : _ -> lvar</code>*/
    final public static LVar lvar = new LVar();

    /** Ref-Zellen-Konstruktor, entspricht etwa NewCell oder so.*/
    final public static class Ref extends Builtin {
	final synchronized public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args = fromTuple(val,1,"General.ref");
	    return new Reference(args[0]);
	}
    }
    /** <code>val ref : 'a -> ref 'a</code>*/
    final public static Ref ref = new Ref();

    final public static class Spawn extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"spawn");
	    de.uni_sb.ps.dml.runtime.Thread t=new de.uni_sb.ps.dml.runtime.Thread(args[0]);
	    t.start();
	    return Constants.dmlunit;
	}
    }
    /** <code>val spawn : (_ -> 'a) -> unit</code>
     *  spawn startet einen neuen de.uni_sb.ps.dml.runtime.Thread, der das Argument
     *  von <code>apply</code> appliziert.
     */
    final public static Spawn spawn = new Spawn();

    final public static class Equals extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"equals");
	    DMLValue car=args[0].request();
	    DMLValue cdr=args[1].request();
	    if (car.equals(cdr))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
    }
    /** <code>val equals : ('a * 'b) -> bool</code>*/
    final public static Equals equals = new Equals();

    final public static class Pickle extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"General.pickle");
	    DMLValue fst=args[0].request();
	    if (!(fst instanceof de.uni_sb.ps.dml.runtime.String))
		return error("argument #1 not de.uni_sb.ps.dml.runtime.String",val);
	    java.lang.String whereto=((de.uni_sb.ps.dml.runtime.String) fst).getString();
	    DMLValue ex=null;
	    java.io.FileOutputStream outf=null;
	    PickleOutputStream out=null;
	    try{
		outf=new java.io.FileOutputStream(whereto);
		out=new PickleOutputStream(outf);
		out.flush();
		out.writeObject(args[1]);
	    } catch (Exception e) {
		ex=Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String(e.getMessage()));
	    }
	    finally {
		try {
		    outf.close();
		} catch (Exception e) {
		System.err.println(e);}
		if (ex != null)
		    ex.raise();
	    }
	    return Constants.dmlunit;
	}
    }

    final public static Pickle pickle = new Pickle();

    final public static class Unpickle extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"General.unpickle");
	    DMLValue fst=args[0].request();
	    if (!(fst instanceof de.uni_sb.ps.dml.runtime.String))
		return error("argument #1 not de.uni_sb.ps.dml.runtime.String",val);
	    java.lang.String wherefrom=((de.uni_sb.ps.dml.runtime.String) fst).getString();
	    DMLValue ex=null;
	    java.io.FileInputStream inf=null;
	    PickleInputStream in=null;
	    DMLValue result = null;
	    try{
		inf=new java.io.FileInputStream(wherefrom);
		in=new PickleInputStream(inf);
		result=(DMLValue) in.readObject();
	    } catch (Exception e) {
		ex=Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String(e.getMessage()));
	    }
	    finally {
		try {
		    inf.close();
		} catch (Exception e) {
		System.err.println(e);}
		if (ex != null)
		    ex.raise();
	    }
	    return result;
	}
    }

    final public static Unpickle unpickle = new Unpickle();

    // val exnName : exn -> string 
    // val exnMessage : exn -> string

    // Hilfsfunktionen
    final public static DMLValue[] fromTuple
	(DMLValue v, // Value-Tuple
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) throws java.rmi.RemoteException {
	v=v.request();
	if (v instanceof DMLTuple) {
	    DMLTuple t=(DMLTuple) v;
	    if (t.getArity()==ea) {
		DMLValue[] vals = new DMLValue[ea];
		for(int i=0; i<ea; i++)
		    vals[i]=t.getByIndex(i);
		return vals;
	    }
	    else
		error("wrong number of arguments in "+errMsg, v);
	}
	else
	    error("wrong argument type for "+errMsg,v);
	return null;
    }

    final public static DMLValue error
	(java.lang.String msg, DMLValue v) throws java.rmi.RemoteException {
	// sonst: Fehler
	DMLValue[] err = {
	    new de.uni_sb.ps.dml.runtime.String(msg),
	    v};
	return Constants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
