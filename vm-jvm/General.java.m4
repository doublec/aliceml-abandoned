/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

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
	    DMLValue arg = args[0].request();
	    if (arg instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) arg;
		if (cv.getConstructor()==Constants.reference)
		    return cv.getContent();
	    }
	    return _error("wrong argument 1 for deref",val);
	}
    }
    /** <code>val ! : 'a ref -> 'a</code>*/
    _FIELD(deref);

    _BUILTIN(Assign) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"assign");
	    DMLValue car=args[0].request();
	    if (car instanceof DMLConVal) {
		return ((DMLConVal) car).assign(args[1]);
	    }
	    else
		return _error("wrong argument 1 for assign",val);
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    _FIELD(assign);

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
	    DMLValue f = args[0].request();
	    DMLValue g = args[1].request();
	    return new CO(f,g);
	}
    }
    /** <code>val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c </code>*/
    _FIELD(o);

    _BUILTIN(Before) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"before");
	    return args[0];
	}
    }
    /** <code>val before : ('a * unit) -> 'a</code>*/
    // wirft einfach das zweite Argument weg
    _FIELD(before);

    _BUILTIN(Ignore) {
	_APPLY(_) {
	    return Constants.dmlunit;
	}
    }
    /** <code>val ignore : 'a -> unit </code>*/
    _FIELD(ignore);

    _BUILTIN(Lvar) {
	_APPLY(_) {
	    return new LVar();
	}
    }
    /** <code>val lvar : _ -> lvar</code>*/
    _FIELD(lvar);

    /** Ref-Zellen-Konstruktor, entspricht etwa NewCell oder so.*/
    _BUILTIN(Ref) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"General.ref");
	    return new Reference(args[0]);
	}
    }
    /** <code>val ref : 'a -> ref 'a</code>*/
    _FIELD(ref);

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
    _FIELD(spawn);

    _BUILTIN(Equals) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"equals");
	    DMLValue car=args[0].request();
	    DMLValue cdr=args[1].request();
	    if (car.equals(cdr))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
    }
    /** <code>val equals : ('a * 'b) -> bool</code>*/
    _FIELD(equals);

    _BUILTIN(Pickle) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.pickle");
	    DMLValue fst=args[0].request();
	    if (!(fst instanceof de.uni_sb.ps.dml.runtime.String))
		return _error("argument 1 not de.uni_sb.ps.dml.runtime.String",val);
	    java.lang.String whereto=((de.uni_sb.ps.dml.runtime.String) fst).getString();
	    DMLValue ex=null;
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
		ex=Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String(e.getMessage()));
	    }
	    finally {
		try {
		    outf.close();
		} catch (Exception e) {
		    System.err.println(e);
		    e.printStackTrace();
		}
		if (ex != null)
		    ex.raise();
	    }
	    return Constants.dmlunit;
	}
    }

    _FIELD(pickle);

    _BUILTIN(Unpickle) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.unpickle");
	    DMLValue fst=args[0].request();
	    if (!(fst instanceof de.uni_sb.ps.dml.runtime.String))
		return _error("argument 1 not de.uni_sb.ps.dml.runtime.String",val);
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
		System.err.println(e);
		e.printStackTrace();
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

    _FIELD(unpickle);

    // val exnName : exn -> string 
    // val exnMessage : exn -> string
}
