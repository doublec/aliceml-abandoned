using System;

namespace Alice {
    class Prebound {
	public static Object unit              = null;
	public static Object Transient_Future  = "Transient.Future";
	public static Object General_Chr       = "General.Chr";
	public static Object General_Div       = "General.Div";
	public static Object General_Domain    = "General.Domain";
	public static Object General_Fail      = "General.Fail";
	public static Object General_Overlow   = "General.Overflow";
	public static Object General_Size      = "General.Size";
	public static Object General_Span      = "General.Span";
	public static Object General_Subscript = "General.Subscript";
    }
    class Cell {
	Object Value;
	public void Assign(Object value) {
	    Value = value;
	}
	public Object Access() {
	    return Value;
	}
    }
    class Future {
	private Object Ref;
	Future() {
	    Ref = null;
	}
	public virtual Object Await() {
	    lock (this) {
		if (Ref == null) {
		    Wait();
		}
	    }
	    return Ref;
	}
	public virtual Object Bind(Object o) {
	    lock (this) {
		if (Ref == null) {
		    Ref = o;
		}
		else {
		    throw new Exception(Prebound.Transient_Future);
		}
	    }
	    return Prebound.unit;
	}
    }
    class CommonOp {
	public static Object Sync(Object obj) {
	    if (obj is Future) {
		return ((Future) obj).Await();
	    }
	    else {
		return obj;
	    }
	}
	public static Int32 BtI(bool v) {
	    if (v == true) {
		return (Int32) 1;
	    }
	    else {
		return (Int32) 0;
	    }
	}
   }
    class TagVal {
	int Tag;
	Object Value;
	public TagVal(int tag, Object value) {
	    Tag   = tag;
	    Value = value;
	}
	public int GetTag() {
	    return Tag;
	}
	public Object GetValue() {
	    return Value;
	}
    }
    class ConVal {
	Object Id;
	Object Value;
	public ConVal(Object id, Object v) {
	    Id    = id;
	    Value = v;
	}
	public Object GetId() {
	    return Id;
	}
	public Object GetValue() {
	    return Value;
	}
    }
    class ListOp {
	public static bool IsNil(Object obj) {
	    return (obj is Int32);
	}
	public static bool IsCons(Object obj) {
	    return (obj is TagVal);
	}
	public static Object Car(Object obj) {
	    return ((Object[]) ((TagVal) obj).GetValue())[0];
	}
	public static Object Cdr(Object obj) {
	    return ((Object[]) ((TagVal) obj).GetValue())[1];
	}
	public static Object Cons(Object car, Object cdr) {
	    Object[] a = new Object[2];
	    
	    a[0] = car;
	    a[1] = cdr;
	    return new TagVal(1, a);
	}
	public static int Length(Object obj) {
	    int n = 0;
	    
	    while (!IsNil(obj)) {
		obj = CommonOp.Sync(((Object[]) ((TagVal) obj).GetValue())[1]);
		n++;
	    }
	    return n;
	}
    }
    class Exception : SystemException {
	public Object Value;
	public Exception(Object obj) {
	    Value = obj;
	}
    }
    abstract class Procedure0 {
	public abstract Object Apply();
	public virtual Object Apply(Object obj) {
	    obj = CommonOp.Sync(obj);
	    return Apply();
	}
    }
    abstract class Procedure1 {
	public abstract Object Apply(Object a);
    }
    abstract class Procedure2 {
	public abstract Object Apply(Object a, Object b);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1]);
	}
    }
    abstract class Procedure3 {
	public abstract Object Apply(Object a, Object b, Object c);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2]);
	}
    }
    abstract class Procedure4 {
	public abstract Object Apply(Object a, Object b, Object c, Object d);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3]);
	}
    }
    abstract class Procedure5 {
	public abstract Object Apply(Object a, Object b, Object c, Object d, Object e);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4]);
	}
    }
    abstract class Procedure6 {
	public abstract Object Apply(Object a, Object b, Object c, Object d, Object e, Object f);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5]);
	}
    }
    abstract class Procedure7 {
	public abstract Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
	}
    }
    abstract class Procedure8 {
	public abstract Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
	}
    }
    abstract class Procedure9 {
	public abstract Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i);
	public virtual Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
	}
    }
    class Array {
	public class array : Procedure2 {
	    public static Object StaticApply(Object n, Object x) {
		x = CommonOp.Sync(n);
		int elems  = (Int32) CommonOp.Sync(n);
		Object[] a = new Object[elems];
		
		for (int i = 0; i < elems; i++) {
		    a[i] = x;
		}
		return a;
	    }
	    public override Object Apply(Object n, Object x) {
		return StaticApply(n, x);
	    }
	}
	public class fromList : Procedure1 {
	    public static Object StaticApply(Object x) {
		x = CommonOp.Sync(x);
		int n      = ListOp.Length(x);
		Object[] a = new Object[n];

		for (int i = 0; i < n; i++) {
		    a[i] = ListOp.Car(x);
		    x    = ListOp.Cdr(x);
		}
		
		return a;
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class length : Procedure1 {
	    public static Object StaticApply(Object x) {
		return ((Object[]) CommonOp.Sync(x)).GetLength();
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class sub : Procedure2 {
	    public static Object StaticApply(Object a, Object i) {
		return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(i)];
	    }
	    public override Object Apply(Object a, Object i) {
		return StaticApply(a, i);
	    }
	}
	public class update : Procedure3 {
	    public static Object StaticApply(Object a, Object i, Object x) {
		((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(i)] = x;
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object i, Object x) {
		return StaticApply(a, i, x);
	    }
	}
    }
    class Char {
	public class less : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) < ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class greater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) > ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class lessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) <= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class greaterThan : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) >= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class ord : Procedure1 {
	    public static Object StaticApply(Object a) {
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class chr : Procedure1 {
	    public static Object StaticApply(Object a) {
		System.Char ca = (System.Char) CommonOp.Sync(a);

		if (System.Char.IsLetterOrDigit(ca) || System.Char.IsISOControl(ca)) {
		    return ca;
		}
		else {
		    // to be determined
		    throw new Exception(ca);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isAlpha : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetter(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isAlphaNum : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetterOrDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isDigit : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isGraph : Procedure1 {
	    public static Object StaticApply(Object a) {
		System.Char ca = (System.Char) CommonOp.Sync(a);
		return CommonOp.BtI((System.Char.IsPrintable(ca)
				     && (!System.Char.IsWhiteSpace(ca))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isHexDigit : Procedure1 {
	    public static Object StaticApply(Object a) {
		// to be determined
		return CommonOp.BtI(false);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isLower : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLower(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isPrint : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPrintable(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isPunct : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPunctuation(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isSpace : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsWhiteSpace(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isUpper : Procedure1 {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsUpper(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toLower : Procedure1 {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToLower(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toUpper : Procedure1 {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToUpper(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    class General {
	public class assign : Procedure2 {
	    public static Object StaticApply(Object c, Object v) {
		((Cell) CommonOp.Sync(v)).Assign(v);
		return Prebound.unit;
	    }
	    public override Object Apply(Object c, Object v) {
		return StaticApply(c, v);
	    }
	}
	public class exchange : Procedure2 {
	    public static Object StaticApply(Object c, Object nv) {
		Cell cl = (Cell) CommonOp.Sync(c);

		c = cl.Access();
		cl.Assign(nv);
		return c;
	    }
	    public override Object Apply(Object c, Object nw) {
		return StaticApply(c, nw);
	    }
	}
	public class exnName : Procedure1 {
	    public static Object StaticApply(Object v) {
		v = CommonOp.Sync(v);
		if (v is ConVal) {
		    v = ((ConVal) v).GetValue();
		}
		if (v is String) {
		    return v;
		}
		else if (v is Guid) {
		    return (String) "";
		}
		else {
		    // to be determined
		    throw new Exception(v);
		}
	    }
	    public override Object Apply(Object v) {
		return StaticApply(v);
	    }
	}
    }
}
