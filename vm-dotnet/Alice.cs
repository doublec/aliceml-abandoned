using System;
using System.Threading;
using System.IO;

namespace Alice {
    public class AliceArray {
	public Object[] Value;
	public AliceArray(Object[] Value) {
	    this.Value = Value;
	}
    }
    public abstract class Procedure {
	public abstract Object Apply(Object a);
	public virtual Object Apply() {
	    return Apply(null);
	}
	public virtual Object Apply(Object a, Object b) {
	    Object[] ar = new Object[2];
	    ar[0] = a;
	    ar[1] = b;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c) {
	    Object[] ar = new Object[3];
	    ar[0] = a; ar[1] = b; ar[2] = c;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d) {
	    Object[] ar = new Object[4];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d, Object e) {
	    Object[] ar = new Object[5];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f) {
	    Object[] ar = new Object[6];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
	    Object[] ar = new Object[7];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f; ar[6] = g;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
	    Object[] ar = new Object[8];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f; ar[6] = g; ar[7] = h;
	    return Apply(a);
	}
	public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
	    Object[] ar = new Object[9];
	    ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f;
	    ar[6] = g; ar[7] = h; ar[8] = i;
	    return Apply(a);
	}
	public override bool Equals(Object a) {
	    return (this == (Procedure) a);
	}
    }
    public abstract class Procedure0 : Procedure {
	public override Object Apply(Object obj) {
	    obj = CommonOp.Sync(obj);
	    return Apply();
	}
    }
    public abstract class Procedure2 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1]);
	}
    }
    public abstract class Procedure3 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2]);
	}
    }
    public abstract class Procedure4 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3]);
	}
    }
    public abstract class Procedure5 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4]);
	}
    }
    public abstract class Procedure6 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5]);
	}
    }
    public abstract class Procedure7 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
	}
    }
    public abstract class Procedure8 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
	}
    }
    public abstract class Procedure9 : Procedure {
	public override Object Apply(Object obj) {
	    Object[] a = (Object[]) CommonOp.Sync(obj);
	    return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
	}
    }
    class StreamWrapper {
	public System.String name;
	public Object stream;
	public StreamWrapper(System.String name, Object stream) {
	    this.name   = name;
	    this.stream = stream;
	}
    }
    public class Cell {
	Object Value;
	public Cell() {}
	public Cell(Object value) {
	    Value = value;
	}
	public void Assign(Object value) {
	    Value = value;
	}
	public Object Access() {
	    return Value;
	}
	public Object Exchange(Object value) {
	    lock (this) {
		Object oldval = Value;
		
		Value = value;
		return oldval;
	    }
	}
    }
    public class AliceFuture : Procedure {
	private Object Ref;
	AliceFuture() {
	    Ref = null;
	}
	public override Object Apply(Object obj) {
	    return ((Procedure) Await()).Apply(obj);
	}
	public override object Apply(Object a, Object b) {
	    return ((Procedure) Await()).Apply(a, b);
	}
	public override object Apply(Object a, Object b, Object c) {
	    return ((Procedure) Await()).Apply(a, b, c);
	}
	public override object Apply(Object a, Object b, Object c, Object d) {
	    return ((Procedure) Await()).Apply(a, b, c, d);
	}
	public override object Apply(Object a, Object b, Object c, Object d, Object e) {
	    return ((Procedure) Await()).Apply(a, b, c, d, e);
	}
	public override object Apply(Object a, Object b, Object c, Object d, Object e, Object f) {
	    return ((Procedure) Await()).Apply(a, b, c, d, e, f);
	}
	public override object Apply(Object a, Object b, Object c, Object d, Object e,
				     Object f, Object g) {
	    return ((Procedure) Await()).Apply(a, b, c, d, e, f, g);
	}
	public override object Apply(Object a, Object b, Object c, Object d, Object e,
				     Object f, Object g, Object h) {
	    return ((Procedure) Await()).Apply(a, b, c, d, e, f, g, h);
	}
	public override object Apply(Object a, Object b, Object c, Object d, Object e,
				     Object f, Object g, Object h, Object i) {
	    return ((Procedure) Await()).Apply(a, b, c, d, e, f, g, h, i);
	}
	public virtual Object Await() {
	    //  	    lock (this) {
	    //  		if (Ref == null) {
	    //  		    Wait();
	    //  		}
	    //  	    }
	    return Ref;
	}
	public virtual Object Bind(Object o) {
	    lock (this) {
		if (Ref == null) {
		    Ref = o;
		}
		else {
		    throw new Exception(Prebound.Future.future);
		}
	    }
	    return Prebound.unit;
	}
    }
    public class TagVal {
	int Tag;
	public Object Value;
	public TagVal(int tag) {
	    Tag = tag;
	}
	public TagVal(int tag, Object value) {
	    Tag   = tag;
	    Value = value;
	}
	public int GetTag() {
	    return Tag;
	}
	public override bool Equals(Object a) {

	    if (a is TagVal) {
		TagVal ac = (TagVal) a;
		return ((Tag == ac.GetTag()) && (Value.Equals(ac.Value)));
	    }
	    else {
		return false;
	    }
	}
    }
    public class ConVal {
	Object Id;
	public Object Value;
	public ConVal(Object id) {
	    Id = CommonOp.Sync(id);
	}
	public ConVal(Object id, Object v) {
	    Id    = CommonOp.Sync(id);
	    Value = v;
	}
	public Object GetId() {
	    return Id;
	}
	public override bool Equals(Object a) {
	    if (a is ConVal) {
		ConVal ac = (ConVal) a;
		return (Id.Equals(ac.GetId()) && (Value.Equals(ac.Value)));
	    }
	    else {
		return false;
	    }
	}
    }
    class Ref : Procedure {
	public override Object Apply(Object obj) {
	    return new Alice.Cell(obj);
	}
    }
    public class Exception : SystemException {
	public Object Value;
	public Exception(Object obj) {
	    Value = obj;
	}
    }
    public class TagConstructor : Procedure {
	int Tag;
	public TagConstructor(int i) {
	    Tag = i;
	}
	public override Object Apply(Object x) {
	    return new TagVal(Tag, x);
	}
    }
    public class ConConstructor : Procedure {
	Object Id;
	public ConConstructor(Object id) {
	    Id = id;
	}
	public override Object Apply(Object x) {
	    return new ConVal(Id, x);
	}
    }
    public class Selector : Procedure {
	int Index;
	public Selector(int i) {
	    Index = i;
	}
	public override Object Apply(Object x) {
	    return ((Object[]) CommonOp.Sync(x))[Index];
	}
    }
    class CommonOp {
	public static Object Sync(Object obj) {
	    if (obj is AliceFuture) {
		return ((AliceFuture) obj).Await();
	    }
	    else {
		return obj;
	    }
	}
	public static Int32 BtI(bool v) {
	    if (v) {
		return (Int32) 1;
	    }
	    else {
		return (Int32) 0;
	    }
	}
	public static bool ItB(Object v) {
	    if ((Int32) v == (Int32) 1) {
		return true;
	    }
	    else {
		return false;
	    }
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
	    return ((Object[]) ((TagVal) obj).Value)[0];
	}
	public static Object Cdr(Object obj) {
	    return ((Object[]) ((TagVal) obj).Value)[1];
	}
	public static TagVal Cons(Object car, Object cdr) {
	    Object[] a = new Object[2];
	    
	    a[0] = car;
	    a[1] = cdr;
	    return new TagVal(0, a);
	}
	public static int Length(Object obj) {
	    int n = 0;
	    // Precondition: Object is not future
	    while (!IsNil(obj)) {
		TagVal tval   = (TagVal) obj;
		Object[] cons = (Object[]) tval.Value;

		obj     = CommonOp.Sync(cons[1]);
		cons[1] = obj; // Path Compression
		n++;
	    }
	    return n;
	}
    }
    public class opeq : Procedure2 {
	public static bool BothConVal(Object a, Object b) {
	    return ((a is Alice.ConVal) && (b is Alice.ConVal));
	}
	public static bool BothTagVal(Object a, Object b) {
	    return ((a is Alice.TagVal) && (b is Alice.TagVal));
	}
	public static Object StaticApply(Object a, Object b) {
	    a = CommonOp.Sync(a);
	    b = CommonOp.Sync(b);

	    if (a is Alice.AliceArray) {
		return CommonOp.BtI(a == b);
	    }
	    else if (a is Alice.Cell) {
		return CommonOp.BtI(a == b);
	    }
	    else if (BothConVal(a, b)) {
		ConVal at = (ConVal) a;
		ConVal bt = (ConVal) b;
		return CommonOp.BtI((at.GetId() == bt.GetId()) &&
				    CommonOp.ItB(StaticApply(at.Value, bt.Value)));
	    }
	    else if (BothTagVal(a, b)) {
		TagVal at = (TagVal) a;
		TagVal bt = (TagVal) b;
		
		return CommonOp.BtI((at.GetTag() == bt.GetTag()) &&
				    CommonOp.ItB(StaticApply(at.Value, bt.Value)));
	    }
	    else if (a is System.Array) {
		Object[] ar = (Object[]) a;
		Object[] br = (Object[]) b;
		int al      = ar.Length;

		for (int i = 0; i < al; i++) {
		    if (!CommonOp.ItB(StaticApply(ar[i], br[i]))) {
			return (Int32) 0;
		    }
		}
		return (Int32) 1;
	    }
	    else if (a == null) {
		return (Int32) 1;
	    }
	    else {
		return CommonOp.BtI(a.Equals(b));
	    }
	}
	public override Object Apply(Object a, Object b) {
	    return StaticApply(a, b);
	}
    }
    public class opnoteq : Procedure2 {
	public static Object StaticApply(Object a, Object b) {
	    if ((int) opeq.StaticApply(a, b) == 0) {
		return (Int32) 1;
	    }
	    else {
		return (Int32) 0;
	    }
	}
	public override Object Apply(Object a, Object b) {
	    return StaticApply(a, b);
	}
    }
    public class Array {
	public class array : Procedure2 {
	    public static Object StaticApply(Object n, Object x) {
		int elems  = (Int32) CommonOp.Sync(n);
		Object[] a = new Object[elems];
		
		for (int i = 0; i < elems; i++) {
		    a[i] = x;
		}
		return new AliceArray(a);
	    }
	    public override Object Apply(Object n, Object x) {
		return StaticApply(n, x);
	    }
	}
	public class fromList : Procedure {
	    public static Object StaticApply(Object x) {
		x = CommonOp.Sync(x);
		int n      = ListOp.Length(x);
		Object[] a = new Object[n];

		for (int i = 0; i < n; i++) {
		    a[i] = ListOp.Car(x);
		    x    = ListOp.Cdr(x);
		}
		
		return new AliceArray(a);
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class length : Procedure {
	    public static Object StaticApply(Object x) {
		return ((AliceArray) CommonOp.Sync(x)).Value.Length;
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class sub : Procedure2 {
	    public static Object StaticApply(Object a, Object i) {
		try {
		    return (((AliceArray) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(i)];
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
	    }
	    public override Object Apply(Object a, Object i) {
		return StaticApply(a, i);
	    }
	}
	public class update : Procedure3 {
	    public static Object StaticApply(Object a, Object i, Object x) {
		try {
		    (((AliceArray) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(i)] = x;
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object i, Object x) {
		return StaticApply(a, i, x);
	    }
	}
    }
    public class Char{
	public class opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) >
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <=
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) >=
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class ord : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((int) (System.Char) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class chr : Procedure {
	    public static Object StaticApply(Object a) {
		Int32 ca = (Int32) CommonOp.Sync(a);

		if ((ca >= System.Char.MinValue) && (ca <= System.Char.MaxValue)) {
		    return (System.Char) ca;
		}
		else {
		    throw new Exception(Prebound.General.Chr);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isAlpha : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetter(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isAlphaNum : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetterOrDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isDigit : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isGraph : Procedure {
	    public static Object StaticApply(Object a) {
		System.Char ca = (System.Char) CommonOp.Sync(a);
		return CommonOp.BtI((System.Char.IsPrintable(ca)
				     && (!System.Char.IsWhiteSpace(ca))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isHexDigit : Procedure {
	    public static Object StaticApply(Object a) {
		System.Char c = (System.Char) CommonOp.Sync(a);
		
		return ((c >= '0') && (c <= '9') ||
			((c >= 'A') && (c <= 'F')) ||
			((c >= 'a') && (c <= 'f')));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isLower : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLower(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isPrint : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPrintable(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isPunct : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPunctuation(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isSpace : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsWhiteSpace(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isUpper : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsUpper(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toLower : Procedure {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToLower(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toUpper : Procedure {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToUpper(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class General {
	public class assign : Procedure2 {
	    public static Object StaticApply(Object c, Object v) {
		((Cell) CommonOp.Sync(c)).Assign(v);
		return Prebound.unit;
	    }
	    public override Object Apply(Object c, Object v) {
		return StaticApply(c, v);
	    }
	}
	public class exchange : Procedure2 {
	    public static Object StaticApply(Object c, Object nv) {
		return ((Cell) CommonOp.Sync(c)).Exchange(nv);
	    }
	    public override Object Apply(Object c, Object nw) {
		return StaticApply(c, nw);
	    }
	}
	public class exnName : Procedure {
	    public static Object StaticApply(Object v) {
		v = CommonOp.Sync(v);
		if (v is ConVal) {
		    v = ((ConVal) v).GetId();
		}
		if (v is String) {
		    return v;
		}
		else {// if (v is Guid) {
		    return (System.String) ""; // to be determined
		}
	    }
	    public override Object Apply(Object v) {
		return StaticApply(v);
	    }
	}
    }
    public class Future {
	public class alarmQuote : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class await : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class awaitOne : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return a; // to be Determined
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class byneed : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class concur : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class isFailed : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class isFuture : Procedure {
	    public static Object StaticApply(Object obj) {
		return obj; // to be Determined
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
    }
    public class GlobalStamp {
	public class @new : Procedure0 {
	    public static Object StaticApply() {
		return new Guid();
	    }
	    public override Object Apply() {
		return StaticApply();
	    }
	}
	public class fromString : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toString : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.Sync(a).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		int val = System.String.Compare((System.String) CommonOp.Sync(a),
						(System.String) CommonOp.Sync(b));

		if (val < 0) {
		    return Prebound.General.LESS;
		}
		else if (val == 0) {
		    return Prebound.General.EQUAL;
		}
		else {
		    return Prebound.General.GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class hash : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a).GetHashCode();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Hole {
	public class fail : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	
	}
	public class fill : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class future : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class hole : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isFailed : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) 0; // not implemented
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isFree : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Int {
	public class opnegate : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    int t = (int) CommonOp.Sync(a);
		    checked {
			return (Int32) (-t);
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) + ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) - ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) * ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
		    
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) < ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) > ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) <= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) >= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class abs : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    return System.Math.Abs((Int32) CommonOp.Sync(a));
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		Int32 ai = (Int32) CommonOp.Sync(a);
		Int32 bi = (Int32) CommonOp.Sync(b);
		if (ai == bi) {
		    return Prebound.General.EQUAL;
		}
		else if (ai < bi) {
		    return Prebound.General.LESS;
		}
		else {
		    return Prebound.General.GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class div : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		Int32 ai = (Int32) CommonOp.Sync(a);
		Int32 bi = (Int32) CommonOp.Sync(b);
		bool b1  = (ai >= 0);
		bool b2  = (bi >= 0);

		try {
		    if (b1 == b2) {
			return (Int32) (ai / bi);
		    }
		    else if (b2) {
			return (Int32) ((ai - bi + 1) / bi);
		    }
		    else {
			return (Int32) ((ai - bi - 1) / bi);
		    }
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class mod : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    int ai = (Int32) CommonOp.Sync(a);
		    int bi = (Int32) CommonOp.Sync(b);
		    int c = (ai % bi);

		    if (c == 0) {
			return c;
		    }
		    else {
			if (c < 0) {
			    if (bi <= 0) {
				return c;
			    }
			    else {
				return (c + bi);
			    }
			}
			else {
			    if (bi < 0) {
				return (c + bi);
			    }
			    else {
				return c;
			    }
			}
		    }
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class quot : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (Int32) (((Int32) CommonOp.Sync(a)) / ((Int32) CommonOp.Sync(b)));
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class rem : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    Int32 ai = (Int32) CommonOp.Sync(a);
		    Int32 bi = (Int32) CommonOp.Sync(b);
		    
		    return (Int32) (ai % bi);
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class toString : Procedure {
	    public static Object StaticApply(Object a) {
		return ((Int32) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Math {
	public class acos : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Acos((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class acosh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) System.Math.Log(x + System.Math.Sqrt(x * x - 1.0));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class asin : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Asin((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class asinh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) System.Math.Log(x + System.Math.Sqrt(x * x + 1.0));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class atan : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Atan((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class atanh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		if (System.Math.Abs(x) > 1.0) {
		    // to be determined: code: errno = EDOM;
		    return (double) System.Math.Asin(2.0);
		}
		else {
		    return (double) System.Math.Log((double) ((1.0 + x) / (1.0 - x)));
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class atan2 : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (double) System.Math.Atan2((double) CommonOp.Sync(a), (double) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class cos : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Cos((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class cosh : Procedure {
	    public static Object StaticApply(Object a) {
		double c = (double) CommonOp.Sync(a);
		return (double) ((System.Math.Pow(System.Math.E, c) +
				  System.Math.Pow(System.Math.E, -c)) / 2.0);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class exp : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Exp((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class ln : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Log((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class pow : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (double) System.Math.Pow((double) CommonOp.Sync(a),
						(double) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class sin : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Sin((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class sinh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) ((System.Math.Pow(System.Math.E, x) -
				  System.Math.Pow(System.Math.E, -x)) / 2);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class sqrt : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Sqrt((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class tan : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Tan((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class tanh : Procedure {
	    public static Object StaticApply(Object a) {
		double x   = (double) CommonOp.Sync(a);
		double ex  = (double) System.Math.Pow(System.Math.E, x);
		double emx = (double) System.Math.Pow(System.Math.E, -x);
		return (double) ((ex - emx) / (ex + emx));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Real {
	public class opnegate : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    checked {
			return (double) (-((double) CommonOp.Sync(a)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) +
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) -
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) *
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opdiv : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) /
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Exception(Prebound.General.Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) <
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) >
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) <=
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) >=
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class ceil : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Ceil((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		double ai = (double) CommonOp.Sync(a);
		double bi = (double) CommonOp.Sync(b);

		if (ai == bi) {
		    return Prebound.General.EQUAL;
		}
		else if (ai < bi) {
		    return Prebound.General.LESS;
		}
		else {
		    return Prebound.General.GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class floor : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Floor((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class fromInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) ((Int32) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class realCeil : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Ceil((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class realFloor : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Floor((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class realTrunc : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		if (x >= 0.0) {
		    return (double) System.Math.Floor(x);
		}
		else {
		    return (double) System.Math.Ceil(x);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class rem : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    double ai = (double) CommonOp.Sync(a);
		    double bi = (double) CommonOp.Sync(b);
		    
		    // to be determined
		    return (double) (ai - (((Int64) (ai / bi)) * bi));
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}

	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class round : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Round((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toString : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return ((double) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class trunc : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class String {
	public class append : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return System.String.Concat((System.String) CommonOp.Sync(a),
					    (System.String) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) < 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) > 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) <= 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) >= 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		int v = System.String.Compare(CommonOp.Sync(a).ToString(),
					      CommonOp.Sync(b).ToString());
		
		if (v < 0) {
		    return Prebound.General.LESS;
		}
		else if (v == 0) {
		    return Prebound.General.EQUAL;
		}
		else {
		    return Prebound.General.GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class explode : Procedure {
	    public static Object StaticApply(Object a) {
		System.String s = (System.String) CommonOp.Sync(a);
		int len         = s.Length;

		if (len == 0) {
		    return (Int32) 1;
		}
		else {
		    TagVal head = ListOp.Cons((System.Char) s[len - 1], (Int32) 1);
		    for (int i = (len - 2); i >= 0; i--) {
			head = ListOp.Cons((System.Char) s[len], head);
		    }
		    return head;
	    }
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class implode : Procedure {
	    public static Object StaticApply(Object a) {
		System.Text.StringBuilder s = new System.Text.StringBuilder();
		Object head                 = CommonOp.Sync(a);
		while (!(head is Int32)) {
		    s.Append((System.Char) CommonOp.Sync(ListOp.Car(head)));
		    head = CommonOp.Sync(ListOp.Cdr(head));
		}
		return s.ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class size : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((System.String) CommonOp.Sync(a)).Length;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (System.Char)
			((System.String) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class substring : Procedure3 {
	    public static Object StaticApply(Object a, Object b, Object c) {
		try {
		    char[] oa = ((System.String) CommonOp.Sync(a)).ToCharArray();
		    int i     = (Int32) CommonOp.Sync(b);
		    int j     = (Int32) CommonOp.Sync(c);
		    int len   = (j - i + 1);
		    char[] na = new char[len + 1];

		    for (int k = 0; i <= j; i++, k++) {
			na[k] = oa[i];
		    }
		    na[len] = (char) 0;
		    return new System.String(na);
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b, Object c) {
		return StaticApply(a, b, c);
	    }
	}
	public class str : Procedure {
	    public static Object StaticApply(Object a) {
		return ((System.Char) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Thread {
	public class Terminate : Procedure {
	    public static Object StaticApply(Object obj) {
		((System.Threading.Thread) CommonOp.Sync(obj)).Stop();
		return Prebound.unit;
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class current : Procedure {
	    public static Object StaticApply(Object a) {
		return System.Threading.Thread.CurrentThread;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class isSuspended : Procedure {
	    public static Object StaticApply(Object a) {
		System.Threading.Thread t = (System.Threading.Thread) CommonOp.Sync(a);
		return CommonOp.BtI(t.ThreadState == System.Threading.ThreadState.Suspended);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class raiseIn : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class resume : Procedure {
	    public static Object StaticApply(Object a) {
		((System.Threading.Thread) CommonOp.Sync(a)).Resume();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class state : Procedure {
	    public static Object StaticApply(Object a) {
		System.Threading.Thread t = (System.Threading.Thread) CommonOp.Sync(a);

		if (t.IsAlive) {
		    return Prebound.Thread.RUNNABLE;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Suspended) {
		    // to be determined
		    return Prebound.Thread.BLOCKED;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Stopped) {
		    return Prebound.Thread.TERMINATED;
		}
		return Prebound.Thread.RUNNABLE;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class suspend : Procedure {
	    public static Object StaticApply(Object a) {
		((System.Threading.Thread) CommonOp.Sync(a)).Suspend();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class yield : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Unsafe {
	public class Array {
	    public class sub : Procedure2 {
		public static Object StaticApply(Object a, Object b) {
		    return (((AliceArray) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(b)];
		}
		public override Object Apply(Object a, Object b) {
		    return StaticApply(a, b);
		}
	    }
	    public class update : Procedure3 {
		public static Object StaticApply(Object a, Object b, Object c) {
		    (((AliceArray) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(b)] = c;
		    return Prebound.unit;
		}
		public override Object Apply(Object a, Object b, Object c) {
		    return StaticApply(a, b, c);
		}
	    }
	}
	public class String {
	    public class sub : Procedure2 {
		public static Object StaticApply(Object a, Object b) {
		    return (System.Char)
			((System.String) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		public override Object Apply(Object a, Object b) {
		    return StaticApply(a, b);
		}
	    }
	}
	public class Vector {
	    public class sub : Procedure2 {
		public static Object StaticApply(Object a, Object b) {
		    return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		public override Object Apply(Object a, Object b) {
		    return StaticApply(a, b);
		}
	    }
	}
	public class cast : Procedure {
	    public static Object StaticApply(Object a) {
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Vector {
	public class fromList : Procedure {
	    public static Object StaticApply(Object a) {
		a = CommonOp.Sync(a);
		int n       = ListOp.Length(a);
		Object[] na = new Object[n];
		for (int i = 0; i < n; i++) {
		    na[i] = ListOp.Car(a);
		    a     = ListOp.Cdr(a);
		}
		return na;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class length : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((Object[]) CommonOp.Sync(a)).Length;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
    }
    public class Word {
	public class fromIntQuote : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class fromInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class toIntX : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) + (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) - (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) * (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class div : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (Int32) ((Int32) CommonOp.Sync(a) / (Int32) CommonOp.Sync(b));
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class mod : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    int ai = (int) CommonOp.Sync(a);
		    int bi = (int) CommonOp.Sync(b);
		    return (Int32) (ai - ((int) (ai / bi)));
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class orb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) | (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class xorb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) ^ (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class andb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) & (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class notb : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) (~(Int32) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class shl : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) << (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class shr : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((uint) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class arithshr : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((int) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b));
		//		Int32 i = (Int32) CommonOp.Sync(a);
		//Int32 n = (Int32) CommonOp.Sync(b);
		//return (Int32) (System.Math.Floor(i / System.Math.Pow(2, n)));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class toString : Procedure {
	    public static Object StaticApply(Object a) {
		return ((Int32) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class OS {
	public class Process {
	    public class system : Procedure {
		public static Object StaticApply(Object a) {
		    // to be determined
		    return a;
		}
		public override Object Apply(Object a) {
		    return StaticApply(a);
		}
	    }
	    public class exit : Procedure {
		public static Object StaticApply(Object obj) {
		    Environment.Exit((Int32) CommonOp.Sync(obj));
		    return Prebound.unit;
		}
		public override Object Apply(Object obj) {
		    return StaticApply(obj);
		}
	    }
	    public class getEnv : Procedure {
		public static Object StaticApply(Object a) {
		    try {
			return new TagVal(1,
					  (System.String)
					  Environment.GetEnvironmentVariable((System.String)
									     CommonOp.Sync(a)));
		    }
		    catch (System.Exception) {
			return Prebound.Option.NONE;
		    }
		}
		public override Object Apply(Object a) {
		    return StaticApply(a);
		}
	    }
	}
    }
    public class TextIO {
	public class openIn : Procedure {
	    public static Object StaticApply(Object a) {
		System.String name = "";
		try {
		    name = (System.String) CommonOp.Sync(a);
		    return new StreamWrapper(name,
					     new StreamReader(new FileStream(name,
									     FileMode.Open,
									     FileAccess.Read)));
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0]       = e; // to be determined
		    ar[1]       = "openIn";
		    ar[2]       = name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class inputAll : Procedure {
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamReader r        = (StreamReader) wrapper.stream;
		try {
		    return r.ReadAll();
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0] = e; // to be determined
		    ar[1] = "inputAll";
		    ar[2] = wrapper.name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class inputLine : Procedure {
	    static Char[] buf = new Char[4097];
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamReader r        = (StreamReader) wrapper.stream;
		try {
		    System.String line = r.ReadLine();

		    if (line == "") {
			return line;
		    }
		    else {
			// SML Base Library helper text
			// if the file ends without newline, stick it on anyway
			return System.String.Concat(line, "\n");
		    }
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0]       = e; // to be determined
		    ar[1]       = "inputLine";
		    ar[2]       = wrapper.name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class closeIn : Procedure {
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamReader r        = (StreamReader) wrapper.stream;

		r.Close();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class openOut : Procedure {
	    public static Object StaticApply(Object a) {
		System.String name = "";
		try {
		    name = (System.String) CommonOp.Sync(a);
		    return new StreamWrapper(name,
					     new StreamWriter(new FileStream(name,
									     FileMode.CreateNew,
									     FileAccess.Write)));
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0]       = e; // to be determined
		    ar[1]       = "openOut";
		    ar[2]       = name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class output : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamWriter  w       = (StreamWriter) wrapper.stream;
		try {
		    w.Write((System.String) CommonOp.Sync(b));
		    return Prebound.unit;
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0] = e; // to be determined
		    ar[1] = "output";
		    ar[2] = wrapper.name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class output1 : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamWriter  w       = (StreamWriter) wrapper.stream;
		try {
		    w.Write((System.Char) CommonOp.Sync(b));
		    return Prebound.unit;
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0] = e; // to be determined
		    ar[1] = "output1";
		    ar[2] = wrapper.name;
		    throw new Exception(new ConVal(Prebound.IO.Io, ar));
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class flushOut : Procedure {
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamWriter w        = (StreamWriter) wrapper.stream;

		w.Flush();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class closeOut : Procedure {
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamWriter w        = (StreamWriter) wrapper.stream;

		w.Close();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class CommandLine {
	public class name : Procedure {
	    public static Object StaticApply(Object a) {
		return Environment.GetCommandLineArgs()[0];
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class arguments : Procedure {
	    public static Object StaticApply(Object a) {
		System.String[] args = Environment.GetCommandLineArgs();
		int len              = args.Length;

		if (len == 1) {
		    return (Int32) 1;
		}
		else {
		    TagVal head = ListOp.Cons(args[len - 1], (Int32) 1);
		    for (int i = (len - 2); i >= 1; i--) {
			head = (TagVal) ListOp.Cons(args[i], head);
		    }
		    return head;
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Prebound {
	public static Object unit = null;
	public class Future {
	    public static Object future = "Future.Future";
	    public static Object alarmQuote = new Alice.Future.alarmQuote();
	    public static Object await      = new Alice.Future.await();
	    public static Object awaitOne   = new Alice.Future.awaitOne();
	    public static Object byneed     = new Alice.Future.byneed();
	    public static Object concur     = new Alice.Future.concur();
	    public static Object isFailed   = new Alice.Future.isFailed();
	    public static Object isFuture   = new Alice.Future.isFuture();
	}
	public class Bool {
	    public static Object @false = (Int32) 0;
	    public static Object @true  = (Int32) 1;
	}
	public class List {
	    public static Object Empty = "List.Empty";
	    public static Object nil   = (Int32) 1;
	    public static Object cons  = (Int32) 0;
	}
	public class Array {
	    public static Object array    = new Alice.Array.array();
	    public static Object fromList = new Alice.Array.fromList();
	    public static Object length   = new Alice.Array.length();
	    public static Object sub      = new Alice.Array.sub();
	    public static Object update   = new Alice.Array.update();
	}
	public class Char {
	    public static Object opless      = new Alice.Char.opless();
	    public static Object opgreater   = new Alice.Char.opgreater();
	    public static Object oplessEq    = new Alice.Char.oplessEq();
	    public static Object opgreaterEq = new Alice.Char.opgreaterEq();
	    public static Object ord         = new Alice.Char.ord();
	    public static Object chr         = new Alice.Char.chr();
	    public static Object isAlpha     = new Alice.Char.isAlpha();
	    public static Object isAlphaNum  = new Alice.Char.isAlphaNum();
	    public static Object isDigit     = new Alice.Char.isDigit();
	    public static Object isGraph     = new Alice.Char.isGraph();
	    public static Object isHexDigit  = new Alice.Char.isHexDigit();
	    public static Object isLower     = new Alice.Char.isLower();
	    public static Object isPrint     = new Alice.Char.isPrint();
	    public static Object isPunct     = new Alice.Char.isPunct();
	    public static Object isSpace     = new Alice.Char.isSpace();
	    public static Object isUpper     = new Alice.Char.isUpper();
	    public static Object toLower     = new Alice.Char.toLower();
	    public static Object toUpper     = new Alice.Char.toUpper();
	}
	public class General {
	    public static Object Chr       = "General.Chr";
	    public static Object Div       = "General.Div";
	    public static Object Domain    = "General.Domain";
	    public static Object Fail      = "General.Fail";
	    public static Object Overflow  = "General.Overflow";
	    public static Object Size      = "General.Size";
	    public static Object Span      = "General.Span";
	    public static Object Subscript = "General.Subscript";
	    public static Object Match     = "General.Match";
	    public static Object Bind      = "General.Bind";
	    public static Object EQUAL     = (Int32) 0;
	    public static Object LESS      = (Int32) 2;
	    public static Object GREATER   = (Int32) 1;
	    public static Object assign    = new Alice.General.assign();
	    public static Object exchange  = new Alice.General.exchange();
	    public static Object exnName   = new Alice.General.exnName();
	    public static Object @ref      = new Alice.Ref();
	}
	public class Option {
	    public static Object option  = "Option.Option";
	    public static Object NONE    = (Int32) 0;
	    public static Object SOME    = (Int32) 1;
	}
	public class GlobalStamp {
	    public static Object @new       = new Alice.GlobalStamp.@new();
	    public static Object fromString = new Alice.GlobalStamp.fromString();
	    public static Object toString   = new Alice.GlobalStamp.toString();
	    public static Object compare    = new Alice.GlobalStamp.compare();
	    public static Object hash       = new Alice.GlobalStamp.hash();
	}
	public class Hole {
	    public static Object hole     = "Hole.Hole";
	    public static Object fail     = new Alice.Hole.fail();
	    public static Object fill     = new Alice.Hole.fill();
	    public static Object future   = new Alice.Hole.future();
	    public static Object isFailed = new Alice.Hole.isFailed();
	    public static Object isFree   = new Alice.Hole.isFree();
	}
	public class Int {
	    public static Object minInt      = Int32.MinValue;
	    public static Object maxInt      = Int32.MaxValue;
	    public static Object precision   = (Int32) 32;
	    public static Object opnegate    = new Alice.Int.opnegate();
	    public static Object opadd       = new Alice.Int.opadd();
	    public static Object opsub       = new Alice.Int.opsub();
	    public static Object opmul       = new Alice.Int.opmul();
	    public static Object opless      = new Alice.Int.opless();
	    public static Object opgreater   = new Alice.Int.opgreater();
	    public static Object oplessEq    = new Alice.Int.oplessEq();
	    public static Object opgreaterEq = new Alice.Int.opgreaterEq();
	    public static Object abs         = new Alice.Int.abs();
	    public static Object compare     = new Alice.Int.compare();
	    public static Object div         = new Alice.Int.div();
	    public static Object mod         = new Alice.Int.mod();
	    public static Object quot        = new Alice.Int.quot();
	    public static Object rem         = new Alice.Int.rem();
	    public static Object toString    = new Alice.Int.toString();
	}
	public class Math {
	    public static Object e     = System.Math.E;
	    public static Object pi    = System.Math.PI;
	    public static Object acos  = new Alice.Math.acos();
	    public static Object acosh = new Alice.Math.acosh();
	    public static Object asin  = new Alice.Math.asin();
	    public static Object asinh = new Alice.Math.asinh();
	    public static Object atan  = new Alice.Math.atan();
	    public static Object atanh = new Alice.Math.atanh();
	    public static Object atan2 = new Alice.Math.atan2();
	    public static Object cos   = new Alice.Math.cos();
	    public static Object cosh  = new Alice.Math.cosh();
	    public static Object exp   = new Alice.Math.exp();
	    public static Object ln    = new Alice.Math.ln();
	    public static Object pow   = new Alice.Math.pow();
	    public static Object sin   = new Alice.Math.sin();
	    public static Object sinh  = new Alice.Math.sinh();
	    public static Object sqrt  = new Alice.Math.sqrt();
	    public static Object tan   = new Alice.Math.tan();
	    public static Object tanh  = new Alice.Math.tanh();
	}
	public class Real {
	    public static Object negInf      = System.Double.NegativeInfinity;
	    public static Object posInf      = System.Double.PositiveInfinity;
	    public static Object opnegate    = new Alice.Real.opnegate();
	    public static Object opadd       = new Alice.Real.opadd();
	    public static Object opsub       = new Alice.Real.opsub();
	    public static Object opmul       = new Alice.Real.opmul();
	    public static Object opdiv       = new Alice.Real.opdiv();
	    public static Object opless      = new Alice.Real.opless();
	    public static Object opgreater   = new Alice.Real.opgreater();
	    public static Object oplessEq    = new Alice.Real.oplessEq();
	    public static Object opgreaterEq = new Alice.Real.opgreaterEq();
	    public static Object ceil        = new Alice.Real.ceil();
	    public static Object compare     = new Alice.Real.compare();
	    public static Object floor       = new Alice.Real.floor();
	    public static Object fromInt     = new Alice.Real.fromInt();
	    public static Object realCeil    = new Alice.Real.realCeil();
	    public static Object realFloor   = new Alice.Real.realFloor();
	    public static Object realTrunc   = new Alice.Real.realTrunc();
	    public static Object rem         = new Alice.Real.rem();
	    public static Object round       = new Alice.Real.round();
	    public static Object toString    = new Alice.Real.toString();
	    public static Object trunc       = new Alice.Real.trunc();
	}
	public class String {
	    public static Object maxSize     = Int32.MaxValue;
	    public static Object append      = new Alice.String.append();
	    public static Object opless      = new Alice.String.opless();
	    public static Object opgreater   = new Alice.String.opgreater();
	    public static Object oplessEq    = new Alice.String.oplessEq();
	    public static Object opgreaterEq = new Alice.String.opgreaterEq();
	    public static Object compare     = new Alice.String.compare();
	    public static Object explode     = new Alice.String.explode();
	    public static Object implode     = new Alice.String.implode();
	    public static Object size        = new Alice.String.size();
	    public static Object sub         = new Alice.String.sub();
	    public static Object substring   = new Alice.String.substring();
	    public static Object str         = new Alice.String.str();
	}
	public class Thread {
	    public static Object RUNNABLE    = "Thread.RUNNABLE";
	    public static Object BLOCKED     = "Thread.BLOCKED";
	    public static Object TERMINATED  = "Thread.TERMINATED";
	    public static Object Terminate   = new Alice.Thread.Terminate();
	    public static Object current     = new Alice.Thread.current();
	    public static Object isSuspended = new Alice.Thread.isSuspended();
	    public static Object raiseIn     = new Alice.Thread.raiseIn();
	    public static Object resume      = new Alice.Thread.resume();
	    public static Object state       = new Alice.Thread.resume();
	    public static Object suspend     = new Alice.Thread.suspend();
	    public static Object yield       = new Alice.Thread.yield();
	}
	public class Word {
	    public static Object fromIntQuote = new Alice.Word.fromIntQuote();
	    public static Object fromInt      = new Alice.Word.fromInt();
	    public static Object toInt        = new Alice.Word.toInt();
	    public static Object toIntX       = new Alice.Word.toIntX();
	    public static Object opadd        = new Alice.Word.opadd();
	    public static Object opsub        = new Alice.Word.opsub();
	    public static Object opmul        = new Alice.Word.opmul();
	    public static Object div          = new Alice.Word.div();
	    public static Object mod          = new Alice.Word.mod();
	    public static Object orb          = new Alice.Word.orb();
	    public static Object xorb         = new Alice.Word.xorb();
	    public static Object andb         = new Alice.Word.andb();
	    public static Object notb         = new Alice.Word.notb();
	    public static Object shl          = new Alice.Word.shl();
	    public static Object shr          = new Alice.Word.shr();
	    public static Object arithshr     = new Alice.Word.arithshr();
	    public static Object toString     = new Alice.Word.toString();
	}
	public class IO {
	    public static Object Io = "IO.Io";
	}
	public class OS {
	    public class Process {
		public static Object success = (Int32) 0;
		public static Object failure = (Int32) 1;
		public static Object system  = new Alice.OS.Process.system();
		public static Object exit    = new Alice.OS.Process.exit();
		public static Object getEnv  = new Alice.OS.Process.getEnv();
	    }
	}
	public class TextIO {
	    public static Object stdIn =
		new StreamWrapper("stdin",
				  new StreamReader(new FileStream("stdin",
								  FileMode.Open,
								  FileAccess.Read)));
	    public static Object stdOut =
		new StreamWrapper("stdout",
				  new StreamReader(new FileStream("stdout",
								  FileMode.Open,
								  FileAccess.Write)));
	    public static Object stdErr =
		new StreamWrapper("stderr",
				  new StreamReader(new FileStream("stderr",
								  FileMode.Open,
								  FileAccess.Write)));
	    public static Object openIn    = new Alice.TextIO.openIn();
	    public static Object inputAll  = new Alice.TextIO.inputAll();
	    public static Object inputLine = new Alice.TextIO.inputLine();
	    public static Object closeIn   = new Alice.TextIO.closeIn();
	    public static Object openOut   = new Alice.TextIO.openOut();
	    public static Object output    = new Alice.TextIO.output();
	    public static Object output1   = new Alice.TextIO.output1();
	    public static Object flushOut  = new Alice.TextIO.flushOut();
	    public static Object closeOut  = new Alice.TextIO.closeOut();
	}
	public class Unsafe {
	    public class Array {
		public Object sub    = new Alice.Unsafe.Array.sub();
		public Object update = new Alice.Unsafe.Array.update();
	    }
	    public class Vector {
		public static Object sub = new Alice.Unsafe.Vector.sub();
	    }
	    public static Object cast = new Alice.Unsafe.cast();
	}
	public class Vector {
	    public static Object fromList = new Alice.Vector.fromList();
	    public static Object maxLen   = Int32.MaxValue;
	    public static Object length   = new Alice.Vector.length();
	    public static Object sub      = new Alice.Vector.sub();
	}
	public class CommandLine {
	    public static Object name      = new Alice.CommandLine.name();
	    public static Object arguments = new Alice.CommandLine.arguments();
	}
    }
}
