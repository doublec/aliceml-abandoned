using System;
using System.Threading;
using System.Reflection;

using Alice;
using Alice.Values;
using Alice.Builtins;

namespace Alice {
    namespace Values {
	public class Array {
	    public Object[] Value;
	    public Array(Object[] Value) {
		this.Value = Value;
	    }
	}
	public abstract class Procedure {
	    public abstract Object Apply(Object a);
	    public virtual Object Apply() {
		return Apply(Prebound.unit);
	    }
	    public virtual Object Apply(Object a, Object b) {
		Object[] ar = new Object[2];
		ar[0] = a;
		ar[1] = b;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c) {
		Object[] ar = new Object[3];
		ar[0] = a; ar[1] = b; ar[2] = c;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d) {
		Object[] ar = new Object[4];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d, Object e) {
		Object[] ar = new Object[5];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f) {
		Object[] ar = new Object[6];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
		Object[] ar = new Object[7];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f; ar[6] = g;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
		Object[] ar = new Object[8];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f; ar[6] = g; ar[7] = h;
		return Apply(ar);
	    }
	    public virtual Object Apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
		Object[] ar = new Object[9];
		ar[0] = a; ar[1] = b; ar[2] = c; ar[3] = d; ar[4] = e; ar[5] = f;
		ar[6] = g; ar[7] = h; ar[8] = i;
		return Apply(ar);
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
	public class Cell {
	    public Object Value;
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
	public abstract class Transient : Procedure {
	    public abstract Object Deref();
	    public abstract Object Await();
	    public abstract bool IsVariantOf(Transient t);
	    public abstract bool IsFailed();
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
	}
	public class FailedTransient : Transient {
	    private Object Exn;
	    public FailedTransient(Object exn) {
		Exn = exn;
	    }
	    public override Object Deref() {
		return this;
	    }
	    public override Object Await() {
		throw new Exception(Exn);
	    }
	    public override bool IsVariantOf(Transient t) {
		return false;
	    }
	    public override bool IsFailed() {
		return true;
	    }
	}
	public class Hole : Transient {
	    private Object Ref;
	    public Hole() {
		Ref = null;
	    }
	    public override Object Deref() {
		lock (this) {
		    if (Ref == null) {
			return this;
		    }
		    else if (Ref is Transient) {
			return ((Transient) Ref).Deref();
		    }
		    else {
			return Ref;
		    }
		}
	    }
	    public override Object Await() {
		lock (this) {
		    if (Ref == null) {
			throw new Exception(Prebound.Hole_Hole);
		    }
		    if (Ref is Transient) {
			return ((Transient) Ref).Await();
		    }
		    else {
			return Ref;
		    }
		}
	    }
	    public Object AwaitInternal() {
		lock (this) {
		    if (Ref == null) {
			System.Threading.Monitor.Wait(this);
		    }
		    if (Ref is Transient) {
			return ((Transient) Ref).Await();
		    }
		    else {
			return Ref;
		    }
		}
	    }
	    public override bool IsVariantOf(Transient t) {
		lock (this) {
		    return
			this == t ||
			Ref != null &&
			Ref is Transient &&
			((Transient) Ref).IsVariantOf(t);
		}
	    }
	    public override bool IsFailed() {
		lock (this) {
		    return
			Ref != null &&
			Ref is Transient &&
			((Transient) Ref).IsFailed();
		}
	    }
	    public void Fill(Object x) {
		lock (this) {
		    if (Ref == null) {
			if (x is Transient && ((Transient) x).IsVariantOf(this)) {
			    throw new Exception(Prebound.Hole_Cyclic);
			}
			Ref = x;
			System.Threading.Monitor.PulseAll(this);
		    }
		    else {
			throw new Exception(Prebound.Hole_Hole);
		    }
		}
	    }
	}
	public class Future : Transient {
	    private Hole Ref;
	    public Future(Hole hole) {
		Ref = hole;
	    }
	    public override Object Deref() {
		Object r = Ref.Deref();
		if (r == Ref) {
		    return this;
		}
		else {
		    return r;
		}
	    }
	    public override Object Await() {
		return Ref.AwaitInternal();
	    }
	    public override bool IsVariantOf(Transient t) {
		return Ref.IsVariantOf(t);
	    }
	    public override bool IsFailed() {
		return Ref.IsFailed();
	    }
	}
	public class Byneed : Transient {
	    private enum ByneedState { Fresh, Bound, Failed };
	    private ByneedState State;
	    private Object Ref;
	    public Byneed(Procedure proc) {
		State = ByneedState.Fresh;
		Ref = proc;
	    }
	    public override Object Deref() {
		lock (this) {
		    switch (State) {
		    case ByneedState.Bound:
			return Ref;
		    case ByneedState.Failed:
			throw new Exception(Ref);
		    default:
			return this;
		    }
		}
	    }
	    public override Object Await() {
		lock (this) {
		    switch (State) {
		    case ByneedState.Bound:
			return Ref;
		    case ByneedState.Failed:
			throw new Exception(Ref);
		    default:
			try {
			    Object r = ((Procedure) Ref).Apply();
			    if (r is Transient) {
				Transient t = (Transient) r;
				if (t.IsVariantOf(this)) {
				    throw new Exception(Prebound.Hole_Cyclic);
				}
				State = ByneedState.Bound;
				Ref = t.Await();
			    }
			    else {
				State = ByneedState.Bound;
				Ref = r;
			    }
			}
			catch (Values.Exception exn) {
			    State = ByneedState.Failed;
			    Ref = new ConVal(Prebound.Future_Future, exn.Value);
			    throw new Exception(Ref);
			}
			return Ref;
		    }
		}
	    }
	    public override bool IsVariantOf(Transient t) {
		return this == t;
	    }
	    public override bool IsFailed() {
		return State == ByneedState.Failed;
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
	class RefConstructor : Procedure {
	    public override Object Apply(Object obj) {
		return new Cell(obj);
	    }
	}
	public class Exception : SystemException {
	    public Object Value;
	    public int Line;
	    public Exception(Object obj) {
		Value = obj;
		Line  = -1;
	    }
	    public Exception(Object obj, int line) {
		Value = obj;
		Line = line;
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
    }
    public class CommonOp {
	public static Object Sync(Object obj) {
	    if (obj is Transient) {
		return ((Transient) obj).Await();
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
    public class ListOp {
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
    namespace Builtins {
	public class opeq : Procedure2 {
	    public static bool BothConVal(Object a, Object b) {
		return ((a is ConVal) && (b is ConVal));
	    }
	    public static bool BothTagVal(Object a, Object b) {
		return ((a is TagVal) && (b is TagVal));
	    }
	    public static Object StaticApply(Object a, Object b) {
		a = CommonOp.Sync(a);
		b = CommonOp.Sync(b);

		if (a is Values.Array) {
		    return CommonOp.BtI(a == b);
		}
		else if (a is Cell) {
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
	public class Array_array : Procedure2 {
	    public static Object StaticApply(Object n, Object x) {
		int elems  = (Int32) CommonOp.Sync(n);
		Object[] a = new Object[elems];
	    
		for (int i = 0; i < elems; i++) {
		    a[i] = x;
		}
		return new Values.Array(a);
	    }
	    public override Object Apply(Object n, Object x) {
		return StaticApply(n, x);
	    }
	}
	public class Array_fromList : Procedure {
	    public static Object StaticApply(Object x) {
		x = CommonOp.Sync(x);
		int n      = ListOp.Length(x);
		Object[] a = new Object[n];
	    
		for (int i = 0; i < n; i++) {
		    a[i] = ListOp.Car(x);
		    x    = ListOp.Cdr(x);
		}
	    
		return new Values.Array(a);
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class Array_length : Procedure {
	    public static Object StaticApply(Object x) {
		return ((Values.Array) CommonOp.Sync(x)).Value.Length;
	    }
	    public override Object Apply(Object x) {
		return StaticApply(x);
	    }
	}
	public class Array_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object i) {
		try {
		    return (((Values.Array) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(i)];
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override Object Apply(Object a, Object i) {
		return StaticApply(a, i);
	    }
	}
	public class Array_update : Procedure3 {
	    public static Object StaticApply(Object a, Object i, Object x) {
		try {
		    (((Values.Array) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(i)] = x;
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object i, Object x) {
		return StaticApply(a, i, x);
	    }
	}
	public class Char_opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) >
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <=
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) >=
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_ord : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((int) (System.Char) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_chr : Procedure {
	    public static Object StaticApply(Object a) {
		Int32 ca = (Int32) CommonOp.Sync(a);

		if ((ca >= System.Char.MinValue) && (ca <= System.Char.MaxValue)) {
		    return (System.Char) ca;
		}
		else {
		    throw new Values.Exception(Prebound.General_Chr);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isAlpha : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetter(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isAlphaNum : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLetterOrDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isCntrl : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(!System.Char.IsPrintable(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isDigit : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsDigit(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isGraph : Procedure {
	    public static Object StaticApply(Object a) {
		System.Char ca = (System.Char) CommonOp.Sync(a);
		return CommonOp.BtI((System.Char.IsPrintable(ca)
				     && (!System.Char.IsWhiteSpace(ca))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isHexDigit : Procedure {
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
	public class Char_isLower : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsLower(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isPrint : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPrintable(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isPunct : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsPunctuation(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isSpace : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsWhiteSpace(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isUpper : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(System.Char.IsUpper(((System.Char) CommonOp.Sync(a))));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_toLower : Procedure {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToLower(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Char_toUpper : Procedure {
	    public static Object StaticApply(Object a) {
		return (System.Char) System.Char.ToUpper(((System.Char) CommonOp.Sync(a)));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class General_assign : Procedure2 {
	    public static Object StaticApply(Object c, Object v) {
		((Cell) CommonOp.Sync(c)).Assign(v);
		return Prebound.unit;
	    }
	    public override Object Apply(Object c, Object v) {
		return StaticApply(c, v);
	    }
	}
	public class General_exchange : Procedure2 {
	    public static Object StaticApply(Object c, Object nv) {
		return ((Cell) CommonOp.Sync(c)).Exchange(nv);
	    }
	    public override Object Apply(Object c, Object nw) {
		return StaticApply(c, nw);
	    }
	}
	public class General_exnName : Procedure {
	    public static Object StaticApply(Object v) {
		v = CommonOp.Sync(v);
		if (v is ConVal) {
		    v = ((ConVal) v).GetId();
		}
		if (v is String) {
		    return v;
		}
		else {// if (v is Guid)
		    return (System.String) ""; // to be determined
		}
	    }
	    public override Object Apply(Object v) {
		return StaticApply(v);
	    }
	}
	public class Future_alarmQuote : Procedure {
	    class FutureClass {
		Hole H;
		int MS;
		public FutureClass(Hole hole, int msecs) {
		    H  = hole;
		    MS = msecs;
		}
		public void Run() {
		    Thread.Sleep((MS + 500) / 1000);
		    H.Fill(Prebound.unit);
		}
	    }
	    public static Object StaticApply(Object obj) {
		Hole hole = new Hole();
		int msecs = (Int32) CommonOp.Sync(obj);
		FutureClass fc = new FutureClass(hole, msecs);
		
		new Thread(new ThreadStart(fc.Run)).Start();
		return new Future(hole);
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_await : Procedure {
	    public static Object StaticApply(Object obj) {
		if (obj is Transient) {
		    return ((Transient) obj).Await();
		}
		else {
		    return obj;
		}
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_awaitOne : Procedure2 {
	    class ThreadClass {
		Object W;
		Object N;
		public ThreadClass(Object w, Object n) {
		    W = w;
		    N = n;
		}
		public void Run() {
		    if (W is Transient) {
			((Transient) W).Await();
		    }
		    Monitor.PulseAll(N);
		}
	    }
	    public static Object StaticApply(Object a, Object b) {
		Object n  = new Object();
		Thread at = new Thread(new ThreadStart(new ThreadClass(a, n).Run));
		Thread bt = new Thread(new ThreadStart(new ThreadClass(b, n).Run));
		at.Start();
		bt.Start();
		Monitor.Wait(n);
		at.Stop();
		bt.Stop();
		return a;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Future_byneed : Procedure {
	    public static Object StaticApply(Object obj) {
		return new Byneed((Procedure) CommonOp.Sync(obj));
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_concur : Procedure {
	    class FutureClass {
		Hole H;
		Procedure F;
		public FutureClass(Hole h, Procedure f) {
		    H = h;
		    F = f;
		}
		public void Run() {
		    try {
			H.Fill(F.Apply());
		    }
		    catch (Values.Exception e) {
			H.Fill(new FailedTransient(new ConVal(Prebound.Future_Future, e.Value)));
		    }
		}
	    }
	    public static Object StaticApply(Object obj) {
		Hole hole   = new Hole();
		Procedure f = (Procedure) CommonOp.Sync(obj);
		new Thread(new ThreadStart(new FutureClass(hole, f).Run)).Start();
		return new Future(hole);
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_isFailed : Procedure {
	    public static Object StaticApply(Object obj) {
		return CommonOp.BtI(obj is Transient &&
				    ((Transient) obj).IsFailed());
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_isFuture : Procedure {
	    public static Object StaticApply(Object obj) {
		if (obj is Transient) {
		    obj = ((Transient) obj).Deref();
		    return CommonOp.BtI(obj is Future || obj is Byneed ||
					obj is FailedTransient);
		}
		else {
		    return (Int32) 0;
		}
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class GlobalStamp_new : Procedure0 {
	    public static Object StaticApply() {
		return new Guid();
	    }
	    public override Object Apply() {
		return StaticApply();
	    }
	}
	public class GlobalStamp_fromString : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class GlobalStamp_toString : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.Sync(a).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class GlobalStamp_compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		int val = System.String.Compare((System.String) CommonOp.Sync(a),
						(System.String) CommonOp.Sync(b));

		if (val < 0) {
		    return Prebound.General_LESS;
		}
		else if (val == 0) {
		    return Prebound.General_EQUAL;
		}
		else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class GlobalStamp_hash : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a).GetHashCode();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_fail : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    Object exn = new ConVal(Prebound.Future_Future, b);
		    ((Hole) a).Fill(new FailedTransient(exn));
		}
		else {
		    throw new Values.Exception(Prebound.Hole_Hole);
		}
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Hole_fill : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    ((Hole) a).Fill(b);
		}
		else {
		    throw new Values.Exception(Prebound.Hole_Hole);
		}
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Hole_future : Procedure {
	    public static Object StaticApply(Object a) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    return new Future((Hole) a);
		}
		else {
		    return a;
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_hole : Procedure0 {
	    public static Object StaticApply() {
		return new Values.Hole();
	    }
	    public override Object Apply() {
		return StaticApply();
	    }
	}
	public class Hole_isFailed : Procedure {
	    public static Object StaticApply(Object a) {
		return CommonOp.BtI(a is Transient &&
				    ((Transient) a).IsFailed());
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_isHole : Procedure {
	    public static Object StaticApply(Object a) {
		if (a is Transient) {
		    return CommonOp.BtI(((Transient) a).Deref() is Hole);
		}
		else {
		    return (Int32) 0;
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Int_opnegate : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    int t = (int) CommonOp.Sync(a);
		    checked {
			return (Int32) (-t);
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Int_opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) + ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) - ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (Int32) (((Int32) CommonOp.Sync(a)) * ((Int32) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
		    
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) < ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) > ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) <= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((Int32) CommonOp.Sync(a)) >= ((Int32) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_abs : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    return System.Math.Abs((Int32) CommonOp.Sync(a));
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Int_compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		Int32 ai = (Int32) CommonOp.Sync(a);
		Int32 bi = (Int32) CommonOp.Sync(b);
		if (ai == bi) {
		    return Prebound.General_EQUAL;
		}
		else if (ai < bi) {
		    return Prebound.General_LESS;
		}
		else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_div : Procedure2 {
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
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_mod : Procedure2 {
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
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_quot : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (Int32) (((Int32) CommonOp.Sync(a)) / ((Int32) CommonOp.Sync(b)));
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_rem : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    Int32 ai = (Int32) CommonOp.Sync(a);
		    Int32 bi = (Int32) CommonOp.Sync(b);
		    
		    return (Int32) (ai % bi);
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_toString : Procedure {
	    public static Object StaticApply(Object a) {
		return ((Int32) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_acos : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Acos((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_acosh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) System.Math.Log(x + System.Math.Sqrt(x * x - 1.0));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_asin : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Asin((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_asinh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) System.Math.Log(x + System.Math.Sqrt(x * x + 1.0));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_atan : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Atan((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_atanh : Procedure {
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
	public class Math_atan2 : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (double) System.Math.Atan2((double) CommonOp.Sync(a), (double) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Math_cos : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Cos((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_cosh : Procedure {
	    public static Object StaticApply(Object a) {
		double c = (double) CommonOp.Sync(a);
		return (double) ((System.Math.Pow(System.Math.E, c) +
				  System.Math.Pow(System.Math.E, -c)) / 2.0);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_exp : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Exp((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_ln : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Log((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_pow : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (double) System.Math.Pow((double) CommonOp.Sync(a),
						(double) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Math_sin : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Sin((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_sinh : Procedure {
	    public static Object StaticApply(Object a) {
		double x = (double) CommonOp.Sync(a);
		return (double) ((System.Math.Pow(System.Math.E, x) -
				  System.Math.Pow(System.Math.E, -x)) / 2);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_sqrt : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Sqrt((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_tan : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Tan((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Math_tanh : Procedure {
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
	public class Real_opnegate : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    checked {
			return (double) (-((double) CommonOp.Sync(a)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) +
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) -
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) *
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opdiv : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    checked {
			return (double) (((double) CommonOp.Sync(a)) /
					 ((double) CommonOp.Sync(b)));
		    }
		}
		catch (System.OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) <
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) >
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) <=
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((double) CommonOp.Sync(a)) >=
				     ((double) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_ceil : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Ceil((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		double ai = (double) CommonOp.Sync(a);
		double bi = (double) CommonOp.Sync(b);

		if (ai == bi) {
		    return Prebound.General_EQUAL;
		}
		else if (ai < bi) {
		    return Prebound.General_LESS;
		}
		else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_floor : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Floor((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_fromInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) ((Int32) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realCeil : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Ceil((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realFloor : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Floor((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realRound : Procedure {
	    public static Object StaticApply(Object a) {
		return (double) System.Math.Round((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realTrunc : Procedure {
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
	public class Real_rem : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    double ai = (double) CommonOp.Sync(a);
		    double bi = (double) CommonOp.Sync(b);
		    
		    // to be determined
		    return (double) (ai - (((Int64) (ai / bi)) * bi));
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Div);
		}

	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_round : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) System.Math.Round((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_toString : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return ((double) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Real_trunc : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((double) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class String_append : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return System.String.Concat((System.String) CommonOp.Sync(a),
					    (System.String) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_opless : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) < 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_opgreater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) > 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_oplessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) <= 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_opgreaterEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI(System.String.Compare((System.String) CommonOp.Sync(a),
							  (System.String) CommonOp.Sync(b)) >= 0);
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_compare : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		int v = System.String.Compare(CommonOp.Sync(a).ToString(),
					      CommonOp.Sync(b).ToString());
		
		if (v < 0) {
		    return Prebound.General_LESS;
		}
		else if (v == 0) {
		    return Prebound.General_EQUAL;
		}
		else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_explode : Procedure {
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
	public class String_implode : Procedure {
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
	public class String_size : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((System.String) CommonOp.Sync(a)).Length;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class String_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (System.Char)
			((System.String) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_substring : Procedure3 {
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
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b, Object c) {
		return StaticApply(a, b, c);
	    }
	}
	public class String_str : Procedure {
	    public static Object StaticApply(Object a) {
		return ((System.Char) CommonOp.Sync(a)).ToString();
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_Terminate : Procedure {
	    public static Object StaticApply(Object obj) {
		((System.Threading.Thread) CommonOp.Sync(obj)).Stop();
		return Prebound.unit;
	    }
	    public override Object Apply(Object obj) {
		return StaticApply(obj);
	    }
	}
	public class Thread_current : Procedure {
	    public static Object StaticApply(Object a) {
		return System.Threading.Thread.CurrentThread;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_isSuspended : Procedure {
	    public static Object StaticApply(Object a) {
		System.Threading.Thread t = (System.Threading.Thread) CommonOp.Sync(a);
		return CommonOp.BtI(t.ThreadState == System.Threading.ThreadState.Suspended);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_raiseIn : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Thread_resume : Procedure {
	    public static Object StaticApply(Object a) {
		((System.Threading.Thread) CommonOp.Sync(a)).Resume();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_state : Procedure {
	    public static Object StaticApply(Object a) {
		System.Threading.Thread t = (System.Threading.Thread) CommonOp.Sync(a);

		if (t.IsAlive) {
		    return Prebound.Thread_RUNNABLE;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Suspended) {
		    // to be determined
		    return Prebound.Thread_BLOCKED;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Stopped) {
		    return Prebound.Thread_TERMINATED;
		}
		return Prebound.Thread_RUNNABLE;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_suspend : Procedure {
	    public static Object StaticApply(Object a) {
		((System.Threading.Thread) CommonOp.Sync(a)).Suspend();
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_yield : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return Prebound.unit;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_Array_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (((Values.Array) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(b)];
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_Array_update : Procedure3 {
	    public static Object StaticApply(Object a, Object b, Object c) {
		(((Values.Array) CommonOp.Sync(a)).Value)[(Int32) CommonOp.Sync(b)] = c;
		return Prebound.unit;
	    }
	    public override Object Apply(Object a, Object b, Object c) {
		return StaticApply(a, b, c);
	    }
	}
	public class Unsafe_String_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (System.Char)
		    ((System.String) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_Vector_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_cast : Procedure {
	    public static Object StaticApply(Object a) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_getTag : Procedure {
	    public static Object StaticApply(Object a) {
		a = CommonOp.Sync(a);
		if (a is Int32) {
		    return a;
		}
		else {
		    return ((TagVal) a).GetTag();
		}
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_getValue : Procedure {
	    public static Object StaticApply(Object a) {
		a = CommonOp.Sync(a);
		return ((TagVal) a).Value;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Vector_fromList : Procedure {
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
	public class Vector_length : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) ((Object[]) CommonOp.Sync(a)).Length;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Vector_sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_fromIntQuote : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Word_fromInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Word_toInt : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Word_toIntX : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Word_opadd : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) + (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_opsub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) - (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_opmul : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) * (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_div : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    return (Int32) ((Int32) CommonOp.Sync(a) / (Int32) CommonOp.Sync(b));
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_mod : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    int ai = (int) CommonOp.Sync(a);
		    int bi = (int) CommonOp.Sync(b);
		    return (Int32) (ai - ((int) (ai / bi)));
		}
		catch (System.Exception) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_orb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) | (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_xorb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) ^ (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_andb : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) & (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_notb : Procedure {
	    public static Object StaticApply(Object a) {
		return (Int32) (~(Int32) CommonOp.Sync(a));
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class Word_shl : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) << (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_shr : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((uint) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_arithshr : Procedure2 {
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
	public class Word_toString : Procedure {
	    public static Object StaticApply(Object a) {
		return ((Int32) CommonOp.Sync(a)).ToString(); // to be determined
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Prebound {
	public static Object opeq    = new Builtins.opeq();
	public static Object opnoteq = new Builtins.opnoteq();
	public static Object unit    = (Int32) 0;

	public static Object Array_array    = new Array_array();
	public static Object Array_fromList = new Array_fromList();
	public static Object Array_length   = new Array_length();
	public static Object Array_maxLen   = Int32.MaxValue;
	public static Object Array_sub      = new Array_sub();
	public static Object Array_update   = new Array_update();

	public static Object Bool_false = (Int32) 0;
	public static Object Bool_true  = (Int32) 1;

	public static Object Char_opless      = new Char_opless();
	public static Object Char_opgreater   = new Char_opgreater();
	public static Object Char_oplessEq    = new Char_oplessEq();
	public static Object Char_opgreaterEq = new Char_opgreaterEq();
	public static Object Char_ord         = new Char_ord();
	public static Object Char_chr         = new Char_chr();
	public static Object Char_isAlpha     = new Char_isAlpha();
	public static Object Char_isAlphaNum  = new Char_isAlphaNum();
	public static Object Char_isCntrl     = new Char_isCntrl();
	public static Object Char_isDigit     = new Char_isDigit();
	public static Object Char_isGraph     = new Char_isGraph();
	public static Object Char_isHexDigit  = new Char_isHexDigit();
	public static Object Char_isLower     = new Char_isLower();
	public static Object Char_isPrint     = new Char_isPrint();
	public static Object Char_isPunct     = new Char_isPunct();
	public static Object Char_isSpace     = new Char_isSpace();
	public static Object Char_isUpper     = new Char_isUpper();
	public static Object Char_toLower     = new Char_toLower();
	public static Object Char_toUpper     = new Char_toUpper();

	public static Object Future_Future     = "Future.Future";
	public static Object Future_alarmQuote = new Future_alarmQuote();
	public static Object Future_await      = new Future_await();
	public static Object Future_awaitOne   = new Future_awaitOne();
	public static Object Future_byneed     = new Future_byneed();
	public static Object Future_concur     = new Future_concur();
	public static Object Future_isFailed   = new Future_isFailed();
	public static Object Future_isFuture   = new Future_isFuture();

	public static Object General_Chr       = "General.Chr";
	public static Object General_Div       = "General.Div";
	public static Object General_Domain    = "General.Domain";
	public static Object General_Fail      = "General.Fail";
	public static Object General_Overflow  = "General.Overflow";
	public static Object General_Size      = "General.Size";
	public static Object General_Span      = "General.Span";
	public static Object General_Subscript = "General.Subscript";
	public static Object General_Match     = "General.Match";
	public static Object General_Bind      = "General.Bind";
	public static Object General_EQUAL     = (Int32) 0;
	public static Object General_LESS      = (Int32) 2;
	public static Object General_GREATER   = (Int32) 1;
	public static Object General_assign    = new General_assign();
	public static Object General_exchange  = new General_exchange();
	public static Object General_exnName   = new General_exnName();
	public static Object General_ref      = new RefConstructor();

	public static Object GlobalStamp_new        = new GlobalStamp_new();
	public static Object GlobalStamp_fromString = new GlobalStamp_fromString();
	public static Object GlobalStamp_toString   = new GlobalStamp_toString();
	public static Object GlobalStamp_compare    = new GlobalStamp_compare();
	public static Object GlobalStamp_hash       = new GlobalStamp_hash();

	public static Object Hole_Cyclic   = "Hole.Cyclic";
	public static Object Hole_Hole     = "Hole.Hole";
	public static Object Hole_fail     = new Hole_fail();
	public static Object Hole_fill     = new Hole_fill();
	public static Object Hole_hole     = new Hole_hole();
	public static Object Hole_future   = new Hole_future();
	public static Object Hole_isFailed = new Hole_isFailed();
	public static Object Hole_isHole   = new Hole_isHole();

	public static Object Int_minInt      = Int32.MinValue;
	public static Object Int_maxInt      = Int32.MaxValue;
	public static Object Int_precision   = (Int32) 32;
	public static Object Int_opnegate    = new Int_opnegate();
	public static Object Int_opadd       = new Int_opadd();
	public static Object Int_opsub       = new Int_opsub();
	public static Object Int_opmul       = new Int_opmul();
	public static Object Int_opless      = new Int_opless();
	public static Object Int_opgreater   = new Int_opgreater();
	public static Object Int_oplessEq    = new Int_oplessEq();
	public static Object Int_opgreaterEq = new Int_opgreaterEq();
	public static Object Int_abs         = new Int_abs();
	public static Object Int_compare     = new Int_compare();
	public static Object Int_div         = new Int_div();
	public static Object Int_mod         = new Int_mod();
	public static Object Int_quot        = new Int_quot();
	public static Object Int_rem         = new Int_rem();
	public static Object Int_toString    = new Int_toString();

	public static Object List_Empty = "List.Empty";
	public static Object List_nil   = (Int32) 1;
	public static Object List_cons  = (Int32) 0;

	public static Object Math_e     = System.Math.E;
	public static Object Math_pi    = System.Math.PI;
	public static Object Math_acos  = new Math_acos();
	public static Object Math_acosh = new Math_acosh();
	public static Object Math_asin  = new Math_asin();
	public static Object Math_asinh = new Math_asinh();
	public static Object Math_atan  = new Math_atan();
	public static Object Math_atanh = new Math_atanh();
	public static Object Math_atan2 = new Math_atan2();
	public static Object Math_cos   = new Math_cos();
	public static Object Math_cosh  = new Math_cosh();
	public static Object Math_exp   = new Math_exp();
	public static Object Math_ln    = new Math_ln();
	public static Object Math_pow   = new Math_pow();
	public static Object Math_sin   = new Math_sin();
	public static Object Math_sinh  = new Math_sinh();
	public static Object Math_sqrt  = new Math_sqrt();
	public static Object Math_tan   = new Math_tan();
	public static Object Math_tanh  = new Math_tanh();

	public static Object Option_Option  = "Option.Option";
	public static Object Option_NONE    = (Int32) 0;
	public static Object Option_SOME    = (Int32) 1;

	public static Object Real_negInf      = System.Double.NegativeInfinity;
	public static Object Real_posInf      = System.Double.PositiveInfinity;
	public static Object Real_opnegate    = new Real_opnegate();
	public static Object Real_opadd       = new Real_opadd();
	public static Object Real_opsub       = new Real_opsub();
	public static Object Real_opmul       = new Real_opmul();
	public static Object Real_opdiv       = new Real_opdiv();
	public static Object Real_opless      = new Real_opless();
	public static Object Real_opgreater   = new Real_opgreater();
	public static Object Real_oplessEq    = new Real_oplessEq();
	public static Object Real_opgreaterEq = new Real_opgreaterEq();
	public static Object Real_ceil        = new Real_ceil();
	public static Object Real_compare     = new Real_compare();
	public static Object Real_floor       = new Real_floor();
	public static Object Real_fromInt     = new Real_fromInt();
	public static Object Real_realCeil    = new Real_realCeil();
	public static Object Real_realFloor   = new Real_realFloor();
	public static Object Real_realRound   = new Real_realRound();
	public static Object Real_realTrunc   = new Real_realTrunc();
	public static Object Real_rem         = new Real_rem();
	public static Object Real_round       = new Real_round();
	public static Object Real_toString    = new Real_toString();
	public static Object Real_trunc       = new Real_trunc();

	public static Object String_maxSize     = Int32.MaxValue;
	public static Object String_append      = new String_append();
	public static Object String_opless      = new String_opless();
	public static Object String_opgreater   = new String_opgreater();
	public static Object String_oplessEq    = new String_oplessEq();
	public static Object String_opgreaterEq = new String_opgreaterEq();
	public static Object String_compare     = new String_compare();
	public static Object String_explode     = new String_explode();
	public static Object String_implode     = new String_implode();
	public static Object String_size        = new String_size();
	public static Object String_sub         = new String_sub();
	public static Object String_substring   = new String_substring();
	public static Object String_str         = new String_str();

	public static Object Thread_RUNNABLE    = "Thread.RUNNABLE";
	public static Object Thread_BLOCKED     = "Thread.BLOCKED";
	public static Object Thread_TERMINATED  = "Thread.TERMINATED";
	public static Object Thread_Terminate   = new Thread_Terminate();
	public static Object Thread_current     = new Thread_current();
	public static Object Thread_isSuspended = new Thread_isSuspended();
	public static Object Thread_raiseIn     = new Thread_raiseIn();
	public static Object Thread_resume      = new Thread_resume();
	public static Object Thread_state       = new Thread_resume();
	public static Object Thread_suspend     = new Thread_suspend();
	public static Object Thread_yield       = new Thread_yield();

	public static Object Unsafe_Array_sub    = new Unsafe_Array_sub();
	public static Object Unsafe_Array_update = new Unsafe_Array_update();
	public static Object Unsafe_String_sub   = new Unsafe_String_sub();
	public static Object Unsafe_Vector_sub   = new Unsafe_Vector_sub();
	public static Object Unsafe_cast         = new Unsafe_cast();
	public static Object Unsafe_getTag       = new Unsafe_getTag();
	public static Object Unsafe_getValue     = new Unsafe_getValue();

	public static Object Vector_fromList = new Vector_fromList();
	public static Object Vector_maxLen   = Int32.MaxValue;
	public static Object Vector_length   = new Vector_length();
	public static Object Vector_sub      = new Vector_sub();

	public static Object Word_fromIntQuote = new Word_fromIntQuote();
	public static Object Word_fromInt      = new Word_fromInt();
	public static Object Word_toInt        = new Word_toInt();
	public static Object Word_toIntX       = new Word_toIntX();
	public static Object Word_opadd        = new Word_opadd();
	public static Object Word_opsub        = new Word_opsub();
	public static Object Word_opmul        = new Word_opmul();
	public static Object Word_div          = new Word_div();
	public static Object Word_mod          = new Word_mod();
	public static Object Word_orb          = new Word_orb();
	public static Object Word_xorb         = new Word_xorb();
	public static Object Word_andb         = new Word_andb();
	public static Object Word_notb         = new Word_notb();
	public static Object Word_shl          = new Word_shl();
	public static Object Word_shr          = new Word_shr();
	public static Object Word_arithshr     = new Word_arithshr();
	public static Object Word_toString     = new Word_toString();
	public static Object Word_wordSize     = (Int32) 32;
    }
    public class Komponist {
	static public Komponist global_k          = null;
	static System.Collections.Hashtable table = new System.Collections.Hashtable();
	class link : Procedure0 {
	    private System.String Url;
	    public link(System.String url) {
		if (url.StartsWith("x-alice:")) {
		    Url = url.Remove(0, "x-alice:".Length);
		}
		else {
		    Url = url;
		}
	    }
	    public override Object Apply() {
		Console.Write("Komponist: Loading Assembly: ");
		Console.WriteLine(Url);
		Module mod = Assembly.LoadFrom(Url).GetModule(Url);
		if (mod == null) {
		    Console.Write("Komponist: Error: loading of ");
		    Console.Write(Url);
		    Console.WriteLine(" failed.");
		    return Prebound.unit;
		}
		else {
		    Console.Write("Komponist: Fetching ");
		    Console.Write(Url);
		    Console.WriteLine("/class Execute");
		    Type type = mod.GetType("Execute");
		    if (type == null) {
			Console.Write("Komponist: Error: Fetch of ");
			Console.Write(Url);
			Console.WriteLine("/class Execute failed.\n");
			return Prebound.unit;
		    }
		    else {
			Console.Write("Komponist: Fetching Method ");
			Console.Write(Url);
			Console.WriteLine("/class Execute::Main");
			MethodInfo minf = type.GetMethod("Main");
			Object[] args   = new Object[1];
			args[0]         = Komponist.global_k;

			if (minf == null) {
			    Console.Write("Komponist: Error: Fetch of Method ");
			    Console.Write(Url);
			    Console.Write("/class Execute::Main failed.");
			    return Prebound.unit;
			}
			else {
			    Object val;

			    Console.Write("Komponist: Invoking ");
			    Console.Write(Url);
			    Console.WriteLine("/class Execute::Main");
			    val = minf.Invoke(null, args);
			    Console.Write("Komponist: Finished ");
			    Console.Write(Url);
			    Console.WriteLine("/class Execute::Main");
			    return val;
			}
		    }
		}
	    }
	}
	public Object Import(System.String url) {
	    if (table.ContainsKey(url)) {
		return table[url];
	    }
	    else {
		Object val = new Byneed(new link(url));
		table.Add(url, val);
		return val;
	    }
	}
    }
}
