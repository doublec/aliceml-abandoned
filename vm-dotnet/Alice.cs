//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt, 2000-2003
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

using System;
using System.Threading;
using System.Reflection;

using Alice;
using Alice.Values;
using Alice.Builtins;

namespace Alice {
    namespace Values {
	public class Array {
	    public object[] Value;
	    public Array(object[] value) {
		Value = value;
	    }
	}
	public abstract class Procedure {
	    public abstract object Apply(object a);
	    public virtual object Apply() {
		return Apply(Prebound.unit);
	    }
	    public virtual object Apply(object a, object b) {
		return Apply(new object[2] {a, b});
	    }
	    public virtual object Apply(object a, object b, object c) {
		return Apply(new object[3] {a, b, c});
	    }
	    public virtual object Apply(object a, object b, object c, 
					object d) {
		return Apply(new object[4] {a, b, c, d});
	    }
	    public virtual object Apply(object a, object b, object c, 
					object d, object e) {
		return Apply(new object[5] {a, b, c, d, e});
	    }
	    public virtual object Apply(object a, object b, object c,
					object d, object e, object f) {
		return Apply(new object[6] {a, b, c, d, e, f});
	    }
	    public virtual object Apply(object a, object b, object c,
					object d, object e, object f,
					object g) {
		return Apply(new object[7] {a, b, c, d, e, f, g});
	    }
	    public virtual object Apply(object a, object b, object c,
					object d, object e, object f,
					object g, object h) {
		return Apply(new object[8] {a, b, c, d, e, f, g, h});
	    }
	    public virtual object Apply(object a, object b, object c,
					object d, object e, object f,
					object g, object h, object i) {
		return Apply(new object[9] {a, b, c, d, e, f, g, h, i});
	    }
	}
	public abstract class Procedure0: Procedure {
	    public override object Apply(object obj) {
		CommonOp.Sync(obj);   // always returns unit value
		return Apply();
	    }
	}
	public abstract class Procedure2: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1]);
	    }
	}
	public abstract class Procedure3: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2]);
	    }
	}
	public abstract class Procedure4: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3]);
	    }
	}
	public abstract class Procedure5: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3], a[4]);
	    }
	}
	public abstract class Procedure6: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3], a[4], a[5]);
	    }
	}
	public abstract class Procedure7: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
	    }
	}
	public abstract class Procedure8: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
	    }
	}
	public abstract class Procedure9: Procedure {
	    public override object Apply(object obj) {
		object[] a = (object[]) CommonOp.Sync(obj);
		return Apply(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
	    }
	}
	public class Cell {
	    public object Value;
	    public Cell(object value) {
		Value = value;
	    }
	    public void Assign(object value) {
		lock (this)
		    Value = value;
	    }
	    public object Access() {
		return Value;
	    }
	    public object Exchange(object value) {
		object res;
		lock (this) {
		    res = Value;
		    Value = value;
		}
		return res;
	    }
	}
	public abstract class Transient: Procedure {
	    public abstract object Deref();
	    public abstract object Await();
	    public abstract bool IsVariantOf(Transient t);
	    public abstract bool IsFailed();
	    public override object Apply(object a) {
		return ((Procedure) Await()).Apply(a);
	    }
	    public override object Apply(object a, object b) {
		return ((Procedure) Await()).Apply(a, b);
	    }
	    public override object Apply(object a, object b, object c) {
		return ((Procedure) Await()).Apply(a, b, c);
	    }
	    public override object Apply(object a, object b, object c,
					 object d) {
		return ((Procedure) Await()).Apply(a, b, c, d);
	    }
	    public override object Apply(object a, object b, object c,
					 object d, object e) {
		return ((Procedure) Await()).Apply(a, b, c, d, e);
	    }
	    public override object Apply(object a, object b, object c,
					 object d, object e, object f) {
		return ((Procedure) Await()).Apply(a, b, c, d, e, f);
	    }
	    public override object Apply(object a, object b, object c,
					 object d, object e, object f,
					 object g) {
		return ((Procedure) Await()).Apply(a, b, c, d, e, f, g);
	    }
	    public override object Apply(object a, object b, object c,
					 object d, object e, object f,
					 object g, object h) {
		return ((Procedure) Await()).Apply(a, b, c, d, e, f, g, h);
	    }
	    public override object Apply(object a, object b, object c,
					 object d, object e, object f,
					 object g, object h, object i) {
		return ((Procedure) Await()).Apply(a, b, c, d, e, f, g, h, i);
	    }
	}
	public class FailedTransient: Transient {
	    private object Exn;
	    public FailedTransient(object exn) {
		Exn = exn;
	    }
	    public override object Deref() {
		return this;
	    }
	    public override object Await() {
		throw new Exception(Exn);
	    }
	    public override bool IsVariantOf(Transient t) {
		return false;
	    }
	    public override bool IsFailed() {
		return true;
	    }
	}
	public class Hole: Transient {
	    private object Ref;
	    public Hole() {
		Ref = null;
	    }
	    public override object Deref() {
		lock (this) {
		    if (Ref == null) {
			return this;
		    } else if (Ref is Transient) {
			return ((Transient) Ref).Deref();
		    } else {
			return Ref;
		    }
		}
	    }
	    public override object Await() {
		lock (this) {
		    if (Ref == null) {
			throw new Exception(Prebound.Hole_Hole);
		    }
		    if (Ref is Transient) {
			return ((Transient) Ref).Await();
		    } else {
			return Ref;
		    }
		}
	    }
	    public object AwaitInternal() {
		lock (this) {
		    if (Ref == null) {
			Monitor.Wait(this);
		    }
		    if (Ref is Transient) {
			return ((Transient) Ref).Await();
		    } else {
			return Ref;
		    }
		}
	    }
	    public override bool IsVariantOf(Transient t) {
		lock (this) {
		    return
			this == t ||
			Ref != null && Ref is Transient &&
			((Transient) Ref).IsVariantOf(t);
		}
	    }
	    public override bool IsFailed() {
		lock (this) {
		    return
			Ref != null && Ref is Transient &&
			((Transient) Ref).IsFailed();
		}
	    }
	    public void Fill(object x) {
		lock (this) {
		    if (Ref == null) {
			if (x is Transient &&
			    ((Transient) x).IsVariantOf(this)) {
			    throw new Exception(Prebound.Hole_Cyclic);
			}
			Ref = x;
			Monitor.PulseAll(this);
		    } else {
			throw new Exception(Prebound.Hole_Hole);
		    }
		}
	    }
	}
	public class Future: Transient {
	    private Hole Ref;
	    public Future(Hole hole) {
		Ref = hole;
	    }
	    public override object Deref() {
		object r = Ref.Deref();
		return r != Ref? r: this;
	    }
	    public override object Await() {
		return Ref.AwaitInternal();
	    }
	    public override bool IsVariantOf(Transient t) {
		return Ref.IsVariantOf(t);
	    }
	    public override bool IsFailed() {
		return Ref.IsFailed();
	    }
	}
	public class Byneed: Transient {
	    private enum ByneedState { Fresh, Bound, Failed };
	    private ByneedState State;
	    private object Ref;
	    public Byneed(Procedure proc) {
		State = ByneedState.Fresh;
		Ref = proc;
	    }
	    public override object Deref() {
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
	    public override object Await() {
		lock (this) {
		    switch (State) {
		    case ByneedState.Bound:
			return Ref;
		    case ByneedState.Failed:
			throw new Exception(Ref);
		    default:
			try {
			    object r = ((Procedure) Ref).Apply();
			    if (r is Transient) {
				Transient t = (Transient) r;
				if (t.IsVariantOf(this)) {
				    throw new Exception(Prebound.Hole_Cyclic);
				}
				State = ByneedState.Bound;
				Ref = t.Await();
			    } else {
				State = ByneedState.Bound;
				Ref = r;
			    }
			} catch (Values.Exception exn) {
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
	public class Selector: Procedure0 {
	    object Value;
	    int Index;
	    public Selector(object x, int n) {
		Value = x;
		Index = n;
	    }
	    public override object Apply() {
		System.Array array = (System.Array) CommonOp.Sync(Value);
		return array.GetValue(Index);
	    }
	}
	public class TagVal {
	    int Tag;
	    public object Value;
	    public TagVal(int tag, object value) {
		Tag = tag;
		Value = value;
	    }
	    public int GetTag() {
		return Tag;
	    }
	    public override bool Equals(object a) {
		if (a is TagVal) {
		    TagVal tagVal = (TagVal) a;
		    return Tag == tagVal.Tag && Value.Equals(tagVal.Value);
		} else {
		    return false;
		}
	    }
	    public override int GetHashCode() {
		//--** improve; attention: may be cyclic
		return Tag;
	    }
	}
	public class ConVal {
	    object Id;
	    public object Value;
	    public ConVal(object id, object value) {
		Id = CommonOp.Sync(id);
		Value = value;
	    }
	    public object GetId() {
		return Id;
	    }
	    public override bool Equals(object a) {
		if (a is ConVal) {
		    ConVal conVal = (ConVal) a;
		    return Id.Equals(conVal.Id) && Value.Equals(conVal.Value);
		} else {
		    return false;
		}
	    }
	    public override int GetHashCode() {
		//--** improve; attention: may be cyclic
		return Id.GetHashCode();
	    }
	}
	public class Exception: SystemException {
	    public object Value;
	    public Exception(object exn): base(exn.ToString()) {
		Value = exn;
	    }
	    public Exception(object exn, int line):
		base(exn.ToString() + " at line " + line.ToString()) {
		Value = exn;
	    }
	}
    }
    public class CommonOp {
	public static object Sync(object obj) {
	    if (obj is Transient) {
		return ((Transient) obj).Await();
	    } else {
		return obj;
	    }
	}
	public static object BtI(bool b) {
	    return b? Prebound.Bool_true: Prebound.Bool_false;
	}
	public static bool ItB(object o) {
	    return (Int32) o == 1;
	}
	public new static bool Equals(object a, object b) {
	    a = CommonOp.Sync(a);
	    b = CommonOp.Sync(b);
	    if (a is System.Array) {
		object[] ar = (object[]) a;
		object[] br = (object[]) b;
		int al = ar.Length;
		for (int i = 0; i < al; i++) {
		    if (!Equals(ar[i], br[i]))
			return false;
		}
		return true;
	    } else if (a is TagVal && b is TagVal) {
		TagVal at = (TagVal) a;
		TagVal bt = (TagVal) b;
		return at.GetTag() == bt.GetTag() &&
		    Equals(at.Value, bt.Value);
	    } else if (a is ConVal && b is ConVal) {
		ConVal at = (ConVal) a;
		ConVal bt = (ConVal) b;
		return at.GetId() == bt.GetId() &&
		    Equals(at.Value, bt.Value);
	    } else if (a is Cell) {
		return a == b;
	    } else if (a is Values.Array) {
		return a == b;
	    } else {
		return a.Equals(b);
	    }
	}
    }
    public class ListOp {
	public static bool IsNil(object obj) {
	    return obj is Int32;
	}
	public static bool IsCons(object obj) {
	    return obj is TagVal;
	}
	public static object Car(object obj) {
	    // Precondition: obj is no transient
	    return ((object[]) ((TagVal) obj).Value)[0];
	}
	public static object Cdr(object obj) {
	    // Precondition: obj is no transient
	    return ((object[]) ((TagVal) obj).Value)[1];
	}
	public static TagVal Cons(object car, object cdr) {
	    return new TagVal(0, new object[2] {car, cdr});
	}
	public static int Length(object obj) {
	    int n = 0;
	    // Precondition: obj is no transient
	    while (!IsNil(obj)) {
		object[] pair = (object[]) ((TagVal) obj).Value;
		obj = CommonOp.Sync(pair[1]);
		pair[1] = obj;   // path compression
		n++;
	    }
	    return n;
	}
    }
    namespace Builtins {
	public class op_Equality: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(CommonOp.Equals(a, b));
	    }
	    public override object Apply(object a, object b) {
		return CommonOp.BtI(CommonOp.Equals(a, b));
	    }
	}
	public class op_Inequality: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(!CommonOp.Equals(a, b));
	    }
	    public override object Apply(object a, object b) {
		return CommonOp.BtI(!CommonOp.Equals(a, b));
	    }
	}
	public class Array_array: Procedure2 {
	    public static object StaticApply(object n, object x) {
		int elems = (Int32) CommonOp.Sync(n);
		object[] a = new object[elems];
		for (int i = 0; i < elems; i++)
		    a[i] = x;
		return new Values.Array(a);
	    }
	    public override object Apply(object n, object x) {
		return StaticApply(n, x);
	    }
	}
	public class Array_fromList: Procedure {
	    public static object StaticApply(object x) {
		x = CommonOp.Sync(x);
		int n = ListOp.Length(x);
		object[] a = new object[n];
		for (int i = 0; i < n; i++, x = ListOp.Cdr(x))
		    a[i] = ListOp.Car(x);
		return new Values.Array(a);
	    }
	    public override object Apply(object x) {
		return StaticApply(x);
	    }
	}
	public class Array_length: Procedure {
	    public static object StaticApply(object x) {
		return ((Values.Array) CommonOp.Sync(x)).Value.Length;
	    }
	    public override object Apply(object x) {
		return StaticApply(x);
	    }
	}
	public class Array_sub: Procedure2 {
	    public static object StaticApply(object a, object i) {
		int index = (Int32) CommonOp.Sync(i);
		try {
		    return (((Values.Array) CommonOp.Sync(a)).Value)[index];
		} catch (IndexOutOfRangeException) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override object Apply(object a, object i) {
		return StaticApply(a, i);
	    }
	}
	public class Array_update: Procedure3 {
	    public static object StaticApply(object a, object i, object x) {
		int index = (Int32) CommonOp.Sync(i);
		try {
		    (((Values.Array) CommonOp.Sync(a)).Value)[index] = x;
		    return Prebound.unit;
		} catch (IndexOutOfRangeException) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override object Apply(object a, object i, object x) {
		return StaticApply(a, i, x);
	    }
	}
	public class Char_op_LessThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Char) CommonOp.Sync(a) <
				    (Char) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_op_GreaterThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Char) CommonOp.Sync(a) >
				    (Char) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_op_LessThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Char) CommonOp.Sync(a) <=
				    (Char) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_op_GreaterThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Char) CommonOp.Sync(a) >=
				    (Char) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Char_ord: Procedure {
	    public static object StaticApply(object a) {
		return (int) (Char) CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_chr: Procedure {
	    public static object StaticApply(object a) {
		int i = (Int32) CommonOp.Sync(a);
		if (i >= Char.MinValue && i <= Char.MaxValue) {
		    return (char) i;
		} else {
Console.WriteLine(i);
		    throw new Values.Exception(Prebound.General_Chr);
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isAlpha: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsLetter((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isAlphaNum: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsLetterOrDigit((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isCntrl: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsControl((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isDigit: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsDigit((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isGraph: Procedure {
	    public static object StaticApply(object a) {
		char c = (Char) CommonOp.Sync(a);
		return CommonOp.BtI(!Char.IsControl(c) &&
				    !Char.IsWhiteSpace(c));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isHexDigit: Procedure {
	    public static object StaticApply(object a) {
		char c = (Char) CommonOp.Sync(a);
		return c >= '0' && c <= '9' ||
		    c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f';
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isLower: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsLower((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isPrint: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(!Char.IsControl((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isPunct: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsPunctuation((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isSpace: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsWhiteSpace((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_isUpper: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(Char.IsUpper((Char) CommonOp.Sync(a)));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_toLower: Procedure {
	    public static object StaticApply(object a) {
		return Char.ToLower((Char) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Char_toUpper: Procedure {
	    public static object StaticApply(object a) {
		return Char.ToUpper((Char) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class General_exchange: Procedure2 {
	    public static object StaticApply(object c, object v) {
		return ((Cell) CommonOp.Sync(c)).Exchange(v);
	    }
	    public override object Apply(object c, object v) {
		return StaticApply(c, v);
	    }
	}
	public class General_exnName: Procedure {
	    public static object StaticApply(object v) {
		v = CommonOp.Sync(v);
		if (v is ConVal) {
		    v = ((ConVal) v).GetId();
		}
		if (v is String) {
		    return v;
		} else {   // v is Guid
		    return "";   //--**
		}
	    }
	    public override object Apply(object v) {
		return StaticApply(v);
	    }
	}
	public class Future_alarmQuote: Procedure {
	    class AlarmThread {
		Hole H;
		int MS;
		public AlarmThread(Hole hole, int msecs) {
		    H  = hole;
		    MS = msecs;
		}
		public void Run() {
		    Thread.Sleep((MS + 500) / 1000);
		    H.Fill(Prebound.unit);
		}
	    }
	    public static object StaticApply(object obj) {
		Hole hole = new Hole();
		int msecs = (Int32) CommonOp.Sync(obj);
		AlarmThread t = new AlarmThread(hole, msecs);
		new Thread(new ThreadStart(t.Run)).Start();
		return new Future(hole);
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_await: Procedure {
	    public static object StaticApply(object obj) {
		if (obj is Transient) {
		    return ((Transient) obj).Await();
		} else {
		    return obj;
		}
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_awaitOne: Procedure2 {
	    class AwaitThread {
		object W;
		object N;
		public AwaitThread(object w, object n) {
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
	    public static object StaticApply(object a, object b) {
		object n = new object();
		Thread at =
		    new Thread(new ThreadStart(new AwaitThread(a, n).Run));
		Thread bt =
		    new Thread(new ThreadStart(new AwaitThread(b, n).Run));
		at.Start();
		bt.Start();
		Monitor.Wait(n);
		at.Abort();
		bt.Abort();
		return a;
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Future_byneed: Procedure {
	    public static object StaticApply(object obj) {
		return new Byneed((Procedure) CommonOp.Sync(obj));
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_concur: Procedure {
	    class ConcurThread {
		Hole H;
		Procedure F;
		public ConcurThread(Hole h, Procedure f) {
		    H = h;
		    F = f;
		}
		public void Run() {
		    try {
			H.Fill(F.Apply());
		    } catch (Values.Exception e) {
			H.Fill(new FailedTransient(new ConVal(Prebound.Future_Future, e.Value)));
		    }
		}
	    }
	    public static object StaticApply(object obj) {
		Hole hole = new Hole();
		Procedure f = (Procedure) CommonOp.Sync(obj);
		new Thread(new ThreadStart(new ConcurThread(hole, f).Run)).Start();
		return new Future(hole);
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_isFailed: Procedure {
	    public static object StaticApply(object obj) {
		return CommonOp.BtI(obj is Transient &&
				    ((Transient) obj).IsFailed());
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class Future_isFuture: Procedure {
	    public static object StaticApply(object obj) {
		if (obj is Transient) {
		    obj = ((Transient) obj).Deref();
		    return CommonOp.BtI(obj is Future || obj is Byneed ||
					obj is FailedTransient);
		} else {
		    return 0;
		}
	    }
	    public override object Apply(object obj) {
		return StaticApply(obj);
	    }
	}
	public class GlobalStamp_new: Procedure0 {
	    public static object StaticApply() {
		return new Guid();
	    }
	    public override object Apply() {
		return StaticApply();
	    }
	}
	public class GlobalStamp_fromString: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class GlobalStamp_toString: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.Sync(a).ToString();
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class GlobalStamp_compare: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int val = String.Compare((string) CommonOp.Sync(a),
					 (string) CommonOp.Sync(b));
		if (val < 0) {
		    return Prebound.General_LESS;
		} else if (val == 0) {
		    return Prebound.General_EQUAL;
		} else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class GlobalStamp_hash: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.Sync(a).GetHashCode();
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_fail: Procedure2 {
	    public static object StaticApply(object a, object b) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    object exn = new ConVal(Prebound.Future_Future, b);
		    ((Hole) a).Fill(new FailedTransient(exn));
		} else {
		    throw new Values.Exception(Prebound.Hole_Hole);
		}
		return Prebound.unit;
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Hole_fill: Procedure2 {
	    public static object StaticApply(object a, object b) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    ((Hole) a).Fill(b);
		} else {
		    throw new Values.Exception(Prebound.Hole_Hole);
		}
		return Prebound.unit;
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Hole_future: Procedure {
	    public static object StaticApply(object a) {
		if (a is Transient) {
		    a = ((Transient) a).Deref();
		}
		if (a is Hole) {
		    return new Future((Hole) a);
		} else {
		    return a;
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_hole: Procedure0 {
	    public static object StaticApply() {
		return new Hole();
	    }
	    public override object Apply() {
		return StaticApply();
	    }
	}
	public class Hole_isFailed: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.BtI(a is Transient &&
				    ((Transient) a).IsFailed());
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Hole_isHole: Procedure {
	    public static object StaticApply(object a) {
		if (a is Transient) {
		    return CommonOp.BtI(((Transient) a).Deref() is Hole);
		} else {
		    return 0;
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Int_op_UnaryNegation: Procedure {
	    public static object StaticApply(object a) {
		try {
		    int t = (Int32) CommonOp.Sync(a);
		    checked {
			return -t;
		    }
		} catch (OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Int_op_Addition: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    checked {
			return (Int32) CommonOp.Sync(a) + (Int32) CommonOp.Sync(b);
		    }
		} catch (OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_Subtraction: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    checked {
			return (Int32) CommonOp.Sync(a) - (Int32) CommonOp.Sync(b);
		    }
		} catch (OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_Multiply: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    checked {
			return (Int32) CommonOp.Sync(a) * (Int32) CommonOp.Sync(b);
		    }
		} catch (OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_LessThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Int32) CommonOp.Sync(a) < (Int32) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_GreaterThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Int32) CommonOp.Sync(a) > (Int32) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_LessThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Int32) CommonOp.Sync(a) <= (Int32) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_op_GreaterThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Int32) CommonOp.Sync(a) >= (Int32) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_abs: Procedure {
	    public static object StaticApply(object a) {
		try {
		    return Math.Abs((Int32) CommonOp.Sync(a));
		} catch (OverflowException) {
		    throw new Values.Exception(Prebound.General_Overflow);
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Int_compare: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int ai = (Int32) CommonOp.Sync(a);
		int bi = (Int32) CommonOp.Sync(b);
		if (ai == bi) {
		    return Prebound.General_EQUAL;
		} else if (ai < bi) {
		    return Prebound.General_LESS;
		} else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_div: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int ai = (Int32) CommonOp.Sync(a);
		int bi = (Int32) CommonOp.Sync(b);
		bool b1 = ai >= 0;
		bool b2 = bi >= 0;
		try {
		    if (b1 == b2) {
			return ai / bi;
		    } else if (b2) {
			return (ai - bi + 1) / bi;
		    } else {
			return (ai - bi - 1) / bi;
		    }
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_mod: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    int ai = (Int32) CommonOp.Sync(a);
		    int bi = (Int32) CommonOp.Sync(b);
		    int c = ai % bi;
		    if (c == 0) {
			return c;
		    } else if (c < 0) {
			if (bi <= 0) {
			    return c;
			} else {
			    return c + bi;
			}
		    } else {   // c > 0
			if (bi < 0) {
			    return c + bi;
			} else {
			    return c;
			}
		    }
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_quot: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    return (Int32) CommonOp.Sync(a) / (Int32) CommonOp.Sync(b);
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_rem: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    return (Int32) CommonOp.Sync(a) % (Int32) CommonOp.Sync(b);
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Int_toString: Procedure {
	    public static object StaticApply(object a) {
		return ((Int32) CommonOp.Sync(a)).ToString();
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_acos: Procedure {
	    public static object StaticApply(object a) {
		return Math.Acos((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_acosh: Procedure {
	    public static object StaticApply(object a) {
		double x = (Double) CommonOp.Sync(a);
		return Math.Log(x + Math.Sqrt(x * x - 1.0));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_asin: Procedure {
	    public static object StaticApply(object a) {
		return Math.Asin((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_asinh: Procedure {
	    public static object StaticApply(object a) {
		double x = (Double) CommonOp.Sync(a);
		return Math.Log(x + Math.Sqrt(x * x + 1.0));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_atan: Procedure {
	    public static object StaticApply(object a) {
		return Math.Atan((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_atanh: Procedure {
	    public static object StaticApply(object a) {
		double x = (Double) CommonOp.Sync(a);
		if (Math.Abs(x) > 1.0) {
		    return Math.Asin(2.0);
		} else {
		    return Math.Log((1.0 + x) / (1.0 - x));
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_atan2: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return Math.Atan2((Double) CommonOp.Sync(a), (Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Math_cos: Procedure {
	    public static object StaticApply(object a) {
		return Math.Cos((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_cosh: Procedure {
	    public static object StaticApply(object a) {
		double c = (Double) CommonOp.Sync(a);
		return (Math.Pow(Math.E, c) + Math.Pow(Math.E, -c)) / 2.0;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_exp: Procedure {
	    public static object StaticApply(object a) {
		return Math.Exp((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_ln: Procedure {
	    public static object StaticApply(object a) {
		return Math.Log((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_pow: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return Math.Pow((Double) CommonOp.Sync(a),
				(Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Math_sin: Procedure {
	    public static object StaticApply(object a) {
		return Math.Sin((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_sinh: Procedure {
	    public static object StaticApply(object a) {
		double x = (Double) CommonOp.Sync(a);
		return (Math.Pow(Math.E, x) - Math.Pow(Math.E, -x)) / 2.0;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_sqrt: Procedure {
	    public static object StaticApply(object a) {
		return Math.Sqrt((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_tan: Procedure {
	    public static object StaticApply(object a) {
		return Math.Tan((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Math_tanh: Procedure {
	    public static object StaticApply(object a) {
		double x   = (Double) CommonOp.Sync(a);
		double ex  = (Double) Math.Pow(Math.E, x);
		double emx = (Double) Math.Pow(Math.E, -x);
		return (ex - emx) / (ex + emx);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_op_UnaryNegation: Procedure {
	    public static object StaticApply(object a) {
		return -(Double) CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_op_Addition: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Double) CommonOp.Sync(a) + (Double) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_Subtraction: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Double) CommonOp.Sync(a) - (Double) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_Multiply: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Double) CommonOp.Sync(a) * (Double) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_Division: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Double) CommonOp.Sync(a) / (Double) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_LessThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Double) CommonOp.Sync(a) <
				    (Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_GreaterThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Double) CommonOp.Sync(a) >
				    (Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_LessThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Double) CommonOp.Sync(a) <=
				    (Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_op_GreaterThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI((Double) CommonOp.Sync(a) >=
				    (Double) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_ceil: Procedure {
	    public static object StaticApply(object a) {
		return (int) Math.Ceiling((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_compare: Procedure2 {
	    public static object StaticApply(object a, object b) {
		double ai = (Double) CommonOp.Sync(a);
		double bi = (Double) CommonOp.Sync(b);
		if (ai == bi) {
		    return Prebound.General_EQUAL;
		} else if (ai < bi) {
		    return Prebound.General_LESS;
		} else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_floor: Procedure {
	    public static object StaticApply(object a) {
		return (int) Math.Floor((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_fromInt: Procedure {
	    public static object StaticApply(object a) {
		return (double) ((Int32) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realCeil: Procedure {
	    public static object StaticApply(object a) {
		return Math.Ceiling((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realFloor: Procedure {
	    public static object StaticApply(object a) {
		return Math.Floor((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realRound: Procedure {
	    public static object StaticApply(object a) {
		return Math.Round((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_realTrunc: Procedure {
	    public static object StaticApply(object a) {
		double x = (Double) CommonOp.Sync(a);
		if (x >= 0.0) {
		    return Math.Floor(x);
		} else {
		    return Math.Ceiling(x);
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_rem: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    double ai = (Double) CommonOp.Sync(a);
		    double bi = (Double) CommonOp.Sync(b);
		    return ai - ((Int64) (ai / bi)) * bi;   //--**
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Real_round: Procedure {
	    public static object StaticApply(object a) {
		return Math.Round((Double) CommonOp.Sync(a));
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_toString: Procedure {
	    public static object StaticApply(object a) {
		return ((Double) CommonOp.Sync(a)).ToString();   //--** syntax?
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Real_trunc: Procedure {
	    public static object StaticApply(object a) {
		return (int) (Double) CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Ref_op_Assignment: Procedure2 {
	    public static object StaticApply(object c, object v) {
		((Cell) CommonOp.Sync(c)).Assign(v);
		return Prebound.unit;
	    }
	    public override object Apply(object c, object v) {
		return StaticApply(c, v);
	    }
	}
	public class String_op_Concatenation: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return String.Concat((string) CommonOp.Sync(a),
				     (string) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_op_LessThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(String.Compare((string) CommonOp.Sync(a),
						   (string) CommonOp.Sync(b)) < 0);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_op_GreaterThan: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(String.Compare((string) CommonOp.Sync(a),
						   (string) CommonOp.Sync(b)) > 0);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_op_LessThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(String.Compare((string) CommonOp.Sync(a),
						   (string) CommonOp.Sync(b)) <= 0);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_op_GreaterThanOrEqual: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return CommonOp.BtI(String.Compare((string) CommonOp.Sync(a),
						   (string) CommonOp.Sync(b)) >= 0);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_compare: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int v = String.Compare((string) CommonOp.Sync(a),
				       (string) CommonOp.Sync(b));
		if (v < 0) {
		    return Prebound.General_LESS;
		} else if (v == 0) {
		    return Prebound.General_EQUAL;
		} else {
		    return Prebound.General_GREATER;
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_explode: Procedure {
	    public static object StaticApply(object a) {
		string s = (string) CommonOp.Sync(a);
		int len = s.Length;
		if (len == 0) {
		    return 1;
		} else {
		    TagVal head = ListOp.Cons((Char) s[len - 1], (Int32) 1);
		    for (int i = (len - 2); i >= 0; i--) {
			head = ListOp.Cons(s[i], head);
		    }
		    return head;
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class String_implode: Procedure {
	    public static object StaticApply(object a) {
		System.Text.StringBuilder s = new System.Text.StringBuilder();
		object head = CommonOp.Sync(a);
		while (!(head is Int32)) {
		    s.Append((Char) CommonOp.Sync(ListOp.Car(head)));
		    head = CommonOp.Sync(ListOp.Cdr(head));
		}
		return s.ToString();
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class String_size: Procedure {
	    public static object StaticApply(object a) {
		return ((string) CommonOp.Sync(a)).Length;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class String_sub: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    int index = (Int32) CommonOp.Sync(b);
		    return ((string) CommonOp.Sync(a))[index];
		} catch (ArgumentOutOfRangeException) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class String_substring: Procedure3 {
	    public static object StaticApply(object a, object b, object c) {
		try {
		    int i = (Int32) CommonOp.Sync(b);
		    int j = (Int32) CommonOp.Sync(c);
		    return ((string) CommonOp.Sync(a)).Substring(i, j - i + 1);
		} catch (ArgumentOutOfRangeException) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override object Apply(object a, object b, object c) {
		return StaticApply(a, b, c);
	    }
	}
	public class String_str: Procedure {
	    public static object StaticApply(object a) {
		return ((Char) CommonOp.Sync(a)).ToString();
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_current: Procedure0 {
	    public static object StaticApply() {
		return Thread.CurrentThread;
	    }
	    public override object Apply() {
		return StaticApply();
	    }
	}
	public class Thread_isSuspended: Procedure {
	    public static object StaticApply(object a) {
		Thread t = (Thread) CommonOp.Sync(a);
		return CommonOp.BtI(t.ThreadState == ThreadState.Suspended);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_raiseIn: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return a;   //--** not implemented
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Thread_resume: Procedure {
	    public static object StaticApply(object a) {
		((Thread) CommonOp.Sync(a)).Resume();
		return Prebound.unit;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_state: Procedure {
	    public static object StaticApply(object a) {
		Thread t = (Thread) CommonOp.Sync(a);
		if (t.IsAlive) {
		    return Prebound.Thread_RUNNABLE;
		} else if (t.ThreadState == ThreadState.Suspended) {
		    return Prebound.Thread_BLOCKED;   //--** wrong
		} else if (t.ThreadState == ThreadState.Stopped) {
		    return Prebound.Thread_TERMINATED;
		}
		return Prebound.Thread_RUNNABLE;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_suspend: Procedure {
	    public static object StaticApply(object a) {
		((Thread) CommonOp.Sync(a)).Suspend();
		return Prebound.unit;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Thread_yield: Procedure {
	    public static object StaticApply(object a) {
		return Prebound.unit;   //--** not implemented
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_Array_sub: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int index = (Int32) CommonOp.Sync(b);
		return (((Values.Array) CommonOp.Sync(a)).Value)[index];
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_Array_update: Procedure3 {
	    public static object StaticApply(object a, object b, object c) {
		int index = (Int32) CommonOp.Sync(b);
		(((Values.Array) CommonOp.Sync(a)).Value)[index] = c;
		return Prebound.unit;
	    }
	    public override object Apply(object a, object b, object c) {
		return StaticApply(a, b, c);
	    }
	}
	public class Unsafe_String_sub: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return ((string) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_Vector_sub: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return ((object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Unsafe_cast: Procedure {
	    public static object StaticApply(object a) {
		if (a is Transient) {
		    return ((Transient) a).Deref();
		} else {
		    return a;
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_getTag: Procedure {
	    public static object StaticApply(object a) {
		a = CommonOp.Sync(a);
		if (a is Int32) {
		    return a;
		} else {
		    return ((TagVal) a).GetTag();
		}
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Unsafe_getValue: Procedure {
	    public static object StaticApply(object a) {
		return ((TagVal) CommonOp.Sync(a)).Value;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Vector_fromList: Procedure {
	    public static object StaticApply(object a) {
		a = CommonOp.Sync(a);
		int n = ListOp.Length(a);
		object[] na = new object[n];
		for (int i = 0; i < n; i++) {
		    na[i] = ListOp.Car(a);
		    a = ListOp.Cdr(a);
		}
		return na;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Vector_length: Procedure {
	    public static object StaticApply(object a) {
		return ((object[]) CommonOp.Sync(a)).Length;
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Vector_sub: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    int index = (Int32) CommonOp.Sync(b);
		    return ((object[]) CommonOp.Sync(a))[index];
		} catch (IndexOutOfRangeException) {
		    throw new Values.Exception(Prebound.General_Subscript);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Vector_tabulate: Procedure2 {
	    public static object StaticApply(object a, object b) {
		int length = (Int32) CommonOp.Sync(a);
		Procedure f = (Procedure) CommonOp.Sync(b);
		object[] vector = new object[length];
		for (int i = 0; i < length; i++) {
		    vector[i] = f.Apply(i);
		}
		return vector;
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_Addition: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) + (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_Subtraction: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) - (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_Multiply: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) * (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_LeftShift: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) << (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_UnsignedRightShift: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (int) ((uint) (Int32) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b));
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_op_SignedRightShift: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_andb: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) & (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_fromIntQuote: Procedure2 {
	    public static object StaticApply(object a, object b) {
		if ((Int32) CommonOp.Sync(a) == 32) {
		    return CommonOp.Sync(b);
		} else {
		    throw new Values.Exception(Prebound.General_Domain);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_div: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    return (Int32) CommonOp.Sync(a) / (Int32) CommonOp.Sync(b);
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_mod: Procedure2 {
	    public static object StaticApply(object a, object b) {
		try {
		    return (Int32) CommonOp.Sync(a) % (Int32) CommonOp.Sync(b);
		} catch (DivideByZeroException) {
		    throw new Values.Exception(Prebound.General_Div);
		}
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_notb: Procedure {
	    public static object StaticApply(object a) {
		return ~(Int32) CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Word_orb: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) | (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
	public class Word_toInt: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.Sync(a); //--** overflow?
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Word_toIntX: Procedure {
	    public static object StaticApply(object a) {
		return CommonOp.Sync(a);
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Word_toString: Procedure {
	    public static object StaticApply(object a) {
		return ((Int32) CommonOp.Sync(a)).ToString();   //--** syntax!
	    }
	    public override object Apply(object a) {
		return StaticApply(a);
	    }
	}
	public class Word_xorb: Procedure2 {
	    public static object StaticApply(object a, object b) {
		return (Int32) CommonOp.Sync(a) ^ (Int32) CommonOp.Sync(b);
	    }
	    public override object Apply(object a, object b) {
		return StaticApply(a, b);
	    }
	}
    }
    public class Prebound {
	public static object op_Equality   = new Builtins.op_Equality();
	public static object op_Inequality = new Builtins.op_Inequality();

	public static object unit = 0;

	public static object Array_array    = new Array_array();
	public static object Array_fromList = new Array_fromList();
	public static object Array_length   = new Array_length();
	public static object Array_maxLen   = Int32.MaxValue;
	public static object Array_sub      = new Array_sub();
	public static object Array_update   = new Array_update();

	public static object Bool_false = 0;
	public static object Bool_true  = 1;

	public static object Char_op_LessThan           = new Char_op_LessThan();
	public static object Char_op_GreaterThan        = new Char_op_GreaterThan();
	public static object Char_op_LessThanOrEqual    = new Char_op_LessThanOrEqual();
	public static object Char_op_GreaterThanOrEqual = new Char_op_GreaterThanOrEqual();
	public static object Char_ord         = new Char_ord();
	public static object Char_chr         = new Char_chr();
	public static object Char_isAlpha     = new Char_isAlpha();
	public static object Char_isAlphaNum  = new Char_isAlphaNum();
	public static object Char_isCntrl     = new Char_isCntrl();
	public static object Char_isDigit     = new Char_isDigit();
	public static object Char_isGraph     = new Char_isGraph();
	public static object Char_isHexDigit  = new Char_isHexDigit();
	public static object Char_isLower     = new Char_isLower();
	public static object Char_isPrint     = new Char_isPrint();
	public static object Char_isPunct     = new Char_isPunct();
	public static object Char_isSpace     = new Char_isSpace();
	public static object Char_isUpper     = new Char_isUpper();
	public static object Char_toLower     = new Char_toLower();
	public static object Char_toUpper     = new Char_toUpper();

	public static object Future_Future     = "Future.Future";
	public static object Future_alarmQuote = new Future_alarmQuote();
	public static object Future_await      = new Future_await();
	public static object Future_awaitOne   = new Future_awaitOne();
	public static object Future_byneed     = new Future_byneed();
	public static object Future_concur     = new Future_concur();
	public static object Future_isFailed   = new Future_isFailed();
	public static object Future_isFuture   = new Future_isFuture();

	public static object General_Chr       = "General.Chr";
	public static object General_Div       = "General.Div";
	public static object General_Domain    = "General.Domain";
	public static object General_Fail      = "General.Fail";
	public static object General_Overflow  = "General.Overflow";
	public static object General_Size      = "General.Size";
	public static object General_Span      = "General.Span";
	public static object General_Subscript = "General.Subscript";
	public static object General_Match     = "General.Match";
	public static object General_Bind      = "General.Bind";
	public static object General_EQUAL     = 0;
	public static object General_LESS      = 2;
	public static object General_GREATER   = 1;
	public static object General_exchange      = new General_exchange();
	public static object General_exnName       = new General_exnName();

	public static object GlobalStamp_new        = new GlobalStamp_new();
	public static object GlobalStamp_fromString = new GlobalStamp_fromString();
	public static object GlobalStamp_toString   = new GlobalStamp_toString();
	public static object GlobalStamp_compare    = new GlobalStamp_compare();
	public static object GlobalStamp_hash       = new GlobalStamp_hash();

	public static object Hole_Cyclic   = "Hole.Cyclic";
	public static object Hole_Hole     = "Hole.Hole";
	public static object Hole_fail     = new Hole_fail();
	public static object Hole_fill     = new Hole_fill();
	public static object Hole_hole     = new Hole_hole();
	public static object Hole_future   = new Hole_future();
	public static object Hole_isFailed = new Hole_isFailed();
	public static object Hole_isHole   = new Hole_isHole();

	public static object Int_minInt      = Int32.MinValue;
	public static object Int_maxInt      = Int32.MaxValue;
	public static object Int_precision   = 32;
	public static object Int_op_UnaryNegation      = new Int_op_UnaryNegation();
	public static object Int_op_Addition           = new Int_op_Addition();
	public static object Int_op_Subtraction        = new Int_op_Subtraction();
	public static object Int_op_Multiply           = new Int_op_Multiply();
	public static object Int_op_LessThan           = new Int_op_LessThan();
	public static object Int_op_GreaterThan        = new Int_op_GreaterThan();
	public static object Int_op_LessThanOrEqual    = new Int_op_LessThanOrEqual();
	public static object Int_op_GreaterThanOrEqual = new Int_op_GreaterThanOrEqual();
	public static object Int_abs         = new Int_abs();
	public static object Int_compare     = new Int_compare();
	public static object Int_div         = new Int_div();
	public static object Int_mod         = new Int_mod();
	public static object Int_quot        = new Int_quot();
	public static object Int_rem         = new Int_rem();
	public static object Int_toString    = new Int_toString();

	public static object List_Empty = "List.Empty";
	public static object List_nil   = 1;
	public static object List_cons  = 0;

	public static object Math_e     = Math.E;
	public static object Math_pi    = Math.PI;
	public static object Math_acos  = new Math_acos();
	public static object Math_acosh = new Math_acosh();
	public static object Math_asin  = new Math_asin();
	public static object Math_asinh = new Math_asinh();
	public static object Math_atan  = new Math_atan();
	public static object Math_atanh = new Math_atanh();
	public static object Math_atan2 = new Math_atan2();
	public static object Math_cos   = new Math_cos();
	public static object Math_cosh  = new Math_cosh();
	public static object Math_exp   = new Math_exp();
	public static object Math_ln    = new Math_ln();
	public static object Math_pow   = new Math_pow();
	public static object Math_sin   = new Math_sin();
	public static object Math_sinh  = new Math_sinh();
	public static object Math_sqrt  = new Math_sqrt();
	public static object Math_tan   = new Math_tan();
	public static object Math_tanh  = new Math_tanh();

	public static object Option_Option  = "Option.Option";
	public static object Option_NONE    = 0;
	public static object Option_SOME    = 1;

	public static object Real_op_UnaryNegation      = new Real_op_UnaryNegation();
	public static object Real_op_Addition           = new Real_op_Addition();
	public static object Real_op_Subtraction        = new Real_op_Subtraction();
	public static object Real_op_Multiply           = new Real_op_Multiply();
	public static object Real_op_Division           = new Real_op_Division();
	public static object Real_op_LessThan           = new Real_op_LessThan();
	public static object Real_op_GreaterThan        = new Real_op_GreaterThan();
	public static object Real_op_LessThanOrEqual    = new Real_op_LessThanOrEqual();
	public static object Real_op_GreaterThanOrEqual = new Real_op_GreaterThanOrEqual();
	public static object Real_ceil        = new Real_ceil();
	public static object Real_compare     = new Real_compare();
	public static object Real_floor       = new Real_floor();
	public static object Real_fromInt     = new Real_fromInt();
	public static object Real_realCeil    = new Real_realCeil();
	public static object Real_realFloor   = new Real_realFloor();
	public static object Real_realRound   = new Real_realRound();
	public static object Real_realTrunc   = new Real_realTrunc();
	public static object Real_rem         = new Real_rem();
	public static object Real_round       = new Real_round();
	public static object Real_toString    = new Real_toString();
	public static object Real_trunc       = new Real_trunc();

	public static object Ref_op_Assignment = new Ref_op_Assignment();

	public static object String_op_Concatenation      = new String_op_Concatenation();
	public static object String_op_LessThan           = new String_op_LessThan();
	public static object String_op_GreaterThan        = new String_op_GreaterThan();
	public static object String_op_LessThanOrEqual    = new String_op_LessThanOrEqual();
	public static object String_op_GreaterThanOrEqual = new String_op_GreaterThanOrEqual();
	public static object String_compare     = new String_compare();
	public static object String_explode     = new String_explode();
	public static object String_implode     = new String_implode();
	public static object String_maxSize     = Int32.MaxValue;
	public static object String_size        = new String_size();
	public static object String_sub         = new String_sub();
	public static object String_substring   = new String_substring();
	public static object String_str         = new String_str();

	public static object Thread_RUNNABLE    = "Thread.RUNNABLE";
	public static object Thread_BLOCKED     = "Thread.BLOCKED";
	public static object Thread_TERMINATED  = "Thread.TERMINATED";
	public static object Thread_Terminate   = "Thread.Terminate";
	public static object Thread_current     = new Thread_current();
	public static object Thread_isSuspended = new Thread_isSuspended();
	public static object Thread_raiseIn     = new Thread_raiseIn();
	public static object Thread_resume      = new Thread_resume();
	public static object Thread_state       = new Thread_resume();
	public static object Thread_suspend     = new Thread_suspend();
	public static object Thread_yield       = new Thread_yield();

	public static object Unsafe_Array_sub    = new Unsafe_Array_sub();
	public static object Unsafe_Array_update = new Unsafe_Array_update();
	public static object Unsafe_String_sub   = new Unsafe_String_sub();
	public static object Unsafe_Vector_sub   = new Unsafe_Vector_sub();
	public static object Unsafe_cast         = new Unsafe_cast();
	public static object Unsafe_getTag       = new Unsafe_getTag();
	public static object Unsafe_getValue     = new Unsafe_getValue();

	public static object Vector_fromList = new Vector_fromList();
	public static object Vector_maxLen   = Int32.MaxValue;
	public static object Vector_length   = new Vector_length();
	public static object Vector_sub      = new Vector_sub();
	public static object Vector_tabulate = new Vector_tabulate();

	public static object Word_op_Addition           = new Word_op_Addition();
	public static object Word_op_Subtraction        = new Word_op_Subtraction();
	public static object Word_op_Multiply           = new Word_op_Multiply();
	public static object Word_op_LeftShift          = new Word_op_LeftShift();
	public static object Word_op_UnsignedRightShift = new Word_op_UnsignedRightShift();
	public static object Word_op_SignedRightShift   = new Word_op_SignedRightShift();
	public static object Word_andb         = new Word_andb();
	public static object Word_fromIntQuote = new Word_fromIntQuote();
	public static object Word_div          = new Word_div();
	public static object Word_mod          = new Word_mod();
	public static object Word_notb         = new Word_notb();
	public static object Word_orb          = new Word_orb();
	public static object Word_toInt        = new Word_toInt();
	public static object Word_toIntX       = new Word_toIntX();
	public static object Word_toString     = new Word_toString();
	public static object Word_wordSize     = 32;
	public static object Word_xorb         = new Word_xorb();
    }
    public class Komponist {
	Uri BaseUri;
	static System.Collections.Hashtable table =
	    new System.Collections.Hashtable();
	static void Debug(string message) {
	    Console.WriteLine("Komponist: " + message);
	}
	static void Error(string message) {
	    Console.WriteLine("Komponist: Error: " + message);
	    Environment.Exit(1);
	}
	public Komponist(Uri baseUri) {
	    BaseUri = baseUri;
	}
	class link: Procedure0 {
	    private Uri MyUri;
	    private string Filename;
	    public link(Uri myUri, string filename) {
		MyUri = myUri;
		Filename = filename;
	    }
	    public override object Apply() {
		try {
		    Debug("Load " + Filename);
		    Assembly assembly;
		    try {
			assembly = Assembly.LoadFrom(Filename);
		    } catch (System.IO.FileNotFoundException) {
			assembly = Assembly.LoadFrom(Filename + ".dll");
		    }
		    Type type = assembly.GetType("Execute");
		    if (type == null) {
			Error("GetType " + Filename + "#Execute failed.");
		    }
		    MethodInfo minf = type.GetMethod("Main");
		    if (minf == null) {
			Error("GetMethod " + Filename +
			      "#Execute::Main failed.");
		    }
		    Debug("Apply " + Filename);
		    object[] args = new object[] {new Komponist(MyUri)};
		    object val = minf.Invoke(null, args);
		    Debug("Finished " + Filename);
		    return val;
		} catch (System.Exception e) {
		    Debug("Uncaught exception " + e);
		    object exn =
			new ConVal(Prebound.Future_Future, e);   //--**
		    return new FailedTransient(exn);
		}
	    }
	}
	public static Uri ParseUri(Uri baseUri, string relative) {
	    if (relative.StartsWith("x-alice:")) {
		relative = relative.Remove(0, "x-alice:".Length);
		return new Uri("x-alice://loopback" + relative);
	    } else {
		return new Uri(baseUri, relative);
	    }
	}
	public object Import(string relativeUri) {
	    Uri resolvedUri = ParseUri(BaseUri, relativeUri);
	    if (table.ContainsKey(resolvedUri)) {
		return table[resolvedUri];
	    } else {
		Uri uri = resolvedUri;
		if (uri.Scheme.Equals("x-alice")) {
		    string home =
			Environment.GetEnvironmentVariable("ALICE_HOME");
		    if (home == null) {
			Error("Environment variable ALICE_HOME not set.");
		    }
		    uri = new Uri(home + uri.AbsolutePath);
		}
		object val = new Byneed(new link(resolvedUri, uri.LocalPath));
		table.Add(resolvedUri, val);
		return val;
	    }
	}
    }
}
