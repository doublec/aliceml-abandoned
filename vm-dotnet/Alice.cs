using System;
using System.Threading;
using System.IO;

namespace Alice {
    class Prebound {
	public static Object unit = null;
	public class Transient {
	    public static Object Future = "Transient.Future";
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
	    public static Object LESS      = (Int32) 1;
	    public static Object GREATER   = (Int32) 2;
	}
	public class Hole {
	    public static Object hole = "Hole.hole";
	}
	public class List {
	    public static Object Empty = "List.Empty";
	}
	public class Option {
	    public static Object option = "Option.option";
	    public static Object NONE    = (Int32) 0;
	    public static Object SOME    = (Int32) 1;
	}
	public class Real {
	    public static Object Equal   = "Real.EQUAL";
	    public static Object Less    = "Real.LESS";
	    public static Object Greater = "Real.GREATER";
	}
	public class String {
	    public static Object Equal   = "String.EQUAL";
	    public static Object Less    = "String.LESS";
	    public static Object Greater = "String.GREATER";
	}
	public class Thread {
	    public static Object Runnable   = "Thread.RUNNABLE";
	    public static Object Blocked    = "Thread.BLOCKED";
	    public static Object Terminated = "Thread.TERMINATED";
	}
	public class Int {
	    public static Object minInt    = Int32.MinValue;
	    public static Object maxInt    = Int32.MaxValue;
	    public static Object precision = (Int32) 52; // to be determined
	}
	public class Math {
	    public static Object e  = System.Math.E;
	    public static Object pi = System.Math.PI;
	}
	public class IO {
	    public static Object Io = "IO.Io";
	}
	public class OS {
	    public class Process {
		public static Object sucess  = (Int32) 0;
		public static Object failure = (Int32) 1;
	    }
	}
	public class TextIO {
	    public static Object stdIn = System.Console.In;
	}
    }
    class Cell {
	Object Value;
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
    class Future {
	private Object Ref;
	Future() {
	    Ref = null;
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
		    throw new Exception(Prebound.Transient.Future);
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
	    return new TagVal(0, a);
	}
	public static int Length(Object obj) {
	    int n = 0;
	    
	    while (!IsNil(obj)) {
		TagVal tval   = (TagVal) obj;
		Object[] cons = (Object[]) tval.GetValue();
		Object cdr    = CommonOp.Sync(cons[1]);
		
		cons[1] = cdr; // Path Compression
		obj     = cdr;
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
    public abstract class Procedure8  : Procedure {
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
    public class Array {
	public class array : Procedure2 {
	    public static Object StaticApply(Object n, Object x) {
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
	public class fromList : Procedure {
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
	public class length : Procedure {
	    public static Object StaticApply(Object x) {
		return ((Object[]) CommonOp.Sync(x)).Length;
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
    public class Char {
	public class less : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class greater : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) >
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class lessEq : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return CommonOp.BtI((((System.Char) CommonOp.Sync(a)) <=
				     ((System.Char) CommonOp.Sync(b))));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class greaterEq : Procedure2 {
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
		return (Char) CommonOp.Sync(a);
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
	    static char[] arr = "0123456789abcdefABCDEF".ToCharArray();
	    public static Object StaticApply(Object a) {
		System.Char c = (System.Char) CommonOp.Sync(a);
		
		for (int i = 0; i < arr.Length; i++) {
		    if (arr[i] == c) {
			return (Int32) 1;
		    }
		}
		return (Int32) 0;
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
		    v = ((ConVal) v).GetValue();
		}
		if (v is String) {
		    return v;
		}
		else if (v is Guid) {
		    return (System.String) ""; // to be determined
		}
		return (System.String) ""; // to be determined
	    }
	    public override Object Apply(Object v) {
		return StaticApply(v);
	    }
	}
    }
    public class GlobalStamp {
	// exception: cool is stupid (new => New)
	public class New : Procedure0 {
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
		a = CommonOp.Sync(a);

		if (a is Guid) {
		    return ((Guid) a).ToString();
		}
		else {
		    return a;
		}
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
		    return Prebound.String.Less;
		}
		else if (val == 0) {
		    return Prebound.String.Equal;
		}
		else {
		    return Prebound.String.Greater;
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
    class Int {
	public class oppnegate : Procedure {
	    public static Object StaticApply(Object a) {
		try {
		    int t = (int) CommonOp.Sync(a);
		    checked {
			return -t;
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
		    checked {
			return System.Math.Abs((Int32) CommonOp.Sync(a));
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
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class mod : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		try {
		    Int32 ai = (Int32) CommonOp.Sync(a);
		    Int32 bi = (Int32) CommonOp.Sync(b);
		    Int32 c  = (Int32) (ai % bi);

		    if (c < (Int32) 0) {
			return (c + bi);
		    }
		    else {
			return c;
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
		    
		    return (Int32) (ai - (((Int32) (ai / bi)) * bi));
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
		return (double) System.Math.Pow((double) CommonOp.Sync(a), (double) CommonOp.Sync(b));
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
		return (double) ((System.Math.Pow(System.Math.E, x) - System.Math.Pow(System.Math.E, -x)) / 2);
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
    public class Option {
	public class option : Procedure0 {
	    public static Object StaticApply() {
		return Prebound.Option.option;
	    }
	    public override Object Apply() {
		return StaticApply();
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
		    return Prebound.Real.Equal;
		}
		else if (ai < bi) {
		    return Prebound.Real.Less;
		}
		else {
		    return Prebound.Real.Greater;
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
	public class negInf : Procedure0 {
	    public static Object StaticApply() {
		// to be determined
		return 0;
	    }
	    public override Object Apply() {
		return StaticApply();
	    }
	}
	public class posInf : Procedure0 {
	    public static Object StaticApply() {
		// to be determined
		return 0;
	    }
	    public override Object Apply() {
		return StaticApply();
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
		double x = (double) CommonOp.Sync(a);
		if (x >= 0.0) {
		    return (Int32) System.Math.Floor(x);
		}
		else {
		    return (Int32) System.Math.Ceil(x);
		}
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
		int v = System.String.Compare((System.String) CommonOp.Sync(a),
					      (System.String) CommonOp.Sync(b));
		
		if (v < 0) {
		    return Prebound.String.Less;
		}
		else if (v == 0) {
		    return Prebound.String.Equal;
		}
		else {
		    return Prebound.String.Greater;
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class explode : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return (System.String) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class implode : Procedure {
	    public static Object StaticApply(Object a) {
		// to be determined
		return (System.String) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class maxSize : Procedure0 {
	    public static Object StaticApply() {
		return (Int32) 0x7FFFFFFF;
	    }
	    public override Object Apply() {
		return StaticApply();
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
		    char[] arr = ((System.String) CommonOp.Sync(a)).ToCharArray();
		    int i      = (Int32) CommonOp.Sync(b);
		    return (System.Char) arr[i];
		}
		catch (System.Exception) {
		    throw new Exception(Prebound.General.Subscript);
		}
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class subQuote : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		char[] arr = ((System.String) CommonOp.Sync(a)).ToCharArray();
		int i      = (Int32) CommonOp.Sync(b);
		return (System.Char) arr[i];
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
		// to be determined
		return a;
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
    }
    public class Thread {
	public class Terminate : Procedure0 {
	    public static Object StaticApply() {
		// to be determined
		System.Threading.Thread.CurrentThread.Stop();
		return Prebound.unit;
	    }
	    public override Object Apply() {
		return StaticApply();
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
		    return Prebound.Thread.Runnable;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Suspended) {
		    return Prebound.Thread.Blocked;
		}
		else if (t.ThreadState == System.Threading.ThreadState.Stopped) {
		    return Prebound.Thread.Terminated;
		}
		return Prebound.Thread.Runnable;
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
		    return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)];
		}
		public override Object Apply(Object a, Object b) {
		    return StaticApply(a, b);
		}
	    }
	    public class update : Procedure3 {
		public static Object StaticApply(Object a, Object b, Object c) {
		    ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b)] = c;
		    return Prebound.unit;
		}
		public override Object Apply(Object a, Object b, Object c) {
		    return StaticApply(a, b, c);
		}
	    }
	}
	public class Vector {
	    public class sub : Procedure2 {
		public static Object StaticApply(Object a, Object b) {
		    return ((Object[]) CommonOp.Sync(a))[(Int32) CommonOp.Sync(b) + (Int32) 1];
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
	public class maxLen : Procedure0 {
	    public static Object StaticApply() {
		return (Int32) 0x7fffffff;
	    }
	    public override Object Apply() {
		return StaticApply();
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
		// to be determined
		return (Int32) CommonOp.Sync(a);
	    }
	    public override Object Apply(Object a) {
		return StaticApply(a);
	    }
	}
	public class add : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) + (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class sub : Procedure2 {
	    public static Object StaticApply(Object a, Object b) {
		return (Int32) ((Int32) CommonOp.Sync(a) - (Int32) CommonOp.Sync(b));
	    }
	    public override Object Apply(Object a, Object b) {
		return StaticApply(a, b);
	    }
	}
	public class mul : Procedure2 {
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
		    throw new Exception(Prebound.General.Subscript);
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
		    throw new Exception(Prebound.General.Subscript);
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
		return (Int32) ((Int32) CommonOp.Sync(a) >> (Int32) CommonOp.Sync(b));
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
    public class Env {
	public static Object False = (Int32) 0;
	public static Object True  = (Int32) 1;
	public static Object nil   = (Int32) 0; // to be determined
	public static Object cons  = (Int32) 0; // to be determined
	public static Object Ref   = (Int32) 0; // to be determined
	public static Object Match = Prebound.General.Match;
	public static Object Bind  = Prebound.General.Bind;
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
	    public class exit : Procedure2 {
		public static Object StaticApply(Object a, Object b) {
		    Environment.Exit(0);
		    return Prebound.unit;
		}
		public override Object Apply(Object a, Object b) {
		    return StaticApply(a, b);
		}
	    }
	    public class getEnv : Procedure {
		public static Object StaticApply(Object a) {
		    try {
			return (System.String) Environment.GetEnvironmentVariable((System.String)
										  CommonOp.Sync(a));
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
	class StreamWrapper {
	    public System.String name;
	    public Object stream;
	    public StreamWrapper(System.String name, Object stream) {
		this.name   = name;
		this.stream = stream;
	    }
	}
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
		    ar[0]       = e;
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
		    ar[0] = e;
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
	    public static Object StaticApply(Object a) {
		StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
		StreamReader r        = (StreamReader) wrapper.stream;
		try {
		    System.String line = r.ReadLine();

		    if (line == "") {
			return line;
		    }
		    else {
			return System.String.Concat(line, "\n");
		    }
		}
		catch (System.Exception e) {
		    Object[] ar = new Object[3];
		    ar[0]       = e;
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
		    ar[0]       = e;
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
		    ar[0] = e;
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
		    ar[0] = e;
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
}
