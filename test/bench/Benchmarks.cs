using System;
using System.Diagnostics;

class ListClass {
    int head;
    ListClass tail;
    
    ListClass(int hd, ListClass tl) { head = hd; tail = tl; }
    static ListClass app(ListClass l1, ListClass l2) {
	if (l1==null)
	    return l2;
	return new ListClass(l1.head,app(l1.tail,l2));
    }
    public static ListClass genlist(int n) {
	ListClass ret = null;
	for(int i=1; i<=n; i++) {
	    ret = new ListClass(i,ret);
	}
	return ret;
    }
    public static ListClass nrev(ListClass l) {
	if (l == null) return null;
	return app(nrev(l.tail),new ListClass(l.head,null));
    }

    static int random(int  n) {
	return (n*25 + 1345) % 10000 + (n*713 + 1345) % 100000;
    }
    
    public static  ListClass randlist(int n) {
	ListClass l = null;
	int aux = 0;
	while (n!=0) {
	    aux = random(aux);
	    n--;
	    l = new ListClass(aux,l);
	}
	return l;		  
    }

    public static void goquick(ListClass l, int n) {
	while(n-->0) {
	    quick(l);
	}
    }
    static ListClass quick(ListClass l)
    {
	return quickaux(l,null);
    }
    static ListClass quickaux(ListClass l, ListClass cont)
    {
	while (l!=null) {
	    int a = l.head;
	    l = l.tail;
	    
	    ListClass less    = null;
	    ListClass greater = null;
	    while(l!=null) {
		if (l.head<a) {
		    less = new ListClass(l.head,less);
		} else {
		    greater = new ListClass(l.head,greater);
		}
		l = l.tail;
	    }
	    cont = new ListClass(a,quickaux(greater,cont));
	    l = less;
	}
	return cont;
    }
}

abstract class VirtTerm {
    public abstract VirtTerm deriv(String x);
    
    public VirtTerm nthderiv(int n, String x)
    {
	VirtTerm exp = this;
	for (int i=0; i<n; i++) {
	    exp = exp.deriv(x);
	}
	return exp;
    }
    
    public static int goderivvirt(int n)
    {
	for (int i=0; i<n; i++) {
	    new Exp(new Div(new Const(1),new Var("x")),3).nthderiv(6,"x");
	}
	return 1;
    }
}

class Const : VirtTerm {
  int val;
  public Const(int i) { val = i; }
  public override VirtTerm deriv(String x) { return new Const(0); }
}

class Var : VirtTerm {
  String val;
  public Var(String f) { val = f; }
  public override VirtTerm deriv(String x) { 
    return (x.Equals(val)) ? new Const(1) : new Const(0);
  }
}

class Plus : VirtTerm {
  VirtTerm first, second;
  public Plus(VirtTerm f, VirtTerm s) { first=f; second=s; }
  public override VirtTerm deriv(String x) {
      return new Plus(first.deriv(x),second.deriv(x));
  }
}

class Minus : VirtTerm {
  VirtTerm first, second;
  public Minus(VirtTerm f, VirtTerm s) { first=f; second=s; }
  public override VirtTerm deriv(String x) {
      return new Minus(first.deriv(x),second.deriv(x));
  }
}

class Times : VirtTerm {
  VirtTerm first, second;
  public Times(VirtTerm f, VirtTerm s) { first=f; second=s; }
  public override VirtTerm deriv(String x) { 
      return new Plus(new Times(first.deriv(x),second),
		      new Times(first,second.deriv(x)));
  }
}

class Exp : VirtTerm {
  VirtTerm first;
  int second;
    public Exp(VirtTerm f, int i) { first=f; second=i; }
    public override VirtTerm deriv(String x) { 
	int n = (int)second;
	return new Times(new Times(first.deriv(x),new Const(n)),
			 new Exp(first,n-1));
    }
}

class Div : VirtTerm {
  VirtTerm first, second;
  public Div(VirtTerm f, VirtTerm s) { first=f; second=s; }
  public override VirtTerm deriv(String x) { 
    return new Div(new Minus(new Times(first.deriv(x),second),
			     new Times(first,second.deriv(x))),
		   new Exp(second,2));
  }
}

class Uminus : VirtTerm {
  VirtTerm val;
  public Uminus(VirtTerm f) { val = f; }
  public override VirtTerm deriv(String x) {
      return new Minus(val.deriv(x),new Const(0));
  }
}

class Log : VirtTerm {
  VirtTerm val;
  public Log(VirtTerm f) { val = f; }
  public override VirtTerm deriv(String x) {
      return new Div(val.deriv(x),val);
  }
}

class Benchmarks {
    public static void Main() {
	int times = 10;
  	runtimes("fib", times, 31);
  	runtimes("tak", times, 8);
  	runtimes("nrev", times, 3000);
  	runtimes("quick", times, 30);
  	runtimes("derivvirt", times, 30);
    }
    public static long runtimes(String name, int times, int n) {
	long[] results = new long[times];
	long sum = 0;
	for(int i=0; i<times;i++) {
	    results[i] = runit(name,n);
	    sum += results[i];
	}
	Console.Write(name+" "+n+" ");
	long avrg = sum/times;
	Console.Write(avrg);
	Console.Write( " [");
	for(int i=0; i<times;i++) {
	    Console.Write(" ");
	    Console.Write(results[i]);
	}
	Console.WriteLine("]");      
	return avrg;
    } 
    public static DateTime GetTime() {
	return DateTime.Now;
    }
    public static long runit(String name, int n) {
	ListClass lRev = null;
	ListClass lQuick = null;
	if (name.Equals("nrev")) {
	    lRev = ListClass.genlist(n);
	}
	else if (name.Equals("quick")) {
	    lQuick = ListClass.randlist(5000);
	}
	GC.Collect(GC.MaxGeneration);
	DateTime startTime = GetTime();
	int i = 0;
	if (name.Equals("fib")) {
	    i = fib(n);
	}
	else if (name.Equals("tak")) {
	    i=tak(3*n,2*n,n);
	}
	else if (name.Equals("nrev")) {
	    ListClass.nrev(lRev);
	}
	else if (name.Equals("quick")) {
	    ListClass.goquick(lQuick, n);
	}
	else if (name.Equals("derivvirt")) {
	    VirtTerm.goderivvirt(n);
	}
	DateTime endTime = GetTime();
	TimeSpan diffTime = endTime.Subtract(startTime);
	return (long) diffTime.TotalMilliseconds;
    }
    public static int fib(int n) {
	if (2<n)
	    return fib(n-2)+fib(n-1);
	else
	    return 1;
    }
    public static int tak(int x, int y, int z) {
	if (y<x)
	    return tak(tak(x-1,y,z),tak(y-1,z,x),tak(z-1,x,y));
	else
	    return z;
    }
}
