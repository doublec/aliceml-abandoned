public class BenchmarksST {
  public static void main(String[] args) 
  {
      int times = 8;
      runtimes("fib", times, 31);
      runtimes("tak", times, 8);
      runtimes("nrev", times, 3000);
//        runtimes("quick", times, 30);
//        runtimes("quickho", times, 30);
//        runtimes("quickarray", times, 30);
//        runtimes("queens", times, 10);
      runtimes("derivvirt", times, 30);
      System.exit(0);
  }

  public static long runtimes(String name, int times, int n)
  {
    long results[] = new long[times];
    long sum = 0;
    for(int i=0; i<times;i++) {
      results[i] = runit(name,n);
      sum += results[i];
    }
    String x = name + " " + n + " ";
    long avrg = sum/times;
    x = x + avrg + " {";
    for(int i=0; i<times;i++) {
	x = x + " " + results[i];
    }
    Dump.dump(x + "]");      
    return avrg;
  } 

  public static long runit(String name, int n) 
  {
      //System.gc();
    long starttime = System.currentTimeMillis();

    int i = 0;
    if (name.compareTo("fib")==0) {
      i=fib(n);
    } else if (name.compareTo("fibf")==0) {
      i=(int)fibf(n);
    } else if (name.compareTo("fibthread")==0) {
      FibThread.runbench(n);
    } else if (name.compareTo("tak")==0) {
      i=tak(3*n,2*n,n);
    } else if (name.compareTo("nrev")==0) {
      ListClass l = ListClass.genlist(n);
      ListClass.nrev(l);//.print();
      //Dump.dump("");            
    } else if (name.compareTo("quick")==0) {
      ListClass.goquick(n);//.print();
    } else if (name.compareTo("quickho")==0) {
      ListClass.goquickho(n);//.print();
    } else if (name.compareTo("quickarray")==0) {
      ListClass.goquickarray(n);//.print();
    } else if (name.compareTo("queens")==0) {
      // Dump.dump(ListClass.queens(n).length());
      ListClass.queens(n);
    } else if (name.compareTo("fastnrev")==0) {
      FastList l = FastList.make(n);
      l.reverse();//.print();
      //Dump.dump("");            
    } else if (name.compareTo("threadcrea")==0) {
      MyThread.makeThreads(n);
      //Dump.dump("");            
    } else if (name.compareTo("threadcomm")==0) {
      CommThread.commThreads(n);
    } else if (name.compareTo("pnpoly")==0) {
      Pnpoly.main();
      //Dump.dump("");            
    } else if (name.compareTo("mandelloop")==0) {
      Mandel.main();
    } else if (name.compareTo("mandel")==0) {
      Mandel.go();
      //Dump.dump("");            
    } else if (name.compareTo("derivvirt")==0) {
      VirtTerm.goderivvirt(n);
    } else if (name.compareTo("intarithfew")==0) {
      IntArith.few(n,23,new Noop());
    } else if (name.compareTo("intarithmany")==0) {
      IntArith.many(n,23,new Noop());
    } else if (name.compareTo("floatarithfew")==0) {
      FloatArith.few(n,23.0,new Noop());
    } else if (name.compareTo("floatarithmany")==0) {
      FloatArith.many(n,23.0,new Noop());

    } else if (name.compareTo("apply1hofew")==0) {
      Apply.few1ho(n,23,new Noop());
    } else if (name.compareTo("apply1homany")==0) {
      Apply.many1ho(n,23,new Noop());
    } else if (name.compareTo("apply5hofew")==0) {
      Apply.few1ho(n,23,new Noop());
    } else if (name.compareTo("apply5homany")==0) {
      Apply.many5ho(n,23,new Noop());
    } else if (name.compareTo("apply10hofew")==0) {
      Apply.few1ho(n,23,new Noop());
    } else if (name.compareTo("apply10homany")==0) {
      Apply.many10ho(n,23,new Noop());

    } else if (name.compareTo("apply1few")==0) {
      Apply.few1(n,23,new Noop());
    } else if (name.compareTo("apply1many")==0) {
      Apply.many1(n,23,new Noop());
    } else if (name.compareTo("apply5few")==0) {
      Apply.few1(n,23,new Noop());
    } else if (name.compareTo("apply5many")==0) {
      Apply.many5(n,23,new Noop());
    } else if (name.compareTo("apply10few")==0) {
      Apply.few1(n,23,new Noop());
    } else if (name.compareTo("apply10many")==0) {
      Apply.many10(n,23,new Noop());

    } else if (name.compareTo("listsfew")==0) {
      Lists.few(n,23,new Noop());
    } else if (name.compareTo("listsmany")==0) {
      Lists.many(n,23,new Noop());

    } else {
      Dump.dump("usage: java Benchmarks <iterations> <name> <n>\n");      
    }
    return System.currentTimeMillis() - starttime;

  }

  public static final int fib(int n) {
    if (2<n) return fib(n-2)+fib(n-1);
    else return 1;
  }
  public static final double fibf(double n) {
    if (n>2.0) return fibf(n-2.0)+fibf(n-1.0);
    return 1.0;
  }
  public static final int tak(int x, int y, int z) {
    if (y<x)
      return tak(tak(x-1,y,z),tak(y-1,z,x),tak(z-1,x,y));
    else
      return z;
  }
}


class Noop {
    public void noop(int x1) {}
    public void noopl(FastList x1) {}
    public void noopf(double x1) {}
    public int fun1(int x) { return 0; }
    public int fun5(int x1,int x2,int x3,int x4,int x5) { return 0; }
    public int fun10(int x1,int x2,int x3,int x4,int x5,int x6,int x7,int x8,int x9,int x10) { return 0; }
    public static int sfun1(int x) { return 0; }
    public static int sfun5(int x1,int x2,int x3,int x4,int x5) { return 0; }
    public static int sfun10(int x1,int x2,int x3,int x4,int x5,int x6,int x7,int x8,int x9,int x10) { return 0; }
}


class IntArith {
    public static void few(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { no.noop(x); } }
    public static void many(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { no.noop(x+x+x); } }
}


class FloatArith {
    public static void few(int n, double x, Noop no)
    { for (int i=0; i<n; i++) { no.noopf(x); } }
    public static void many(int n, double x, Noop no)
    { for (int i=0; i<n; i++) { no.noopf(x+x+x); } }
}

class Lists {
    public static void few(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { no.noopl(null); } }
    public static void many(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopl(new FastList(x,null));
    }}
}


class Apply {
    public static void few1ho(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { no.noopf(x); } }

    public static void many1ho(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.fun1(x)); 
    } }

    public static void many5ho(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.fun5(x,x,x,x,x));
    } }

    public static void many10ho(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.fun10(x,x,x,x,x,x,x,x,x,x));
		 
    } }

    public static void few1(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { no.noopf(x); } }
    public static void many1(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.sfun1(x)); 
    } }
    public static void many5(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.sfun5(x,x,x,x,x));
    } }
    public static void many10(int n, int x, Noop no)
    { for (int i=0; i<n; i++) { 
	no.noopf(no.sfun10(x,x,x,x,x,x,x,x,x,x));
    } }
}


class Cmp {
  public boolean cmp(int a, int b) { return a<b; }
}

final class ListClass {
  int head;
  ListClass tail;

  ListClass(int hd, ListClass tl) { head = hd; tail = tl; }
  static ListClass app(ListClass l1, ListClass l2)
  {
    if (l1==null) return l2;
    return  new ListClass(l1.head,app(l1.tail,l2));
  }

  public static ListClass genlist(int n) {
    ListClass ret = null;
    for(int i=1; i<=n; i++) {
      ret = new ListClass(i,ret);
    }
    return ret;
  }


  static int random(int  n) {
    return (n*25 + 1345) % 10000 + (n*713 + 1345) % 100000;
  }

  static  ListClass randlist(int n)
  {
    ListClass l = null;
    int aux = 0;
    while (n!=0) {
      aux = random(aux);
      n--;
      l = new ListClass(aux,l);
    }
    return l;		  
  }

  int length()
  {
    ListClass aux = this;
    int ret = 0;
    while(aux!=null) {
      aux = aux.tail;
      ret++;
    }
    return ret;
  }

  static ListClass nrev(ListClass l) 
  {
    if (l == null) return null;
    return app(nrev(l.tail),new ListClass(l.head,null));
  }


  /**************************************************/

  static int [] randArray(int n)
  {
    int ret[] = new int[n];
    int aux = 0;
    for(int i=0; i<n; i++) {
      aux = random(aux);
      ret[i] = aux;
    }
    return ret;
  }
  
  static void printArray(int ar[], int n) {
    for(int i=0; i<n; i++) {
      Dump.dump(ar[i]);
    }
  }

  static int partitionarray(int ar[], int low, int high)
  {
    int pivot  = ar[low];
    int pindex = low+1;

    for(int i = pindex; i <= high; i++) {
      int old = ar[i];
      if (pivot > old) {
	ar[i] = ar[pindex];
	ar[pindex] = old;
	pindex++;
      }
    }

    pindex--;
    ar[low]    = ar[pindex];
    ar[pindex] = pivot;
    return pindex;
  }
	
  static void goquickarray(int n)
  {
    while(n-->0) {
      int l[] = randArray(5000);
      quickArray(l);
    }
  }

  static void quickarray1(int ar[], int low, int high)
  {
    while (low < high) {
      int  mid = partitionarray(ar,low,high);
      quickarray1(ar,low,mid-1);
      low = mid+1;
    }
  }

  static void quickArray(int ar[]) {
    quickarray1(ar,0,ar.length-1);
  }


  /**************************************************/
  static void goquick(int n)
  {
    ListClass l = randlist(5000);

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


  static void goquickho(int n)
  {
    ListClass l = randlist(5000);

    while(n-->0) {
      quickho(l,new Cmp());
    }
  }

  static ListClass quickho(ListClass l, Cmp o)
  {
    return quickauxho(l,null,o);
  }

  static ListClass quickauxho(ListClass l, ListClass cont, Cmp o)
  {
    while (l!=null) {
      int a = l.head;
      l = l.tail;

      ListClass less    = null;
      ListClass greater = null;
      while(l!=null) {
	if (o.cmp(l.head,a)) {
	  less = new ListClass(l.head,less);
	} else {
	  greater = new ListClass(l.head,greater);
	}
	l = l.tail;
      }
      cont = new ListClass(a,quickauxho(greater,cont,o));
      l = less;
    }
    return cont;
  }

  /**************************************************/

  static final boolean no_attack1(ListClass l, int c, int y, int i)
  {
    while (l!=null) {
      int x = l.head;
      if (x==y) return false;
      int x1 = x-y;
      int x2 = c-i;
      if (x1==x2) return false;
      if (x1==-x2) return false;
      i++;
      l = l.tail;
    }
    return true;
  }

  
  static ListOfList lft(ListOfList ss,int c,ListClass xs,int y,int n)
  {
    while(y<=n) {
      if (no_attack1(xs,c,y,1)) {
	ss = new ListOfList(app(xs,new ListClass(y,null)),ss);
      }
      y++;
    }
    return ss;
  }

  static ListOfList doFoldL(ListOfList l, ListOfList z, int t, int c) 
  {
    
    while (l!=null) {
      z = lft(z,c,l.head,1,t);
      l = l.tail;
    }
    return z;
  }


  static ListOfList loopForThread(int c, int t, ListOfList i)
  {
    while (c<=t) {
      i = doFoldL(i,null,t,c);
      c++;
    }
    return i;
  }

  static ListOfList queens(int n)
  {
    return loopForThread(1,n,new ListOfList(null,null));
  }

  void print() { 
    Dump.dump("[");
    ListClass aux = this;
    while(aux!=null) {
      Dump.dump(aux.head+" ");
      aux = aux.tail;
    }
    Dump.dump("]");
  }
}


class ListOfList {
  ListClass head;
  ListOfList tail;

  ListOfList(ListClass hd, ListOfList tl) { head = hd; tail = tl; }
  static ListOfList app(ListOfList l1, ListOfList l2)
  {
    if (l1==null) return l2;
    return  new ListOfList(l1.head,app(l1.tail,l2));
  }

  int length()
  {
    ListOfList aux = this;
    int ret = 0;
    while(aux!=null) {
      aux = aux.tail;
      ret++;
    }
    return ret;
  }

  void print() { 
    Dump.dump("[");
    ListOfList aux = this;
    while(aux!=null) {
      head.print();
      Dump.dump(" ");
      aux = aux.tail;
    }
    Dump.dump("]");
  }
}

final class FastList {
  int head;
  FastList tail;

  FastList(int hd, FastList tl) { head = hd; tail = tl; }
  void append(FastList l)
  {
    FastList aux = this;
    if (aux==null)
      return;
    while(aux.tail != null) {
      aux = aux.tail;
    }
    aux.tail = l;
  }
  FastList reverse() 
  {
    if (tail==null)
      return this;
    FastList aux = tail.reverse();
    tail = null;
    aux.append(this);
    return aux;
  }
  public static FastList make(int n) {
    FastList ret = null;
    while(n>0) {
      ret = new FastList(n,ret);
      n--;
    }
    return ret;
  }
  void print() 
  {
    FastList aux = this;
    while(aux!=null) {
      Dump.dump(aux.head+" ");
      aux = aux.tail;
    }
  }
}

final class MyThread extends Thread {
  public void run() { 
    // Dump.dump("son\n");
  }

  public static void makeThreads(int n) {
    while(n-->0) {
      MyThread t = new MyThread();
      t.start();
      try { t.join(); } catch (InterruptedException e) {}
    }
  }
}

final class FibThread extends Thread {
    int n;
    FibThread(int i) { n=i; }

    public void run() { 
	n = fibthread(n);
    }

    int fibthread(int i) 
    {
	if (i<=2) {
	    n=1;
	} else {
	    FibThread aux1 = new FibThread(i-2);
	    FibThread aux2 = new FibThread(i-1);
	    aux1.start();
	    aux2.start();
	    try { aux1.join(); aux2.join(); } catch (InterruptedException e) {};
	    n = aux1.n+aux2.n;
	}
	return n;
    }

    public static void runbench(int n) {
	FibThread aux = new FibThread(n);
	int res = aux.fibthread(n);
	Dump.dump("fibthread(" + n + ") = " + res + "\n");
    }
}


final class Sync extends Object { }

final class CommThread extends Thread {
    int sz;
    boolean numgen;
    static Sync s1,s2;
    static int val = 0;

    CommThread(boolean b, int size) { numgen = b; sz = size; }

    public void generate()
    {
	while(val>0) {
	    try {synchronized (s2) { s2.wait();} } catch (InterruptedException e) {}
	    //Dump.dump("numgen "+val+"\n");
	    val--;
	    synchronized (s1) { s1.notify(); }
	}
    }

    public void reader()
    {
	while(sz-->0) {
	    //Dump.dump("reader "+val+"\n");
	    synchronized (s2) { s2.notify(); }
	    try {synchronized (s1) { s1.wait();} } catch (InterruptedException e) {}
	}
    }

    public void run () {
	if (numgen) {
	    generate();
	} else {
	    reader();
	}
    }

    public static void commThreads(int n) 
    {
	val = n;
	s1 = new Sync();
	s2 = new Sync();
	CommThread t1 = new CommThread(true,n);
	CommThread t2 = new CommThread(false,n);
	t1.start();
	t2.start();
	try { t1.join(); } catch (InterruptedException e) {}
	try { t2.join(); } catch (InterruptedException e) {}
    }
}


class Mandel {
  static final double XBase = -2.0;
  static final double YBase = 1.25;
  static final double Side  = 2.5;
  
  static final int SZ = 800;
  static final int MAX_COUNT = 1024;
  
  static final double delta = (Side / (double)SZ);

  static int doit ()
  {
    int i, j, count;
    int k,sum_iterations;
    
    sum_iterations = 0;
    for (i = 0;  i < SZ;  i++) {
      double c_im = YBase - (delta * (double)i);
      for (j = 0;  j < SZ;  j++) {
	double c_re = XBase * (delta + (double)j);
	double z_re = c_re;
	double z_im = c_im;
	k = MAX_COUNT;
	for (count = 0;  count < MAX_COUNT;  count++) {
	  double z_re_sq = z_re * z_re;
	  double z_im_sq = z_im * z_im;
	  if ((z_re_sq + z_im_sq) > 4.0) {
	    k = count; break;
	  }
	  else {
	    double z_re_im = (z_re * z_im);
	    z_re = (z_re_sq - z_im_sq) + c_re;
	    z_im = z_re_im + z_re_im + c_im;
	  }
	}
	sum_iterations += k; 
      }
    }
    
    return sum_iterations;
    
  }
  

  static int loop3(int count, double z_re, double z_im, double c_re, double c_im) 
  {
    if (count < MAX_COUNT) {
      double z_re_sq = z_re * z_re;
      double z_im_sq = z_im * z_im;
      if ((z_re_sq + z_im_sq) > 4.0)
	return count;
      double z_re_im = (z_re * z_im);
      return loop3 (count+1,
		    (z_re_sq - z_im_sq) + c_re,
		    z_re_im + z_re_im + c_im,
		    c_re, c_im);
    } else {
      return count;
    }
  }

  static int loop2(int j, double c_im, int iter)
  {
    if (j >= SZ) return iter;
    double c_re = XBase * (delta + (double)j);
    int count = loop3(0,c_re,c_im,c_re,c_im);
    return loop2(j+1,c_im,iter+count);
  }
	
  static int loop1(int i, int iter)
  {
    if (i >= SZ) return iter;
    double c_im = YBase - (delta * (double)i);
    return loop1(i+1,loop2(0,c_im,iter));
  }

  static int go() { return loop1(0,0); }

  public static void main() { 
    int i = doit();
    // Dump.dump(i+" iterations\n");
  }

}

class Pnpoly {
  public static boolean pnpoly(int npol, double[] xp, double[] yp, double x, double y)
    {
      int j,i;
      boolean c = false;
      for (i = 0, j = npol-1; i < npol; j = i++) {
	if ((((yp[i]<=y) && (y<yp[j])) ||
	     ((yp[j]<=y) && (y<yp[i]))) &&
	    (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
	  c = !c;
      }
      return c;
    }

  public static void main() {
    int npol=20, count=0;
    double[] xp = {0.0,1.0,1.0,0.0,0.0,1.0,-.5,-1.0,-1.0,-2.0,-2.5,-2.0,-1.5,-.5,1.0,1.0,0.0,-.5,-1.0,-.5};
    double[] yp = {0.0,0.0,1.0,1.0,2.0,3.0,2.0,3.0,0.0,-.5,-1.0,-1.5,-2.0,-2.0,-1.5,-1.0,-.5,-1.0,-1.0,-.5};
    for(int i=0;i<100000;i++) {
      if (pnpoly(npol,xp,yp,0.5,0.5)) count++;
      if (pnpoly(npol,xp,yp,0.5,1.5)) count++;
      if (pnpoly(npol,xp,yp,-.5,1.5)) count++;
      if (pnpoly(npol,xp,yp,0.75,2.25)) count++;
      if (pnpoly(npol,xp,yp,0,2.01)) count++;
      if (pnpoly(npol,xp,yp,-.5,2.5)) count++;
      if (pnpoly(npol,xp,yp,-1.0,-.5)) count++;
      if (pnpoly(npol,xp,yp,-1.5,.5)) count++;
      if (pnpoly(npol,xp,yp,-2.25,-1.0)) count++;
      if (pnpoly(npol,xp,yp,0.5,-.25)) count++;
      if (pnpoly(npol,xp,yp,0.5,-1.25)) count++;
      if (pnpoly(npol,xp,yp,-.5,-2.5)) count++;
    }
    Dump.dump("count " + count+"\n");
  }
}


/********************************************************************************/

abstract class VirtTerm {
  abstract VirtTerm deriv(String x);

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


class Const extends VirtTerm {
  int val;
  Const(int i) { val = i; }
  VirtTerm deriv(String x) { return new Const(0); }
}

class Var extends VirtTerm {
  String val;
  Var(String f) { val = f; }
  VirtTerm deriv(String x) { 
    return (x.compareTo(val)==0) ? new Const(1) : new Const(0);
  }
}

class Plus extends VirtTerm {
  VirtTerm first, second;
  Plus(VirtTerm f, VirtTerm s) { first=f; second=s; }
  VirtTerm deriv(String x) { return new Plus(first.deriv(x),second.deriv(x)); }
}

class Minus extends VirtTerm {
  VirtTerm first, second;
  Minus(VirtTerm f, VirtTerm s) { first=f; second=s; }
  VirtTerm deriv(String x) { return new Minus(first.deriv(x),second.deriv(x)); }
}

class Times extends VirtTerm {
  VirtTerm first, second;
  Times(VirtTerm f, VirtTerm s) { first=f; second=s; }
  VirtTerm deriv(String x) { 
    return new Plus(new Times(first.deriv(x),second),
		    new Times(first,second.deriv(x)));
  }
}

class Exp extends VirtTerm {
  VirtTerm first;
  int second;
  Exp(VirtTerm f, int i) { first=f; second=i; }
  VirtTerm deriv(String x) { 
    int n = (int)second;
    return new Times(new Times(first.deriv(x),new Const(n)),
		     new Exp(first,n-1));
  }
}

class Div extends VirtTerm {
  VirtTerm first, second;
  Div(VirtTerm f, VirtTerm s) { first=f; second=s; }
  VirtTerm deriv(String x) { 
    return new Div(new Minus(new Times(first.deriv(x),second),
			     new Times(first,second.deriv(x))),
		   new Exp(second,2));
  }
}

class Uminus extends VirtTerm {
  VirtTerm val;
  Uminus(VirtTerm f) { val = f; }
  VirtTerm deriv(String x) { return new Minus(val.deriv(x),new Const(0)); }
}

class Log extends VirtTerm {
  VirtTerm val;
  Log(VirtTerm f) { val = f; }
  VirtTerm deriv(String x) { return new Div(val.deriv(x),val); }
}

/********************************************************************************/

/*

compile: use "make Benchmarks.class"

run: use script "dojava"
example:
  dojava java fib 5 31

computes 5 times fib(31)


*/
