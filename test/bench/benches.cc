#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/times.h>


const int heapSize = 100000;

char *heap = NULL;
int heapUsed = heapSize;


inline
char *heapMalloc(int sz)
{
  char *ret = heap+heapUsed;
  heapUsed += sz;
  while (heapUsed>=heapSize) {
    heap = ::new char[heapSize];
    heapUsed = sz;
    ret = heap;
  }
  return ret;
}


class List {
public:

  void *head;
  List *tail;

  static void *operator new(size_t sz)  { return heapMalloc(sz); }

  List() {}
  List(void *hd, List *tl): head(hd), tail(tl) {}

  int length()
  {
    int ret = 0;
    List *aux = this;
    while(aux) {
      ret++;
      aux = aux->tail;
    }
    return ret;
  }

  void print()
  {
    List *aux = this;
    while(aux) {
      printf("%d ",aux->head);
      aux = aux->tail;
    }
    printf("\n");
  }
};

inline List *cons(void *hd, List *tl) { return new List(hd,tl); }
inline List *cons(int hd, List *tl)   { return cons((void *)hd,tl); }
inline void *car(List *l) { return l->head; }
inline List *cdr(List *l) { return l->tail; }

int length(List *l) { return l->length(); }


List *genlist(int n)
{
  int i = n;
  List *ret = 0;
  while(i>0) {
    i--;
    ret = cons(n-i,ret);
  }
  return ret;
}


void check(int n)
{
  if (n>>27) { printf("******* %d\n",n); }
}

int myrandom(int  n) 
{

  //  check(n*25 + 1345);
  //  check(n*713 + 1345);
  return (n*25 + 1345) % 10000 + (n*713 + 1345) % 100000;
}

List *randlist(int n)
{
  List *l = NULL;
  int aux = 0;
  while (n!=0) {
      aux = myrandom(aux);
      n--;
      l = cons(aux,l);
  }
  return l;		  
}



inline
List *app(List *l1, List *l2)
{
  return (l1==NULL) ? l2 : cons(l1->head,app(l1->tail,l2));
}


inline
List *nconc(List *l1, List *l2)
{
  if (l1==NULL) return l2;
  List *aux = l1;
  while(aux->tail != NULL) {
    aux = aux->tail;
  }
  aux->tail = l2;
  return l1;
}


// return current usertime in milliseconds
int osUserTime()
{
  struct tms buffer;

  times(&buffer);
  return (unsigned int)(buffer.tms_utime*1000.0/(double)sysconf(_SC_CLK_TCK));
}

int timeit(int (*f)(int), int n)
{
  int before = osUserTime();
  f(n);
  int after  = osUserTime();
  return after-before;
}


void dotimes(char *bench, int iter, int (*f)(int), int n)
{
  int *results = new int[iter];
  int sum = 0;
  for(int i=0;i<iter;i++) {
    results[i] = timeit(f,n);
    sum += results[i];
  }

  int avrg = sum/iter;
  printf("%s %d %d [",bench,n,avrg);
  for(int i=0;i<iter;i++) {
    printf("%d ",results[i]);
  }
  printf("]\n");
}


void dotimes2(char *bench, int iter, int (*few)(int), int (*many)(int), int n)
{
  int *resultsfew = new int[iter];
  int sumfew = 0;
  for(int i=0;i<iter;i++) {
    resultsfew[i] = timeit(few,n);
    sumfew += resultsfew[i];
  }

  int *resultsmany = new int[iter];
  int summany = 0;
  for(int i=0;i<iter;i++) {
    resultsmany[i] = timeit(many,n);
    summany += resultsmany[i];
  }

  int avrgmany = summany/iter;
  int avrgfew  = sumfew/iter;
  int avrg = avrgmany-avrgfew;
  printf("%s %d %d [",bench,iter,avrg);
  for(int i=0;i<iter;i++) { printf("%d ",resultsfew[i]); }
  printf("] [");
  for(int i=0;i<iter;i++) { printf("%d ",resultsmany[i]); }
  printf("]\n");
}

int gothreadcrea(int n) { return 1; }
int gothreadcomm(int n) { return 1; }

int fib(int n)
{
  if (2<n) {
    return fib(n-2)+fib(n-1);
  } else {
    return 1;
  }
}


double fibf(double n)
{
  if (2.0<n) {
    return fibf(n-2.0)+fibf(n-1.0);
  } else {
    return 1.0;
  }
}

int gofibf(int n) { return (int) fibf(n); }

/************************************************************/

int tak(int x, int y, int z) 
{ 
  if (y<x) 
    return tak(tak(x-1,y,z),tak(y-1,z,x),tak(z-1,x,y));
  else 
    return z;
}

int gotak(int n) { return tak(3*n,2*n,n); }


/************************************************************/


List *quickaux(List *l, List *cont);

List *partition (List *xs, int a, List *left, List* right, List *cont)
{
  while (xs!=0) {
    int x = (int)xs->head;
    xs = xs->tail;

    if (x<a) {
      left = cons(x,left);
    } else {
      right = cons(x,right);
    }
    
  }
  return quickaux(left,cons(a,quickaux(right,cont)));
}



List *quickaux(List *l, List *cont)
{
  if (l==0) return 0;
  return partition(l->tail,(int)l->head,NULL,NULL,cont);
}

List *quick(List *l) { return quickaux(l,NULL); }

static List *lQuick = NULL;

int goquick(int n) 
{ 
  //  List *l = randlist(5000);
  while(n--) {
    quick(lQuick);
  }
  return 1;
}


/******************/


typedef int (cmpfun)(int, int);

List *quickauxho(List *l, List *cont, cmpfun cmp);

List *partitionho(List *xs, int a, List *left, List* right, List *cont, cmpfun cmp)
{
  while (xs!=0) {
    int x = (int)xs->head;
    xs = xs->tail;

    if (cmp(x,a)) {
      left = cons(x,left);
    } else {
      right = cons(x,right);
    }
    
  }
  return quickauxho(left,cons(a,quickauxho(right,cont,cmp)),cmp);
}



List *quickauxho(List *l, List *cont, cmpfun cmp)
{
  if (l==0) return 0;
  return partitionho(l->tail,(int)l->head,NULL,NULL,cont,cmp);
}

List *quickho(List *l, cmpfun cmp) { return quickauxho(l,NULL,cmp); }

int less(int x, int y) { return x<y; }

int goquickho(int n) 
{ 
  List *l = randlist(5000);
  while(n--) {
    quickho(l,less);
  }
  return 1;
}


/*************************/

int *randArray(int n)
{
  int *ret = new int[n];
  int aux = 0;
  for(int i=0; i<n; i++) {
    aux = myrandom(aux);
    ret[n-i] = aux;
  }
  return ret;
}

void printArray(int *ar, int n) {
  for(int i=0; i<n; i++) {
    printf("%d\n",ar[i]);
  }
}

int partitionarray1(int *ar, int pivot, int pindex, int low, int high)
{
  while (low <= high) {
    int old = ar[low];
    if (pivot > old) {
      ar[low] = ar[pindex];
      ar[pindex] = old;
      pindex++;
    }
    low++;
  }
  return pindex-1;
}

int partitionarray(int *ar, int low, int high)
{
  int pivot = ar[low];
  int mid = partitionarray1(ar,pivot,low+1,low+1,high);
  ar[low] = ar[mid];
  ar[mid] = pivot;
  return mid;
}

void quickarray1(int *ar, int low, int high)
{
  while (low < high) {
    int  mid = partitionarray(ar,low,high);
    quickarray1(ar,low,mid-1);
    low = mid+1;
  }
}

void quickArray(int *ar, int n) { quickarray1(ar,0,n-1); }

int goquickarray(int n) 
{ 
  while(n--) {
    quickArray(randArray(5000),5000); 
  }
  return 1; 
}

/************************************************************/

List *nrev(List *l)
{
  if (l==NULL) return NULL;
  return app(nrev(cdr(l)),cons(car(l),NULL));
  //return nconc(nrev(cdr(l)),cons(car(l),NULL));
}

static List *lRev = NULL;

int gonrev(int n) { nrev(lRev); return 1;}


/************************************************************/

int no_attack1(List *l, int c, int y, int i) 
{
  while (l!=NULL) {
    int x = (int) car(l);
    if (x==y || abs(x-y)==(c-i))
      return 0;
    i++;
    l = cdr(l);
  }
  return 1;
}

int no_attack(List *xs, int c, int y) { return no_attack1(xs,c,y,1); }

List *lft(List *ss, int c, List *xs, int y, int n)
{
  while (y<=n){
    if (no_attack(xs,c,y)) {
      ss = cons(app(xs,cons(y,NULL)),ss);
    }
    y++;
  }
  return ss;
}


List *doFoldL(List *l, List *z, int t, int c)
{
  while (l!=NULL) {
    List *x  = (List *)car(l);
    l = cdr(l);
    z = lft(z,c,x,1,t);
  }
  return z;
}


List *loopForThread(int c, int t, List *i)
{
  while (c<=t) {
    i=doFoldL(i,NULL,t,c);
    c++;
  }
  return i;
}

List *queens(int n) { return loopForThread(1,n,cons(NULL,NULL)); }

//int goqueens(int n) { printf("length=%d\n",length(queens(n))); return 1; }
int goqueens(int n) { (void) queens(n); return 1; }

/************************************************************/

#define XBase -2.0
#define YBase 1.25
#define Side 2.5

#define SZ 800
#define MAX_COUNT 1024

int gomandel(int ignored)
{
  double delta = (Side / (double)SZ);
  int    i, j, count;
  int    k,sum_iterations;
  
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
	} else {
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

/************************************************************/

enum Ttag {PLUS, MINUS, VAR, CONST, TIMES, DIV, EXP, UMINUS, LOG};

class Term {
public:
  Ttag tag;
  Term *first, *second;
  static void *operator new(size_t sz)  { return heapMalloc(sz); }
  Term(Ttag t,Term *f, Term *s) { tag=t; first=f; second=s; }
};

Term *deriv(Term *t, char *x)
{
  switch(t->tag) {
  case VAR: return (x==(char*)t->first) 
	      ? new Term(CONST,(Term*)1,0)
	      : new Term(CONST,(Term*)0,0);
  case CONST: return new Term(CONST,(Term*)0,0);
  case PLUS:  return new Term(PLUS,deriv(t->first,x),deriv(t->second,x));
  case MINUS: return new Term(MINUS,deriv(t->first,x),deriv(t->second,x));
  case TIMES: return new Term(PLUS,
			      new Term(TIMES,
				       deriv(t->first,x),
				       t->second),
			      new Term(TIMES,
				       t->first,
				       deriv(t->second,x)));
  case DIV:  return new Term(DIV,
			     new Term(MINUS,
				      new Term(TIMES,
					       deriv(t->first,x),
					       t->second),
				      new Term(TIMES,
					       t->first,
					       deriv(t->second,x))),
			     new Term(EXP,t->second,(Term*)2));
  case EXP: { 
    int n = (int)t->second;
    return new Term(TIMES,
		    new Term(TIMES,
			     deriv(t->first,x), 
			     new Term(CONST,(Term*)n,0)),
		    new Term(EXP,t->first,(Term*)n-1));
  }
  case UMINUS: return new Term(UMINUS,deriv(t->first,x),0);
  case LOG:    return new Term(DIV,deriv(t->first,x),t->first);
  default:     printf("unknown tag: %d\n",t->tag);
  }
}


Term *nthderiv(int n, Term *exp, char *x)
{
  for (int i=0; i<n; i++) {
    exp = deriv(exp,x);
  }
}

int goderiv(int n)
{
  for (int i=0; i<n; i++) {
    nthderiv(6, new Term(EXP,
			 new Term(DIV,
				  new Term(CONST,(Term*)1,0),
				  new Term(VAR,(Term*)"x",0)),
			 (Term*)3),"x");
  }
}

/****************************/

class VirtTerm {
public:
  VirtTerm *first, *second;
  VirtTerm(VirtTerm *f, VirtTerm *s) : first(f), second(s) {}
  virtual VirtTerm *deriv(char *x) = 0;
};


class Const: public VirtTerm {
public:
  Const(int i) : VirtTerm((VirtTerm*)i,0) {}
  VirtTerm *deriv(char *x) { return new Const(0); }
};

class Var: public VirtTerm {
public:
  Var(char *f) : VirtTerm((VirtTerm*)f,0) {}
  VirtTerm *deriv(char *x) { 
    return (x==(char*)first) ? new Const(1) : new Const(0);
  }
};

class Plus: public VirtTerm {
public:
  Plus(VirtTerm *f, VirtTerm *s) : VirtTerm(f,s) {}
  VirtTerm *deriv(char *x) { return new Plus(first->deriv(x),second->deriv(x)); }

};

class Minus: public VirtTerm {
public:
  Minus(VirtTerm *f, VirtTerm *s) : VirtTerm(f,s) {}
  VirtTerm *deriv(char *x) { return new Minus(first->deriv(x),second->deriv(x)); }

};

class Times: public VirtTerm {
public:
  Times(VirtTerm *f, VirtTerm *s) : VirtTerm(f,s) {}
  VirtTerm *deriv(char *x) { 
    return new Plus(new Times(first->deriv(x),second),
		    new Times(first,second->deriv(x)));
  }
};

class Exp: public VirtTerm {
public:
  Exp(VirtTerm *f, int i) : VirtTerm(f,(VirtTerm *)i) {}
  VirtTerm *deriv(char *x) { 
    int n = (int)second;
    return new Times(new Times(first->deriv(x),new Const(n)),
		     new Exp(first,n-1));
  }
};

class Div: public VirtTerm {
public:
  Div(VirtTerm *f, VirtTerm *s) : VirtTerm(f,s) {}
  VirtTerm *deriv(char *x) { 
    return new Div(new Minus(new Times(first->deriv(x),second),
			     new Times(first,second->deriv(x))),
		   new Exp(second,2));
  }
};

class Uminus: public VirtTerm {
public:
  Uminus(VirtTerm *f) : VirtTerm(f,0) {}
  VirtTerm *deriv(char *x) { return new Minus(first->deriv(x),0); }
};

class Log: public VirtTerm {
public:
  Log(VirtTerm *f) : VirtTerm(f,0) {}
  VirtTerm *deriv(char *x) { return new Div(first->deriv(x),first); }
};


VirtTerm *nthderiv(int n, VirtTerm *exp, char *x)
{
  for (int i=0; i<n; i++) {
    exp = exp->deriv(x);
  }
  return exp;
}

int goderivvirt(int n)
{
  for (int i=0; i<n; i++) {
    nthderiv(6, new Exp(new Div(new Const(1),new Var("x")),3),
	     "x");
  }
}


/************************************************************/

main(int argc, char **argv)
{
  int times = 10;

  dotimes("fib",times,fib,31);
//    dotimes("fibf",times,gofibf,31);
  dotimes("tak",times,gotak,8);
//    lRev = genlist(3000);
//    dotimes("nrev",times,gonrev,3000);
//    lQuick = randlist(5000);
//    dotimes("quick",times,goquick,30);
//    dotimes("quickho",times,goquick,30);
//    dotimes("quickarray",times,goquick,30);
//    dotimes("queens",times,goqueens,10);
//    dotimes("mandel",times,gomandel,4711);
  //  dotimes("deriv",times,goderiv,30);
  dotimes("derivvirt",times,goderivvirt,30);
}
