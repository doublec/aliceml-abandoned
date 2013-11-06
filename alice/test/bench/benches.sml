(* Tools *)

fun genlistaux(0,m,acc) = acc
  | genlistaux(n,m,acc) = genlistaux(n-1,m+1,m::acc)

fun genlist(n) = genlistaux(n,1,[]);

fun myrandom(n) = (n*25 + 1345) mod 10000 + (n*713 + 1345) mod 100000;

fun randlistaux(0,_,l) = l
  | randlistaux(n,old,l) =
    let val aux = myrandom(old) in
	randlistaux(n-1,aux,aux::l)
    end;

fun randlist(n) = randlistaux(n,0,nil);


fun app(nil,ys)   = ys
  | app(x::xr,ys) = x::app(xr,ys);

fun dotimes(0,p) = 0
  | dotimes(n,p) = (p(); dotimes(n-1,p));

(* WW: *)
(* fun int32toInt(n) = IntInf.toInt(n); *)

fun int32toInt(n) = Int32.toInt(n);

fun dobench(proc) = 
    let 
	val start = Timer.startCPUTimer()
	val ignored = proc()
	val {usr=u,sys=s,gc=g} = Timer.checkCPUTimer(start)
	val ut = Time.toMilliseconds(u)
	val gt = Time.toMilliseconds(g)
    in int32toInt(ut) (* ut - gt *)
    end;

fun dobenchn (0, _) = []
  | dobenchn (n, proc) =
    dobench(proc)::dobenchn(n-1,proc);

fun sum([],res) = res
  | sum(x::xs,res) = sum(xs,res+x);


fun avrg(l, len) = sum(l,0) div len;

fun dobenchavrg(n,proc) =
    let 
	val aux = dobenchn(n,proc)
    in
	(avrg(aux,n),aux)
    end


(* dobenchavrg(10,fn()=>app(genlist(1000),[])); *)

(* Benches *)

(*--** use <? *)

fun fib n = if n < 2 then 1 else fib(n-2)+fib(n-1);
(* fun fib (0|1) = 1
  | fib n = fib(n-2)+fib(n-1); *)

fun fibf(n:real) = 
    if Real.<(2.0, n) 
    then Real.+(fibf(Real.-(n, 2.0)), fibf(Real.-(n, 1.0)))
    else 1.0;

(**************************************************************)

fun tak(x,y,z) = 
   if y<x then tak(tak(x-1,y,z),tak(y-1,z,x),tak(z-1,x,y))
   else z;

fun cpstakaux(x,y,z,k) = 
   if y<x then
      cpstakaux(x-1,y,z,
		fn(v1)=>
		cpstakaux(y-1,z,x,
			  fn(v2)=>
			  cpstakaux(z-1,x,y,
				    fn(v3)=>cpstakaux(v1,v2,v3,k))))
   else
      k(z);

fun cpstak(x,y,z) = cpstakaux(x,y,z,fn(a)=>a);

(**************************************************************)

fun tailRec(n) = if n = 0 then 0 else tailRec (n - 1);

(**************************************************************)

(* Make app visible to nrev during runtime compilation *)
val _ = app(nil, nil);

fun nrev(nil)   = nil
  | nrev(a::bs) = app(nrev(bs),[a]);

(**************************************************************)

fun quickaux([],cont) = cont
  | quickaux(a::bs,cont) = partition(bs,a,[],[],cont)

and partition ([],a,left,right,cont): int list =
    quickaux(left,a::quickaux(right,cont))
  | partition(x::xs,a,left,right,cont) =
    if x < a then partition(xs,a,x::left,right,cont)
    else partition(xs,a,left,x::right,cont);

fun quick l = quickaux(l,[]);


fun quickaux([],cont,cmp) = cont
  | quickaux(a::bs,cont,cmp) = partition(bs,a,[],[],cont,cmp)

and partition ([],a,left,right,cont,cmp): int list =
    quickaux(left,a::quickaux(right,cont,cmp),cmp)
  | partition(x::xs,a,left,right,cont,cmp) =
    if cmp(x,a) then partition(xs,a,x::left,right,cont,cmp)
    else partition(xs,a,left,x::right,cont,cmp);

val less = op< : int * int -> bool

fun quickho(l,cmp) = quickaux(l,[],cmp);


fun partitionarray1(ar,pivot,pindex,fro,to) =
    if fro <= to then 
	let
	    val old = Array.sub(ar,fro)
	in
	    if pivot > old then 
		(Array.update(ar,fro,Array.sub(ar,pindex));
		 Array.update(ar,pindex,old);
		 partitionarray1(ar,pivot,pindex+1,fro+1,to))
	    else
		partitionarray1(ar,pivot,pindex,fro+1,to)
	end
    else pindex-1;


fun partitionarray(ar,fro,to) =
    let val pivot = Array.sub(ar,fro)
	val mid = partitionarray1(ar,pivot,fro+1,fro+1,to)
    in	(Array.update(ar,fro,Array.sub(ar,mid));
	 Array.update(ar,mid,pivot);
	 mid)
    end;

fun quickarray1(ar,fro:int,to:int) =
   if fro < to then 
      let val mid = partitionarray(ar,fro,to)
      in (quickarray1(ar,fro,mid-1);
	  quickarray1(ar,mid+1,to))
      end
   else ();

fun quickarray(ar) = quickarray1(ar,0, Array.length(ar)-1);


(**************************************************************)

fun no_attackaux(nil,c,y,i) = true
  | no_attackaux(x::xr,c,y,i) = 
    (x<>y) andalso
    ((abs (x-y))<>(c-i)) andalso
    no_attackaux(xr,c,y,i+1);


fun no_attack(xs,c,y) = no_attackaux(xs,c,y,1);

fun queensLoop2(ss,c,xs,y,n) =
   if y>n then ss
   else if no_attack(xs,c,y) 
        then queensLoop2(app(xs,[y])::ss,c,xs,y+1,n)
        else queensLoop2(ss,c,xs,y+1,n);


fun doFoldL(nil,z,t,c) = z
  | doFoldL(x::xr,z,t,c) = doFoldL(xr,queensLoop2(z,c,x,1,t),t,c);


fun queensLoop1(c,t,i) =
   if c<=t then
	 queensLoop1(c+1,t,doFoldL(i,nil,t,c))
   else i;

fun queens(n) = queensLoop1(1,n,[nil]);

(**************************************************************)

val x_base = ~2.0
val y_base = 1.25
val side = 2.5

val sz = 800
val maxCount = 1024
    
val delta = side / (real sz)

fun mandelloop3 (count, z_re:real, z_im:real, c_re:real, c_im:real) = 
    if (count < maxCount)
	then 
	    let
		val z_re_sq = Real.*(z_re, z_re)
		val z_im_sq = Real.*(z_im, z_im)
	    in
		if Real.>(Real.+(z_re_sq, z_im_sq), 4.0)
		    then count
		else 
		    let
			val z_re_im = Real.*(z_re, z_im)
		    in
			mandelloop3 (count+1,
				     Real.+(Real.-(z_re_sq, z_im_sq), c_re),
				     Real.+(z_re_im, Real.+(z_re_im, c_im)),
				     c_re, c_im)
		    end
		end
	else count;

fun mandelloop2(j,c_im,iter) = 
    if (j >= sz) then iter
    else 
	let val c_re = Real.*(x_base, Real.+(delta, real j))
	    val count = mandelloop3(0,c_re,c_im,c_re,c_im)
	in
	    mandelloop2(j+1,c_im,iter+count)
	end;
	
fun mandelloop(i,iter) = 
    if (i >= sz) then iter
    else 
	let val c_im : real = Real.-(y_base, Real.*(delta, real i))
	in mandelloop(i+1,mandelloop2(0,c_im,iter))
	end;


fun mandeliter () =
let val i = ref 0
    val j = ref 0
    val k = ref 0
    val count = ref 0
    val sum_iterations = ref 0
in
    while (!i < sz) do
	let val c_im:real = Real.-(y_base, Real.*(delta, real (!i))) in
	    j := 0;
	    while (!j < sz) do
		let val c_re = Real.*(x_base, Real.+(delta, real (!j))) in
		    let val z_re = ref c_re
			val z_im = ref c_im 
		    in
			k := maxCount;
			count := 0;
			while (!count < maxCount ) do
			    let val z_re_sq = Real.*(!z_re, !z_re) 
				val z_im_sq = Real.*(!z_im, !z_im)
			    in
				if Real.>(Real.+(z_re_sq, z_im_sq), 4.0)
				then (k := !count; 
				      count := maxCount)
				else 
				    let val z_re_im = Real.*((!z_re), (!z_im))
				    in z_re := Real.+(Real.-(z_re_sq, z_im_sq), c_re);
				       z_im := Real.+(z_re_im, Real.+(z_re_im, c_im))
                                    end;
				count := !count+1
                            end
                    end;
		    j := !j+1;
		    sum_iterations := !sum_iterations + !k
                end;
	    i := !i+1
        end;
    !sum_iterations
end;

(*************************************************************)

datatype expr = Plus  of expr * expr
              | Minus of expr * expr
              | Var of int
              | Const of int
              | Times of expr * expr
              | Div of expr * expr
              | Exp of expr * int
              | Uminus of expr
              | Log of expr;

fun deriv(Var(u),x)     = if u=x then Const(1) else Const(0)
  | deriv(Const(u),x)   = Const(0)
  | deriv(Plus(u,v),x)  = Plus(deriv(u,x),deriv(v,x))
  | deriv(Minus(u,v),x) = Minus(deriv(u,x),deriv(v,x))
  | deriv(Times(u,v),x) = Plus(Times(deriv(u,x),v),Times(u,deriv(v,x)))
  | deriv(Div(u,v),x)   = Div(Minus(Times(deriv(u,x),v),
				    Times(u,deriv(v,x))),
			      Exp(v,2))
  | deriv(Exp(u,n),x)   = Times(Times(deriv(u,x),Const(n)),Exp(u,n-1))
  | deriv(Uminus(u),x)  = Uminus(deriv(u,x))
  | deriv(Log(u),x)     = Div(deriv(u,x),u);


fun nthderiv(0,exp,x) = exp
  | nthderiv(n,exp,x) = 
    nthderiv(n-1,deriv(exp,x),x);

fun goderiv(n) =
   dotimes(n, fn()=> nthderiv(6,Exp(Div(Const(1),Var(1)), 3),1));



(**************************************************************)

fun getit("fib",n)    = (fn()=>fib(n))
  | getit("fibf",n)   = (fn()=>(fibf(real(n));1))
  | getit("tak",n)    = (fn()=>(tak(3*n,2*n,n)))
  | getit("cpstak",n) = (fn()=>(cpstak(3*n,2*n,n)))
  | getit("nrev",n)   = let val l = genlist(n) in (fn()=>(nrev(l);1)) end
  | getit("quick",n)  = let val l=randlist(5000) in 
    fn()=>dotimes(n,fn()=>quick(l)) end
  | getit("quickho",n)  = let val l=randlist(5000) in 
    fn()=>dotimes(n,fn()=>quickho(l,less)) end
  | getit("quickarray",n)  = let val l=randlist(5000) in 
    fn()=>dotimes(n,fn()=>quickarray(Array.fromList(l))) end
  | getit("queens",n) = (fn()=>(queens(n);1))
  | getit("mandel",n) = (fn()=>(mandelloop(0,0);1))
  | getit("mandeliter",n) = (fn()=>(mandeliter()))
  | getit("deriv",n)  = (fn()=>(goderiv(n);1))
  | getit("tailrec",n) = (fn()=>tailRec(n))
  | getit(bench,n)    = (fn()=>(print("unknown\n");1));

fun printBench(name, n, (mean, times)) =
    (TextIO.print name;
     TextIO.print "(";
     TextIO.print (Int.toString n);
     TextIO.print ") = ";
     TextIO.print (Int.toString mean);
     TextIO.print " mean, [";
     List.app (fn n => (TextIO.print ((Int.toString n) ^ " "))) times;
     TextIO.print "]\n";
     TextIO.flushOut TextIO.stdOut)


fun doit(bench,iter,n) =
    let
      val proc = getit(bench,n)
    in
      proc ();
      printBench (bench,n,dobenchavrg(iter, proc))
    end;

fun doitall(iter) =
    ( doit("fib",iter,31),
      doit("fibf",iter,31),
      doit("tak",iter,8),
      doit("cpstak",iter,8),
      doit("nrev",iter,3000),
      doit("quick",iter,30),
      doit("quickho",iter,30),
      doit("quickarray",iter,30),
      doit("queens",iter,10),
      doit("mandel",iter,4711),
      doit("deriv",iter,30)
      );

fun benches(iter) =
    [doit("fib",iter,31),
(*       doit("fibf",iter,31), *)
     doit("tak",iter,8),
(*       doit("cpstak",iter,8), *)
     doit("nrev",iter,3000),
     doit("quick",iter,30),
(*       doit("quickho",iter,30), *)
(*       doit("quickarray",iter,30), *)
(*       doit("queens",iter,10), *)
(*       doit("mandel",iter,4711), *)
     doit("deriv",iter,30)]

(* Turn of noisy GC *)
val _ = SMLofNJ.Internals.GC.messages false;

val _ = benches 10;
val _ = OS.Process.exit OS.Process.success
