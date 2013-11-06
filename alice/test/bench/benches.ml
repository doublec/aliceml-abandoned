(* Tools *)

let rec trunc n = truncate n;

open Sys;;
open Printf;;
open Gc;;
open Int32;;

let rec genlistaux n m acc = 
    if (n=0) then acc
    else genlistaux (n-1) (m+1) (m::acc);;

let genlist n = genlistaux n 1 [];;

let random n  = (n*25 + 1345) mod 10000 + (n*713 + 1345) mod 100000;;

let rec randlistaux n old l =
    if n=0 then l
    else
    let aux = random(old) in
	randlistaux (n-1) aux (aux::l);;

let randlist n = randlistaux n 0 [];;

let rec app l y =
  match l with
    [] -> y
  | x::xs -> x::app xs y;;

let gcverbose(onoff:bool) =
    let r = Gc.get () in
	r.verbose <- if onoff=true then 1+2+4+8+16+21+64 else 0;
(*	r.verbose <- onoff; *)
	Gc.set r;; 

gcverbose(false);;

let getheap () =
    let r        = Gc.stat () in
    let minor    = r.minor_words in
    let major    = r.major_words in
    let promoted = r.promoted_words in
    (minor+.major-.promoted)*.4.0;;

let rec dotimes n p =
    if (n=0) then ()
    else (p(); dotimes (n-1) p);;

let dobench proc =
    let start = Sys.time() *. 1000.0 and
	ignored = proc() and
	stop = Sys.time() *. 1000.0
    in (stop -. start);;

let rec  dobenchn n proc =
    if (n=0) then []
    else ( dobench proc)::(dobenchn(n-1) proc);;

let rec sum l res =
    match l with
    []    -> res
  | x::xs -> sum xs (res+.x);;


let avrg l len = (sum l 0.0)/.float(len);;

let rec dobenchavrg n proc =
    let aux = dobenchn n proc
    in ((avrg aux n),aux) ;;


let rec dobenchavrg2 name n sz proc1 proc2 =
    let (avrg1,l1) = dobenchavrg n proc1 and
	(avrg2,l2) = dobenchavrg n proc2
    in
	(name,
         sz,
	 avrg2-.avrg1,
	 l1,
	 l2);;


let rec print_float_list l =
    match l with
    []   -> ()
  | h::r -> print_float h; print_string " "; print_float_list r;;


let print_bench(bench,iter,(avrg,list)) =
    print_newline();
    print_string bench;
    print_string " ";
    print_int iter;
    print_string " ";
    print_float avrg;
    print_string " [";
    print_float_list list;
    print_string "]";
    print_newline();;

let print_bench2(name,n,avrg,list1,list2) =
    print_newline();
    print_string name; print_string ":";
    print_int n; print_string " ";
    print_float avrg;
    print_string " ["; print_float_list list1; print_string "] ";
    print_string " ["; print_float_list list2; print_string "]";
    print_newline();;


let check_usage n s =
    if (Array.length(Sys.argv)=n) then ()
    else (print_string "usage: ";
	  print_string s;
	  print_newline();
	  exit 1);;
    

(* set GC parameters *)
(*  let r = Gc.get () in *)
(*  	r.minor_heap_size      <- 64*1024*1024; *)
(*  	r.minor_heap_size      <- 4*1024*1024; *)
(*  	r.space_overhead       <- 100; *)
(*  	r.max_overhead         <- 1000000; *)
(*  	r.major_heap_increment <- 64*1024*1024; *)
(*  	r.major_heap_increment <- 4*1024*1024; *)
(*  	r.stack_limit          <- 1024*1024; *)
(*  	Gc.set r;;  *)

(*************************************************************)

let rec tak x y z =
  if y<x then (tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)) else z;;

let rec cpstakaux x y z k = 
   if y<x then
      (cpstakaux (x-1) y z
		 (function(v1)->
			  (cpstakaux (y-1) z x
				    (function(v2)->
					     (cpstakaux (z-1) x y
							(function(v3)->
							 (cpstakaux v1 v2 v3 k)))))))
   else
       (k z);;

let rec cpstak x y z = cpstakaux x y z (function(a)->a);;

(*************************************************************)

let rec fib n =
  if 2<n then fib(n-2) + fib(n-1) else 1;;

let rec fibf (n:float) =
  if 2.0<n then fibf(n-.2.0) +. fibf(n-.1.0) else 1.0;;


(*************************************************************)

let rec nrev = function [] -> []
  | (a::bs) ->  (app (nrev bs) [a]);;

(*************************************************************)

let rec quickaux (l: int list) acu =
    match l with
    [] -> acu
  | (a::bs) -> partition bs a [] [] acu

and

 partition (l:int list) a left right acu =
    match l with
    [] -> (quickaux left (a::(quickaux right acu)))
  | x::xs -> if x < a then partition xs a (x::left) right acu
	     else partition xs a left (x::right) acu;;


let rec quick n = (quickaux n []);;


let rec quickaux (l: int list) acu cmp =
    match l with
    [] -> acu
  | (a::bs) -> partition bs a [] [] acu cmp

and

 partition (l:int list) a left right acu cmp =
    match l with
    [] -> (quickaux left (a::(quickaux right acu cmp)) cmp)
  | x::xs -> if (cmp x a) then partition xs a (x::left) right acu cmp
	     else partition xs a left (x::right) acu cmp;;


let less x y = x<y;;

let rec quickho n cmp = (quickaux n [] cmp);;



let rec partitionarray1 ar pivot pindex low high =
    while !low <= !high do
	let old = (Array.get ar !low) in
	    if pivot > old then 
		((Array.set ar !low (Array.get ar !pindex));
		 (Array.set ar !pindex old);
		 pindex := (!pindex+1));
	    low := (!low+1)
    done;
    (!pindex-1);;

let rec partitionarray ar low high =
    let pivot = (Array.get ar low) in
	let mid = (partitionarray1 ar pivot (ref (low+1)) (ref (low+1)) (ref high)) in
	    (Array.set ar low (Array.get ar mid));
	    (Array.set ar mid pivot);
	    mid;;
	
let rec quickarray1 ar low high =
   if low < high then 
       let mid = (partitionarray ar low high)
       in
	   (quickarray1 ar low (mid-1));
	   (quickarray1 ar (mid+1) high)
   else ar;;

let quickarray ar = quickarray1 ar 0 ((Array.length ar) - 1);;





(*************************************************************)

let rec no_attack1 l c y i =
  match l with
    [] -> true
  | x::xr -> (x<>y) &&
             ((Int32.to_int (abs (Int32.of_int (x-y))))<>(c-i)) &&
             (no_attack1 xr c y (i+1));;


let rec no_attack xs c y = (no_attack1 xs c y 1);;

let rec lft ss c xs y n =
   if y>n then ss
   else if (no_attack xs c y) 
        then (lft ((app xs (y::[]))::ss) c xs (y+1) n)
        else (lft ss c xs (y+1) n);;


let rec doFoldL l z t c =
   match l with
     [] -> z
   | x::xr -> (doFoldL xr (lft z c x 1 t) t c);;


let rec loopForThread c t i =
   if c<=t then
	 (loopForThread (c+1) t (doFoldL i [] t c))
   else i;;

let rec queens n = (loopForThread 1 n [[]]);;

(*************************************************************)

let x_base = -2.0 and
    y_base = 1.25 and
    side = 2.5 and

    sz = 800 and
    maxCount = 1024;;

let delta = side /. (float sz);;

let rec mandelloop3 count z_re z_im c_re c_im = 
    if (count < maxCount) then 
	let
	    z_re_sq = z_re *. z_re and
	    z_im_sq = z_im *. z_im
	in
	    if ((z_re_sq +. z_im_sq) > 4.0) then count
	    else 
		let z_re_im = (z_re *. z_im)
		in 
		    (mandelloop3 
		     (count+1)
		     ((z_re_sq -. z_im_sq) +. c_re)
		     (z_re_im +. z_re_im +. c_im)
		     c_re c_im)

    else count;;

let rec mandelloop2 j c_im iter = 
    if (j >= sz) then iter
    else 
	let c_re = x_base *. (delta +. float j)
	in
	    let count = mandelloop3 0 c_re c_im c_re c_im
	    in  mandelloop2 (j+1) c_im (iter+count);;
	
let rec mandelloop1 i iter = 
    if (i >= sz) then iter
    else 
	let c_im : float = y_base -. (delta *. float i)
	in
	    mandelloop1 (i+1) (mandelloop2 0 c_im iter);;



let mandeliter () =
let i = ref 0 and
    j = ref 0 and
    k = ref 0 and
    count = ref 0 and
    sum_iterations = ref 0
in
    while (!i < sz) do
	let c_im = y_base -. (delta *. float !i) in
	    j := 0;
	    while (!j < sz) do
		let c_re = x_base *. (delta +. float !j) in
		    let z_re = ref c_re and
			z_im = ref c_im 
		    in
			k := maxCount;
			count := 0;
			while (!count < maxCount ) do
			    let z_re_sq = !z_re *. !z_re and
				z_im_sq = !z_im *. !z_im
			    in
				if ((z_re_sq +. z_im_sq) > 4.0)
				then (k := !count; 
				      count := maxCount)
				else 
				    let z_re_im = (!z_re *. !z_im)
				    in z_re := (z_re_sq -. z_im_sq) +. c_re;
				       z_im := z_re_im +. z_re_im +. c_im;
			    count := !count+1;
			done;
		j := !j+1;
	        sum_iterations := !sum_iterations + !k;
	      done;
	i := !i+1;
    done;
    !sum_iterations;;


(*************************************************************)

type expr =     Plus  of expr * expr
              | Minus of expr * expr
              | Var of int
              | Const of int
              | Times of expr * expr
              | Div of expr * expr
              | Exp of expr * int
              | Uminus of expr
              | Log of expr;;

let rec deriv exp x     = 
    match exp with
    Var(u)     -> if u=x then Const(1) else Const(0)
  | Const(u)   -> Const(0)
  | Plus(u,v)  -> Plus(deriv u x,deriv v x)
  | Minus(u,v) -> Minus(deriv u x,deriv v x)
  | Times(u,v) -> Plus(Times(deriv u x,v),Times(u,deriv v x))
  | Div(u,v)   -> Div(Minus(Times(deriv u x,v),
			     Times(u,deriv v x)),
		       Exp(v,2))
  | Exp(u,n)   -> Times(Times(deriv u x,Const(n)),Exp(u,n-1))
  | Uminus(u)  -> Uminus(deriv u x)
  | Log(u)     -> Div(deriv u x,u);;


let rec nthderiv n exp x = 
    if n=0 then exp
    else nthderiv (n-1) (deriv exp x) x;;

let goderiv n =
   dotimes n (function()-> nthderiv 6 (Exp(Div(Const(1),Var(1)),3)) 1);;



(*************************************************************)

let rec getit bench len =
      match bench with
        "fib"    -> (function()->(fib len))
      | "fibf"   -> (function()->(fibf(float(len)));1)
      | "tak"    -> (function()->(tak (len*3) (len*2) len))
      | "cpstak" -> (function()->(cpstak (len*3) (len*2) len))
      | "nrev"   -> let l = genlist len in (function()->(nrev l); 1)
      | "quick"  -> let l = (randlist 5000) in 
	(function()->(dotimes len (function()->(quick l))); 1)
      | "quickho"  -> let l = (randlist 5000) in 
	(function()->(dotimes len (function()->(quickho l less)));1)
      | "quickarray" -> let l = (randlist 5000) in 
	(function()->(dotimes len (function()->(quickarray (Array.of_list l))));1)
      | "queens" -> (function()->(queens len); 1)
      | "mandel" -> (function()->(mandelloop1 0 0))
      | "mandeliter" -> (function()->(mandeliter()))
      | "deriv"  -> (function()->(goderiv len); 1)
      |  x       -> (function()->(check_usage 0 "$0 [tak|fib|fibf] iter n");1);;


let doit(bench,iter,n) =
    print_bench (bench,n,(dobenchavrg iter (getit bench n)));;

let doitall(iter) =
    (
     doit("fib",iter,31);
     (*     doit("fibf",iter,31); *)
     doit("tak",iter,8);
(*     doit("cpstak",iter,8); *)
     doit("nrev",iter,3000);
     doit("quick",iter,30);
(*     doit("quickho",iter,30); *)
(*     doit("quickarray",iter,30);*)
(*     doit("queens",iter,10); *)
     (*     doit("mandel",iter,4711); *)
     doit("deriv",iter,50)
     );;

let main () =
    doitall(10); exit 0;;

main ();;


(*

compile using "make all"

example of usage:

       ./benches.ocamlc fib 5 31

runs 5 times fib(31) using the OCAML emulator

       ./benches.ocamlopt fib 5 31

ditto for the native code system

*)
