structure TypePrivate =
  struct

    datatype con_sort = OPEN | CLOSED

    datatype kind = STAR | ARROW of kind * kind		(* [kappa,k] *)

    type lab  = Lab.t					(* [lab,l] *)
    type path = Path.t					(* [pi,p] *)
    type con  = kind * con_sort * path			(* [chi,c] *)

    datatype typ' =					(* [tau',t'] *)
	  HOLE of kind * int	(* variable for inference *)
	| LINK of typ		(* forward (needed for unification) *)
	| MARK of typ'		(* for traversal *)
	| ARR  of typ * typ	(* arrow type *)
	| TUP  of typ list	(* tuple *)
	| ROW  of row		(* record *)
	| SUM  of row		(* sum type (datatype) *)
	| VAR  of kind * int	(* bound variable or skolem types *)
	| CON  of con		(* constructor (of arbitrary kind) *)
	| ALL  of alpha * typ	(* universal quantification *)
	| EX   of alpha * typ	(* existential quantification *)
	| LAM  of alpha * typ	(* abstraction (type function) *)
	| APP  of typ * typ	(* application *)
	| REC  of typ		(* recursive type barrier *)

    and row = NIL | RHO | FLD of lab * typ list * row	(* [rho,r] *)

    withtype typ = typ' ref				(* [tau,t] *)
    and    alpha = typ' ref				(* [alpha,a] *)

    type t = typ

    type rea = typ PathMap.t

(*DEBUG*)
fun pr(ARR(t1,t2))	= "ARR"
| pr(TUP ts)	= "TUP"
| pr(ROW r)	= "ROW"
| pr(SUM r)	= "SUM"
| pr(CON c)	= "CON"
| pr(ALL(a,t))	= "ALL"
| pr(EX(a,t))	= "EX"
| pr(LAM(a,t))	= "LAM"
| pr(APP(t1,t2))= "APP"
| pr(REC _) = "REC"
| pr(LINK _) = "LINK"
| pr(MARK _) = "MARK"
| pr(HOLE _) = "HOLE"
| pr(VAR _)  = "VAR"

    (*
     * We establish the following invariants:
     * - rows are sorted by label
     * - types are always in normal form
     * - directly nested ALLs appear in depth-first leftmost traversal order
     * - same for nested EXs
     *)


    (* Level management *)

    val level = ref 1

    fun enterLevel() = level := !level+1
    fun exitLevel()  = level := !level-1


    (* Follow a path of links (performing path compression on the fly) *)

    fun follow'(ref(LINK t))	= follow' t
      | follow' t		= t

(*DEBUG*)
    fun follow(t as ref(LINK u))= follow u(*let val v = follow' u in t := LINK v ; v end*)
      | follow t		= t


    (* Infer the kind of a type *)

    fun rangeKind(ARROW(k1,k2))	= k2
      | rangeKind  _		= Crash.crash "Type.rangeKind: kind mismatch"


    fun kind(ref t')		= kind' t'

    and kind'(LINK t | REC t)	= kind t
      | kind'(HOLE(k,_))	= k
      | kind'(VAR(k,_))		= k
      | kind'(CON(k,_,_))	= k
      | kind'(LAM(a,t))		= ARROW(kind a, kind t)
      | kind'(APP(t1,t2))	= rangeKind(kind t1)
      | kind' _			= STAR

    val kindVar			= kind


    (* Reduction to some sort of head normal form *)

    fun reduce1(t as ref(APP(t1,t2))) =	(* beta *)
	(case !(follow t1)
	   of LAM(a,t3) =>
		( a := LINK t2
		; t := LINK t3
		; reduce1 t
		)
	    | _ => ()
	)
      | reduce1(t as ref(LAM(a,t1))) =	(* eta *)
	(case !(follow t1)
	   of APP(t2,t3) =>
		if a = t3 then
		( t := LINK t2
		; reduce1 t
		)
		else ()
	    | _ => ()
	)
      | reduce1 _ = ()


    (* Projections and extractions *)

    exception Type

    fun asType(ref(LINK t | REC t))	= asType t
      | asType(t as ref(APP _ | LAM _))	= ( reduce1 t ; asType' t )
      | asType(ref t')			= t'

    and asType'(ref(t' as (APP _ | LAM _))) = t'
      | asType' t			= asType t

    fun isUnknown t	= case asType t of HOLE _ => true | _ => false
    fun isArrow t	= case asType t of ARR _ => true | _ => false
    fun isTuple t	= case asType t of TUP _ => true | _ => false
    fun isRow t		= case asType t of ROW _ => true | _ => false
    fun isSum t		= case asType t of SUM _ => true | _ => false
    fun isVar t		= case asType t of VAR _ => true | _ => false
    fun isCon t		= case asType t of CON _ => true | _ => false
    fun isAll t		= case asType t of ALL _ => true | _ => false
    fun isExist t	= case asType t of EX  _ => true | _ => false
    fun isLambda t	= case asType t of LAM _ => true | _ => false
    fun isApp t		= case asType t of APP _ => true | _ => false
    fun isRec t		= case asType t of REC _ => true | _ => false

    fun asArrow t	= case asType t of ARR tt => tt | _ => raise Type
    fun asTuple t	= case asType t of TUP ts => ts | _ => raise Type
    fun asRow t		= case asType t of ROW r  => r  | _ => raise Type
    fun asSum t		= case asType t of SUM r  => r  | _ => raise Type
    fun asVar t		= case asType t of VAR _  => t  | _ => raise Type
    fun asCon t		= case asType t of CON c  => c  | _ => raise Type
    fun asAll t		= case asType t of ALL at => at | _ => raise Type
    fun asExist t	= case asType t of EX  at => at | _ => raise Type
    fun asLambda t	= case asType t of LAM at => at | _ => raise Type
    fun asApp t		= case asType t of APP tt => tt | _ => raise Type
    fun asRec t		= case asType t of REC t  => t  | _ => raise Type

    fun pathCon(_,_,p)	= p
    fun path t		= pathCon(asCon t)


    (* Traversal *)

    fun app1'(( HOLE _
	      | VAR _
	      | CON _ ), f)		= ()
      | app1'(( LINK t
	      | REC t
	      | ALL(_,t)
	      | EX (_,t)
	      | LAM(_,t)), f)		= f t
      | app1'(( ARR(t1,t2)
	      | APP(t1,t2)), f)		= ( f t1 ; f t2 )
      | app1'(( TUP ts ), f)		= List.app f ts
      | app1'(( ROW r
	      | SUM r ), f)		= appRow(r,f)
      | app1'(( MARK _ ), f)		= Crash.crash "Type.app: bypassed MARK"

    and appRow(FLD(_,ts,r), f)		= ( List.app f ts ; appRow(r,f) )
      | appRow(_, f)			= ()


    fun foldl1'(( HOLE _
		| VAR _
		| CON _ ), f, a)	= a
      | foldl1'(( LINK t
		| REC t
		| ALL(_,t)
		| EX (_,t)
		| LAM(_,t)), f, a)	= f(t,a)
      | foldl1'(( ARR(t1,t2)
		| APP(t1,t2)), f, a)	= f(t2, f(t1,a))
      | foldl1'(( TUP ts ), f, a)	= List.foldl f a ts
      | foldl1'(( ROW r
		| SUM r ), f, a)	= foldlRow(r,f,a)
      | foldl1'(( MARK _ ), f, a)	= Crash.crash"Type.foldl: bypassed MARK"

    and foldlRow(FLD(_,ts,r), f, a)	= foldlRow(r, f, List.foldl f a ts)
      | foldlRow(_, f, a)		= a


    fun unmark(t as ref(MARK t')) 	= ( t := t' ; app1'(t', unmark) )
(*DEBUG*)
before (case t' of MARK _ => Crash.crash "Type.unmark: double mark" | _ =>())
before print"MARK deleted\n"
      | unmark _	            	= ()


    fun app f t =
    (* If f wants to assign a note it has to use update! *)
	let
	    fun app(ref(MARK _)) = ()
	      | app t =
		let
		    val _  = f t
		    val t' = !t
		    val _  = t := MARK t'
(*DEBUG*)
before print"MARK set (by app)\n"
		in
		    app1'(t',app)
		end
	in
	    app t before unmark t
	end

    fun foldl f a t =
    (* If f wants to assign a note it has to use update! *)
	let
	    fun fold(ref(MARK _), a) = a
	      | fold(t, a) =
		let
		    val a' = f(t,a)
		    val t' = !t
		    val _  = t := MARK t'
(*DEBUG*)
before print"MARK set (by foldl)\n"
		in
		    foldl1'(t',fold,a')
		end
	in
	    fold(t,a) before unmark t
	end


    fun update(t as ref(MARK _), t')	= t := MARK t'
(*DEBUG*)
before print"MARK updated\n"
      | update(t, t')			= t := t'


    (* Extraction of holes and paths *)
(*
    fun holes t = foldl (fn(t as ref(HOLE _), a) => t::a | (t,a) => a) t
    fun paths t = foldl (fn(t as ref(CON c), a) => pathCon(c)::a | (t,a) => a) t
*)

    fun paths t =
    	let
	    val s = PathSet.new()
	in
	    app (fn(ref(CON c)) => PathSet.insert(s, pathCon c) | _ => ()) t ;
	    s
	end


    (* Cloning under a type realisation *)

    fun realise phi t =
	let
	    (* We want to be able to handle recursive types, so we have to
	     * implement graph copying here.
	     *)

	    val trail = ref []

	    fun dup'(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
(*DEBUG*)
before print"MARK set (by dup')\n"
		    val _   = t1 := LINK t2
		in
		    t2
		end

	    fun dup(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
(*DEBUG*)
before print"MARK set (by dup)\n"
		    val _   = t1 := LINK t2
		    val t2' = MARK(clone' t1')
(*DEBUG*)
before print"MARK updated (by dup)\n"
		    val _   = t2 := t2'
		in
		    t2
		end

	    and clone t =
		let val t1 = follow t in
		    case !t1 of (MARK _ | VAR _ | HOLE _) => t1
			      | t1'                       => dup t1
		end

	    and clone'(ARR(t1,t2))	= ARR(clone t1, clone t2)
	      | clone'(TUP ts)		= TUP(List.map clone ts)
	      | clone'(ROW r)		= ROW(cloneRow r)
	      | clone'(SUM r)		= SUM(cloneRow r)
	      | clone'(CON c)		= CON c (*UNFINISHED: do rea+red*)
	      | clone'(ALL(a,t))	= ALL(dup' a, clone t)
	      | clone'(EX(a,t))		= EX(dup' a, clone t)
	      | clone'(LAM(a,t))	= LAM(dup' a, clone t)
	      | clone'(APP(t1,t2))	= APP(clone t1, clone t2)
	      | clone'(REC t)		= REC(clone t)
(*DEBUG*)
| clone'(LINK _) = Crash.crash "Type.clone: LINK"
| clone'(MARK _) = Crash.crash "Type.clone: MARK"
| clone'(HOLE _) = Crash.crash "Type.clone: HOLE"
| clone'(VAR _)  = Crash.crash "Type.clone: VAR"
(*
	      | clone' _		= Crash.crash "Type.clone"
*)
	    and cloneRow(FLD(l,ts,r))	= FLD(l, List.map clone ts, cloneRow r)
	      | cloneRow r		= r

	    val t2 = clone t
	in
	    List.app op:= (!trail) ;
	    unmark t2 ;
	    t2
	end


    fun clone t = realise (PathMap.new()) t


    (* Creation and injections *)

    fun unknown' k	= HOLE(k, !level)
    fun unknown k	= ref(unknown' k)

    fun inArrow tt	= ref(ARR tt)
    fun inTuple ts	= ref(TUP ts)
    fun inRow r		= ref(ROW r)
    fun inSum r		= ref(SUM r)
    fun inVar a		= a
    fun inCon c		= ref(CON c)
    fun inAll at	= ref(ALL at)
    fun inExist at	= ref(EX at)
    fun inLambda at	= let val t = ref(LAM at) in reduce1 t ; t end
    fun inApp(t1,t2)	= let val t = ref(APP(clone t1,t2)) in reduce1 t ; t end
    fun inRec t		= ref(REC t)

    fun var k		= ref(VAR(k, !level))


    (* Instantiation: instantiate universally quantified types, skolemise
     * existentially qualified types. If there is any quantification,
     * then we have to copy the type.
     * Skolemisation does it the other way round (needed for checking rank 2
     * signature applications).
     *)

    fun instance'(ref(ALL(a,t)))	= ( a := unknown'(kind a); instance' t )
      | instance'(ref(EX(a,t)))		= instance' t
      | instance' t			= t

    fun instance(t as ref(ALL _| EX _))	= instance'(clone t)
      | instance(t as ref(LAM _))	= clone t
      | instance t			= t

(*DEBUG*)
    fun instance(t as ref(ALL(a,_)| EX(a,_))) =
(case !a of VAR _ => () | _ => print"quantifier 1 ARGH!\n";
let val t2 as ref(ALL(a2,_)|EX(a2,_)) = clone t in
case !a2 of VAR _ => () 
| MARK _ => print"quantifier 2 ARGH: MARK!\n"
| _ => print"quantifier 2 ARGH!\n";
if a = a2 then print"clone ARGH!\n" else ();
instance' t2 before
(case !a2 of HOLE _ => () | _ => print"instance ARGH!\n")
end)
      | instance(t as ref(LAM _))	= clone t
      | instance t			= t


    fun skolem'(ref(ALL(a,t)))		= skolem' t
      | skolem'(ref(EX(a,t)))		= ( a := unknown' STAR ; skolem' t )
      | skolem' t			= t

    fun skolem(t as ref(ALL _| EX _))	= skolem'(clone t)
      | skolem t			= t



    (* Operations on rows *)

    exception Row

    fun unknownRow()			= RHO
    fun emptyRow()			= NIL

    fun extendRow(l,ts, r as (RHO|NIL))	= FLD(l,ts,r)
      | extendRow(l1,ts1, r1 as FLD(l2,ts2,r2)) =
	case Lab.compare(l1,l2)
	  of EQUAL   => raise Row
	   | LESS    => FLD(l1, ts1, r1)
	   | GREATER => FLD(l2, ts2, extendRow(l1,ts2,r2))

    fun tupToRow ts =
	let
	    fun loop(n,  []  ) = NIL
	      | loop(n, t::ts) = FLD(Lab.fromInt n, [t], loop(n+1,ts))
	in
	    loop(1,ts)
	end


    (* Closure *)

    fun close t =
    	let
	    fun close(a as ref(HOLE(k,n)), f) =
		if n > !level then
		    ( update(a, VAR(k,n)) ; fn t => f(inAll(a,t)) )
		else f

	      | close(a as ref(VAR(k,n)), f) =
		(* We have to quantify over VARs as well.
		 * The reason is that there may be several types being
		 * closed that share parts of their graphs.
		 * Note that this means we have to take care to choose
		 * a unique upper level when we use VAR for skolem types.
		 *)
		if n > !level then
		    fn t => f(inAll(a,t))
		else f

	      | close(_, f) = f
	in
	    foldl close (fn t => t) t t
	end


    (* Unification *)

    exception Unify of typ * typ


    fun occurs(t1,t2) =
	let
(*DEBUG*)
val _=print">occurs\n"
	    exception Occurs

	    fun occurs(t2 as ref t2') =
		if t1 = t2 then
		    raise Occurs
		else
		    ( t2 := MARK t2' ; occurs' t2' )
(*DEBUG*)
before print"MARK set (by occurs)\n"

	    and occurs'(REC _ | MARK _) = ()
	      | occurs' t2'             = app1'(t2', occurs)
	in
	    (( occurs t2 ; false ) handle Occurs => true) before unmark t2
(*DEBUG*)
before print"<occurs\n"
	end

    fun unify(t1,t2) =
	let
	    val t1 as ref t1' = follow t1
	    val t2 as ref t2' = follow t2

	    fun recurse f x =
		( t1 := LINK t2
		; f x handle Unify tt => ( t1 := t1' ; raise Unify tt )
		)

	    fun unifyPair((t11,t12), (t21,t22)) =
		( unify(t11,t21) ; unify(t12,t22) )

	    fun unifyRow(r1, r2, ROWorSUM) =
		let
		    fun loop(NIL, false, NIL, false) = NIL
		      | loop(NIL, false, RHO, _    ) = NIL
		      | loop(RHO, _,     NIL, false) = NIL
		      | loop(RHO, _,     RHO, _    ) = RHO
		      | loop(RHO, _, FLD(l,ts,r), b2) =
			    FLD(l,ts, loop(RHO, true, r, b2))
		      | loop(FLD(l,ts,r), b1, RHO, _) =
			    FLD(l,ts, loop(r, b1, RHO, true))
		      | loop(r1 as FLD(l1,ts1,r1'), b1,
			     r2 as FLD(l2,ts2,r2'), b2) =
			(case Lab.compare(l1,l2)
			   of EQUAL   => ( ListPair.app unify (ts1,ts2)
					 ; FLD(l1,ts1, loop(r1',b1, r2',b2)) )
			    | LESS    => FLD(l1,ts1, loop(r1',b1, r2,true))
			    | GREATER => FLD(l2,ts2, loop(r1,true, r2',b2))
			)
		      | loop _ = raise Unify(t1,t2)
		in
		    t2 := ROWorSUM(loop(r1,false, r2,false))
		end
	in
	    if t1 = t2 then () else
	    case (t1',t2')
	      of (HOLE(k1,n1), HOLE(k2,n2)) =>
		 if k1 <> k2 then
		     raise Unify(t1,t2)
		 else
		     if n1 < n2 then t2 := LINK t1
				else t1 := LINK t2

	       | (HOLE(k1,_), _) =>
		 if k1 <> kind' t2' orelse occurs(t1,t2) then
		     raise Unify(t1,t2)
		 else
		     t1 := LINK t2

	       | (_, HOLE(k2,_)) =>
		 if k2 <> kind' t1' orelse occurs(t2,t1) then
		     raise Unify(t1,t2)
		 else
		     t2 := LINK t1

	       | (ARR(tt1), ARR(tt2)) =>
		 recurse unifyPair (tt1,tt2)

	       | (TUP(ts1), TUP(ts2)) =>
		 recurse (ListPair.app unify) (ts1,ts2)

	       | (TUP(ts), ROW(r)) =>
		 recurse unifyRow (tupToRow ts, r, ROW)

	       | (ROW(r), TUP(ts)) =>
		 recurse unifyRow (r, tupToRow ts, ROW)

	       | (ROW(r1), ROW(r2)) =>
		 recurse unifyRow (r1,r2,ROW)

	       | (SUM(r1), SUM(r2)) =>
		 recurse unifyRow (r1,r2,SUM)

	       | (CON(_,_,p1), CON(_,_,p2)) =>
		 if p1 = p2 then t1 := LINK t2
			    else raise Unify(t1,t2)

	       | (ALL(a1,t1), ALL(a2,t2)) =>
		 Crash.crash "Type.unify: universal quantification"

	       | (EX(a1,t1), EX(a2,t2)) =>
		 Crash.crash "Type.unify: existential quantification"

	       | (LAM(a1,t1), LAM(a2,t2)) =>
		 Crash.crash "Type.unify: abstraction"

	       | (APP(tt1), APP(tt2)) =>
		 recurse unifyPair (tt1,tt2)

	       | (REC(t1), _) =>
		 unify(t1,t2)

	       | (_, REC(t2)) =>
		 unify(t1,t2)

	       | _ => raise Unify(t1,t2)
	end


    (* Unification of lists *)

    exception UnifyList of int * typ * typ

    fun unifyList  []    = ()
      | unifyList(t::ts) = 
	let
	    fun loop(n,   []   ) = ()
	      | loop(n, t'::ts') =
	        ( unify(t,t')
		  handle Unify(t1,t2) => raise UnifyList(n,t1,t2)
		; loop(n+1, ts')
		)
	in
	    loop(0,ts)
	end

  end


structure Type : TYPE = TypePrivate
