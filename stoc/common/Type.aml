(*******************************************************************************
On Type Functions:

We evaluate type application lazily. Reduction is triggered on demand, i.e. by
unification or by inspection. Not all applications can be reduced (in
particular applications to abstract type constructors) so we deal with some
sort of head normal form.

We don't normally do eta-reduction, since it is expensive. During unification
eta reduction is not needed since lambdas may only show up there under very
restrictive circumstances anyway (see below). Eta-reduction is still needed
however for equals, to equate eta-convertible type functions. It's done on
demand.

On Sharing:

For efficiency reasons, we try to maximise sharing of type structures. This
reduces space cost and speeds up comparison and unification, as a simple
reference comparison often suffices. Not only unification but even the equality
test (used by module type checking) merges types silently whenever possible.

There are several cases where we have to clone a type:

* Type functions before reduction of type application.
* Type schemes (types with toplevel quantifiers) on instantiation.
* All sorts of types on signature instantiation.

For the first two it would be sufficient to only clone subgraphs containing the
variables bound by the lambdas/quantifiers; for the third we only would need to
clone subgraphs containing signature-local constructors. But since minimizing
cloning is difficult in the presence of cycles (we have recursive types) and
the pay-off is unclear, we currently don't do it.

It is very important to maintain sharing between subgraphs of different type
terms on signature instantiation. Not doing this can lead to a quadratic blowup
of type terms during module type checking, particularly desastrous in the
presence of structural datatypes. Therefor we need a special-and-ugly interface
for cloning a sequence of related types.

On Recursive Types:

Stockhausen makes datatypes structural sum types. This is a problem since
SML has non-uniform recursive datatypes. So in fact we not only get recursive
types but recursive type functions. It is not obvious whether type checking
remains decidable in the general case (in our current scheme with lazy
application unification might fail to terminate).

To stay on the safe side we treat recursive type functions specially:
applications to such functions are never reduced. That is, there is no implicit
unrolling of recursive types, mu is completely structural. For two recursive
types to be compatible they must therefor be constructed from equal type
functions. As a consequence, List(Int) and IntList will not be compatible in
the following example:

	List a  = Nil | Cons(a, List a)
	IntList = Nil | Cons(Int, IntList)

Maybe this can be made a bit more permissive, but in general it seems
impossible to have non-uniform datatypes as well as arbitrary recursive types -
type checking would become undecidable.

It is also unclear to me whether some sort of hash-consing can be applied to
recursive types or even recursive type functions.

On Type Abbreviations:

We try to maintain type abbreviations to generate more user friendly output.
This is managed through a special node ABBREV, which refers to the original
type term as well as to the reduced/substituted one.

*******************************************************************************)

structure TypePrivate =
  struct

  (* Types *)

    datatype sort = OPEN | CLOSED
    datatype kind = STAR | ARROW of kind * kind		(* [kappa,k] *)

    type lab  = Label.t					(* [lab,l] *)
    type path = Path.t					(* [pi,p] *)
    type con  = kind * sort * path			(* [chi,c] *)

    datatype typ' =					(* [tau',t'] *)
	  HOLE   of kind * int	(* variable for inference *)
	| LINK   of typ		(* forward (needed for unification) *)
	| MARK   of typ'	(* for traversal *)
	| FUN    of typ * typ	(* arrow type *)
	| TUPLE  of typ list	(* tuple *)
	| PROD   of row		(* record *)
	| SUM    of row		(* sum type (datatype) *)
	| VAR    of kind * int	(* bound variable or skolem types *)
	| CON    of con		(* constructor (of arbitrary kind) *)
	| ALL    of var * typ	(* universal quantification *)
	| EXIST  of var * typ	(* existential quantification *)
	| LAMBDA of var * typ	(* abstraction (type function) *)
	| APPLY  of typ * typ	(* application *)
	| MU     of typ		(* recursive type barrier *)
	| ABBREV of typ * typ	(* abbreviations *)

    and row =						(* [rho,r] *)
	  NIL
	| RHO   of int ref * row
	| FIELD of lab * typ list * row

    withtype typ = typ' ref				(* [tau,t] *)
    and      var = typ' ref				(* [alpha,a] *)

    type t = typ

    type path_rea = path PathMap.t
    type typ_rea  = typ PathMap.t

    (*
     * We establish the following invariants:
     * - rows are sorted by label, rho variables appear as head only
     * - types are always in head normal form
     * - sequential quantifiers are ordered such that the bound variables
     *   appear in depth-first leftmost traversal order inside the body
     *)


(*DEBUG*)
    fun pr(FUN _)	= "ARROW"
      | pr(TUPLE _)	= "TUPLE"
      | pr(PROD _)	= "PROD"
      | pr(SUM _)	= "SUM"
      | pr(CON _)	= "CON"
      | pr(VAR _)	= "VAR"
      | pr(ALL _)	= "ALL"
      | pr(EXIST _)	= "EXIST"
      | pr(LAMBDA _)	= "LAMBDA"
      | pr(APPLY _)	= "APPLY"
      | pr(MU _)	= "MU"
      | pr(LINK _)	= "LINK"
      | pr(MARK _)	= "MARK"
      | pr(HOLE _)	= "HOLE"
      | pr(ABBREV _)	= "ABBREV"


  (* Level management *)

    val globalLevel	= 0
    val level		= ref(globalLevel+1)

    fun enterLevel()	= level := !level+1
    fun exitLevel()	= level := !level-1
    fun resetLevel()	= level := 1


  (* Follow a path of links (performing path compression on the fly) *)

    fun follow(ref(LINK t))	= follow t
      | follow t		= t

(*DEBUG
    fun follow'(ref(LINK t))	= follow' t
      | follow' t		= t

    fun follow(t as ref(LINK u))= let val v = follow' u in t := LINK v ; v end
      | follow t		= t
*)


  (* Kind inference *)

    fun rangeKind(ARROW(k1,k2))	= k2
      | rangeKind  _		= raise Crash.Crash "Type.rangeKind: \
						    \kind mismatch"

    fun kind(ref t')		= kind' t'

    and kind'(LINK t | MU t)	= kind t
      | kind'(ABBREV(_,t))	= kind t
      | kind'(HOLE(k,_))	= k
      | kind'(VAR(k,_))		= k
      | kind'(CON(k,_,_))	= k
      | kind'(LAMBDA(a,t))	= ARROW(kind a, kind t)
      | kind'(APPLY(t1,t2))	= rangeKind(kind t1)
      | kind'(MARK t')		= kind' t'
      | kind' _			= STAR

    val kindVar			= kind


  (* Type graph traversal *)

    fun app1'(( HOLE _
	      | VAR _
	      | CON _ ), f)		= ()
      | app1'(( LINK t
	      | MU t
	      | ALL(_,t)
	      | EXIST(_,t)
	      | LAMBDA(_,t)), f)	= f t
      | app1'(( FUN(t1,t2)
	      | APPLY(t1,t2)
	      | ABBREV(t1,t2)), f)	= ( f t1 ; f t2 )
      | app1'(( TUPLE ts ), f)		= List.app f ts
      | app1'(( PROD r
	      | SUM r ), f)		= appRow(r,f)
      | app1'(( MARK _ ), f)		= raise Crash.Crash "Type.app: MARK"

    and appRow(FIELD(_,ts,r), f)	= ( List.app f ts ; appRow(r,f) )
      | appRow(RHO(_,r), f)		= appRow(r,f)
      | appRow(NIL, f)			= ()


    fun foldl1'(( HOLE _
		| VAR _
		| CON _ ), f, a)	= a
      | foldl1'(( LINK t
		| MU t
		| ALL(_,t)
		| EXIST(_,t)
		| LAMBDA(_,t)), f, a)	= f(t,a)
      | foldl1'(( FUN(t1,t2)
		| APPLY(t1,t2)
		| ABBREV(t1,t2)), f, a)	= f(t2, f(t1,a))
      | foldl1'(( TUPLE ts ), f, a)	= List.foldl f a ts
      | foldl1'(( PROD r
		| SUM r ), f, a)	= foldlRow(r,f,a)
      | foldl1'(( MARK _ ), f, a)	= raise Crash.Crash "Type.foldl: MARK"

    and foldlRow(FIELD(_,ts,r), f, a)	= foldlRow(r, f, List.foldl f a ts)
      | foldlRow(RHO(_,r), f, a)	= foldlRow(r, f, a)
      | foldlRow(NIL, f, a)		= a


    fun unmark(t as ref(MARK t')) 	=
(*ASSERT				  assert t' of non MARK _ => *)
( case t' of MARK _ => raise Assert.failure | _ =>
					  ( t := t' ; app1'(t', unmark) )
)
      | unmark _	            	= ()


    fun app f t =
	let
	    fun app(ref(MARK _)) = ()
	      | app t =
		let
		    val _  = f t
		    val t' = !t
		    val _  = t := MARK t'
		in
		    app1'(t',app)
		end
	in
	    app t before unmark t handle e => ( unmark t ; raise e )
	end

    fun foldl f a t =
	let
	    fun fold(ref(MARK _), a) = a
	      | fold(t, a) =
		let
		    val a' = f(t,a)
		    val t' = !t
		    val _  = t := MARK t'
		in
		    foldl1'(t',fold,a')
		end
	in
	    fold(t,a) before unmark t handle e => ( unmark t ; raise e )
	end


  (* Cloning *)

    fun clone t =
	let
	    (* We want to be able to handle recursive types, so we have to
	     * implement graph copying here.
	     *)

	    val trail = ref []

	    fun dup'(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
		    val _   = t1 := LINK t2
		in
		    t2
		end

	    fun dup(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
		    val _   = t1 := LINK t2
		    val t2' = MARK(clone' t1')
		    val _   = t2 := t2'
		in
		    t2
		end

	    and clone t1 =
		let val t11 = follow t1 in
		    case !t11 of (MARK _ | VAR _ | HOLE _) => t11
			       | t11'                      => dup t11
		end

	    and clone'(FUN(t1,t2))	= FUN(clone t1, clone t2)
	      | clone'(TUPLE ts)	= TUPLE(List.map clone ts)
	      | clone'(PROD r)		= PROD(cloneRow r)
	      | clone'(SUM r)		= SUM(cloneRow r)
	      | clone'(CON c)		= CON c
	      | clone'(ALL(a,t))	= ALL(dup' a, clone t)
	      | clone'(EXIST(a,t))	= EXIST(dup' a, clone t)
	      | clone'(LAMBDA(a,t))	= LAMBDA(dup' a, clone t)
	      | clone'(APPLY(t1,t2))	= APPLY(clone t1, clone t2)
	      | clone'(MU t)		= MU(clone t)
	      | clone'(ABBREV(t1,t2))	= ABBREV(clone t1, clone t2)
	      | clone' _		= raise Crash.Crash "Type.clone"

	    and cloneRow(FIELD(l,ts,r))	= FIELD(l,List.map clone ts, cloneRow r)
	      | cloneRow(RHO(n,r))	= RHO(ref(!n), cloneRow r)
	      | cloneRow(NIL)		= NIL

	    val t2 = clone t
	in
	    List.app op:= (!trail) ;
	    unmark t2 ;
	    t2
	end


  (* Continuous cloning *)

    type clone_state = { trail: (typ * typ') list ref, typs: typ list ref }

    fun cloneStart() = {trail = ref [], typs = ref []}
    fun cloneFinish{trail,typs} = 
	( List.app op:= (!trail)
	; List.app unmark (!typs)
	)

    fun cloneCont {trail,typs} t =
	let
	    fun dup'(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
		    val _   = t1 := LINK t2
		in
		    t2
		end

	    fun dup(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
		    val _   = t1 := LINK t2
		    val t2' = MARK(clone' t1')
		    val _   = t2 := t2'
		in
		    t2
		end

	    and clone t1 =
		let val t11 = follow t1 in
		    case !t11 of (MARK _ | VAR _ | HOLE _) => t11
			       | t11'                      => dup t11
		end

	    and clone'(FUN(t1,t2))	= FUN(clone t1, clone t2)
	      | clone'(TUPLE ts)	= TUPLE(List.map clone ts)
	      | clone'(PROD r)		= PROD(cloneRow r)
	      | clone'(SUM r)		= SUM(cloneRow r)
	      | clone'(CON c)		= CON c
	      | clone'(ALL(a,t))	= ALL(dup' a, clone t)
	      | clone'(EXIST(a,t))	= EXIST(dup' a, clone t)
	      | clone'(LAMBDA(a,t))	= LAMBDA(dup' a, clone t)
	      | clone'(APPLY(t1,t2))	= APPLY(clone t1, clone t2)
	      | clone'(MU t)		= MU(clone t)
	      | clone'(ABBREV(t1,t2))	= ABBREV(clone t1, clone t2)
	      | clone' _		= raise Crash.Crash "Type.clone"

	    and cloneRow(FIELD(l,ts,r))	= FIELD(l,List.map clone ts, cloneRow r)
	      | cloneRow(RHO(n,r))	= RHO(ref(!n), cloneRow r)
	      | cloneRow(NIL)		= NIL

	    val t2 = clone t
	in
	    typs := t2 :: !typs ;
	    t2
	end


  (* Occur check (not used by unification) *)

    exception Occurs

    fun occurs(t1,t2) =
	let
	    fun occurs t = if t1 = t then raise Occurs else ()
	in
	    ( app occurs t2 ; false ) handle Occurs => true
	end


  (* Reduction to head normal form *)

    (*UNFINISHED: avoid multiple cloning of curried lambdas somehow *)

    fun reduce(t as ref(APPLY(t1,t2))) =
	let
	    fun reduceApply(t1 as ref(LAMBDA(a,_)), to) =
		( t := HOLE(kind a, !level)
		; case !(clone t1)
		    of LAMBDA(a,t11) =>
			( a := LINK t2
			; t := (case to
				  of NONE   => LINK t11
				   | SOME t => ABBREV(ref(APPLY(t,t2)), t11) )
			; reduce t
			)
		    | _ => raise Crash.Crash "Type.reduceApply"
		)
	      | reduceApply(ref(LINK t11), to) =
		    reduceApply(follow t11, to)

	      | reduceApply(ref(ABBREV(t11,t12)), to) =
		    reduceApply(follow t12, SOME(Option.getOpt(to,t11)))
	      (*
	      | reduceApply(ref(MU t11), to) =
		    reduceApply(follow t11, to)
	      *)
	      | reduceApply _ = ()
	in
	    reduceApply(t1, NONE)
	end

      | reduce(ref(LINK t | ABBREV(_,t))) = reduce t

      | reduce _ = ()


    (*
     * We don't normally do eta-reduction, since it is expensive.
     * During unification we curently allow no lambdas anyway.
     * Eta-reduction is still needed however for equals, to equate
     * eta-convertible type functions. It's done on demand.
     *)

    fun reduceEta(t as ref(LAMBDA _)) =
	let
	    fun reduceLambda(ref(LAMBDA(a,t1)), vs) =
		reduceLambda(t1, a::vs)

	      | reduceLambda(ref(APPLY(t1,t2)), a::vs) =
		let
		    val t2' = follow t2
		    val a'  = follow a
		in
		    if t2' = a' andalso not(occurs(a',t1)) then
			reduceLambda(t1, vs)
		    else
			()
		end

	      | reduceLambda(t1, []) =
		    ( t := LINK t1 ; reduceEta t1 )

	      | reduceLambda(ref(LINK t1), vs) =
		    reduceLambda(t1, vs)

	      | reduceLambda(ref(ABBREV(t1,t2)), vs) =
		    reduceLambda(t2, vs)

	      | reduceLambda _ = ()
	in
	    reduceLambda(t, [])
	end

      | reduceEta(ref(LINK t)) = reduceEta t

      | reduceEta _ = ()


  (* Creation and injections *)

    fun unknown' k	= HOLE(k, !level)
    fun unknown k	= ref(unknown' k)

    fun inArrow tt	= ref(FUN tt)
    fun inTuple ts	= ref(TUPLE ts)
    fun inProd r	= ref(PROD r)
    fun inSum r		= ref(SUM r)
    fun inVar a		= a
    fun inCon c		= ref(CON c)
    fun inAll at	= ref(ALL at)
    fun inExist at	= ref(EXIST at)
    fun inLambda at	= ref(LAMBDA at)
    fun inApply(t1,t2)	= let val t = ref(APPLY(t1,t2)) in reduce t ; t end
    fun inMu t		= ref(MU t)
    fun inAbbrev(t1,t2)	= ref(ABBREV(t1,t2))

    fun var k		= ref(VAR(k, !level))


  (* Projections and extractions *)

    exception Type

    fun asType(ref(LINK t))		= asType t
      | asType(ref(ABBREV(_,t)))	= asType t
      | asType(ref t')			= t'

    fun isUnknown t	= case asType t of HOLE _   => true | _ => false
    fun isArrow t	= case asType t of FUN _    => true | _ => false
    fun isTuple t	= case asType t of TUPLE _  => true | _ => false
    fun isProd t	= case asType t of PROD _   => true | _ => false
    fun isSum t		= case asType t of SUM _    => true | _ => false
    fun isVar t		= case asType t of VAR _    => true | _ => false
    fun isCon t		= case asType t of CON _    => true | _ => false
    fun isAll t		= case asType t of ALL _    => true | _ => false
    fun isExist t	= case asType t of EXIST _  => true | _ => false
    fun isLambda t	= case asType t of LAMBDA _ => true | _ => false
    fun isApply t	= case asType t of APPLY _  => true | _ => false
    fun isMu t		= case asType t of MU _     => true | _ => false

    fun asArrow t	= case asType t of FUN tt    => tt | _ => raise Type
    fun asTuple t	= case asType t of TUPLE ts  => ts | _ => raise Type
    fun asProd t	= case asType t of PROD r    => r  | _ => raise Type
    fun asSum t		= case asType t of SUM r     => r  | _ => raise Type
    fun asVar t		= case asType t of VAR _     => t  | _ => raise Type
    fun asCon t		= case asType t of CON c     => c  | _ => raise Type
    fun asAll t		= case asType t of ALL at    => at | _ => raise Type
    fun asExist t	= case asType t of EXIST at  => at | _ => raise Type
    fun asLambda t	= case asType t of LAMBDA at => at | _ => raise Type
    fun asApply t	= case asType t of APPLY tt  => tt | _ => raise Type
    fun asMu t		= case asType t of MU t      => t  | _ => raise Type

    fun isAbbrev t	= case !(follow t) of ABBREV _  => true | _ => false
    fun asAbbrev t	= case !(follow t) of ABBREV tt => tt | _ => raise Type

    fun pathCon(_,_,p)	= p
    fun path t		= pathCon(asCon t)

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


    exception Unclosed

    fun checkClosedRow(RHO _)		= raise Unclosed
      | checkClosedRow _		= ()

    fun checkClosed'(HOLE _)		= raise Unclosed
      | checkClosed'(PROD r | SUM r)	= checkClosedRow r
      | checkClosed' _			= ()

    fun isClosed t =
	( app (fn t as ref t' => checkClosed' t'
	handle Unclosed =>
(*DEBUG*)
	( print(case !t of HOLE _ => "Hummm...\n" | _ => "Haehh?\n")
	; t := CON(STAR,CLOSED,Path.fromLab(Label.fromString "'_ouch"))
	; raise Unclosed
	)
	) t ; true )
(*	( app (fn ref t' => checkClosed' t') t ; true )*)
	handle Unclosed => false


  (* Instantiation *)

    (* Instantiate universally quantified types, skolemise
     * existentially qualified types. If there is any quantification,
     * then we have to copy the type.
     * Skolemisation does it the other way round (needed for checking rank 2
     * signature applications and existential types).
     *)

    (*ASSUME that no quantifiers appear under abbreviations
   	(this might break with higher-order polymorphism!) *)

    fun instance'(ref(ALL(a,t)))	= ( a := unknown'(kind a); instance' t )
      | instance'(ref(EXIST(a,t)))	= instance' t
      | instance' t			= t

    fun instance(t as ref(ALL _| EXIST _))	= instance'(clone t)
      | instance(ref(LINK t))			= instance t
      | instance t				= t

    fun skolem'(ref(ALL(a,t)))		= skolem' t
      | skolem'(ref(EXIST(a,t)))	= ( a := unknown' STAR ; skolem' t )
      | skolem' t			= t

    fun skolem(t as ref(ALL _| EXIST _))	= skolem'(clone t)
      | skolem(ref(LINK t))			= skolem t
      | skolem t				= t



  (* Operations on rows *)

    exception Row

    fun unknownRow()	= RHO(ref(!level), NIL)
    fun emptyRow()	= NIL

    fun extendRow(l,ts, NIL)      = FIELD(l,ts,NIL)
      | extendRow(l,ts, RHO(n,r)) = RHO(n, extendRow(l,ts,r))
      | extendRow(l1,ts1, r1 as FIELD(l2,ts2,r2)) =
	case Label.compare(l1,l2)
	  of EQUAL   => raise Row
	   | LESS    => FIELD(l1, ts1, r1)
	   | GREATER => FIELD(l2, ts2, extendRow(l1,ts1,r2))

    fun tupToRow ts =
	let
	    fun loop(n,  []  ) = NIL
	      | loop(n, t::ts) = FIELD(Label.fromInt n, [t], loop(n+1,ts))
	in
	    loop(1,ts)
	end

    fun openRow(r as RHO _)		= r
      | openRow r			= RHO(ref(!level), r)

    fun openRowType(ref(LINK t))	= openRowType t
      | openRowType(t as ref(PROD r))	= t := PROD(openRow r)
      | openRowType(t as ref(SUM r))	= t := SUM(openRow r)
      | openRowType _			= raise Row


    fun isEmptyRow(NIL | RHO _)		= true
      | isEmptyRow _			= false

    fun isUnknownRow(RHO _)		= true
      | isUnknownRow _			= false

    fun headRow(FIELD(l,ts,r))		= (l,ts)
      | headRow(RHO(_,r))		= headRow r
      | headRow(NIL)			= raise Row

    fun tailRow(FIELD(l,ts,r))		= r
      | tailRow(RHO(n,r))		= RHO(n, tailRow r)
      | tailRow(NIL)			= raise Row


  (* Closure *)

    fun close t =
    	let
	    fun close(a as ref(HOLE(k,n)), f) =
		if n > !level then
		    ( a := VAR(k,n) ; fn t => f(inAll(a,t)) )
		else f

	      | close(a as ref(VAR(k,n)), f) =
		(* UNFINISHED: rethink this *)
		(* We have to quantify over VARs as well.
		 * The reason is that there may be several types being
		 * closed that share parts of their graphs.
		 * Note that this means we have to take care to choose
		 * a unique upper level when we use VAR for skolem types.
		 *)
		if n > !level then
		    fn t => f(inAll(a,t))
		else f

	      | close(ref(ALL(a,t) | EXIST(a,t) | LAMBDA(a,t)), f) =
		( a := MARK(!a) ; f )	(* bit of a hack... *)

	      | close(t as ref(PROD r), f) = ( t := PROD(closeRow r) ; f )
	      | close(t as ref(SUM r), f)  = ( t := SUM(closeRow r) ; f )

	      | close(_, f) = f

	    and closeRow(r as RHO(n,r')) = if !n > !level then r' else r
	      | closeRow r               = r
	in
	    foldl close (fn t => t) t t
	end


  (* Lifting a type to the current level *)

    exception Lift of var

    fun lift t =
	let
	    fun lift(t as ref(t' as HOLE(k,n))) =
		    if n > !level then t := HOLE(k,!level) else ()
	      | lift(t as ref(VAR(k,n))) =
		    if n > !level then raise Lift t else ()
	      | lift(ref(PROD r | SUM r)) = liftRow r
	      | lift(ref(LAMBDA(a,_) | ALL(a,_) | EXIST(a,_))) =
(*ASSERT	    assert isVar a =>*)
if not(isVar a) then raise Assert.failure else
		    a := VAR(kindVar a, globalLevel)
	      | lift t = ()
	in
	    app lift t
	end

    and liftRow(RHO(n,r))	= if !n > !level then n := !level else ()
      | liftRow _		= ()


  (* Filling *)

    exception Fill

    fun fill(t1 as ref(HOLE(k,n)), t2)	= 
(*ASSERT		 		= assert k1 = kind' t2' =>*)
if k <> kind t2 then raise Assert.failure else
					  t1 := LINK t2
      | fill    _			= raise Crash.Crash "Type.fill"


  (* Unification *)

    exception Unify of typ * typ


    fun liftAndCheck(n,t1,t2) =
	let
	    fun lift(t as ref(t' as HOLE(k,n'))) =
		    t := MARK(if n' <= n then t' else HOLE(k,n))
	      | lift(ref(MARK _)) = ()
	      | lift(t as ref(t' as (PROD r | SUM r))) =
		    ( liftRow r ; t := MARK t' ; app1'(t', lift) )
	      | lift(t as ref t') = ( t := MARK t' ; app1'(t', lift) )

	    and liftRow(RHO(n',r)) = if !n' > n then n' := n else ()
	      | liftRow _          = ()

	    fun check(t as ref t') =
		if t1 = t then
		    ( unmark t2 ; raise Unify(t1,t2) )
		else case t'
		  of HOLE(k,n') => t := MARK(if n' <= n then t' else HOLE(k,n))
		   | MARK _     => ()
		   | MU _       => ( t := MARK t' ; app1'(t', lift) )
		   | _          => ( t := MARK t' ; app1'(t', check) )
	in
	    check t2 ; unmark t2
	end


    fun unify(t1,t2) =
	let
	    val trail = ref []

	    fun unify(t1,t2) =
		let
		    val t1 as ref t1' = follow t1
		    val t2 as ref t2' = follow t2

		    fun recur f x =
			( t1 := LINK t2
			; trail := (t1,t1') :: !trail
			; f x
			)

		    fun recurBinder(a1 as ref a1', a2, t1, t2) =
			( a1 := LINK a2
			; trail := (a1,a1') :: !trail
			; recur unify (t1,t2)
			)
		in
		    if t1 = t2 then () else
		    case (t1',t2')
		      of (HOLE(k1,n1), HOLE(k2,n2)) =>
(*ASSERT		 assert k1 = k2 =>*)
if k1 <> k2 then raise Assert.failure else
			 if n1 < n2 then t2 := LINK t1
				    else t1 := LINK t2

		       | (HOLE(k1,n), _) =>
(*ASSERT		 assert k1 = kind' t2' =>*)
if k1 <> kind' t2' then raise Assert.failure else
			 ( liftAndCheck(n,t1,t2) ; t1 := LINK t2 )

		       | (_, HOLE(k2,n)) =>
(*ASSERT		 assert kind' t1' = k2 =>*)
if kind' t1' <> k2 then raise Assert.failure else
			 ( liftAndCheck(n,t2,t1) ; t2 := LINK t1 )

		       | (MU(t11), MU(t21)) =>
			 recur unify (t11,t21)
		       (*
		       | (MU(t11), _) =>
			 ( t2 := MU(ref t2') ; unify(t1,t2) )

		       | (_, MU(t21)) =>
			 ( t1 := MU(ref t1') ; unify(t1,t2) )
		       *)
		       | (FUN(tt1), FUN(tt2)) =>
			 recur unifyPair (tt1,tt2)

		       | (TUPLE(ts1), TUPLE(ts2)) =>
			 (recur unifyList (ts1,ts2)
			  handle Domain => raise Unify(t1,t2))

		       | (TUPLE(ts), PROD(r)) =>
			 recur unifyRow (t1, t2, tupToRow ts, r, PROD)

		       | (PROD(r), TUPLE(ts)) =>
			 recur unifyRow (t1, t2, r, tupToRow ts, PROD)

		       | (PROD(r1), PROD(r2)) =>
			 recur unifyRow (t1, t2, r1, r2, PROD)

		       | (SUM(r1), SUM(r2)) =>
			 recur unifyRow (t1, t2, r1, r2, SUM)

		       | (CON(_,_,p1), CON(_,_,p2)) =>
			 if Path.equals(p1,p2) then t1 := LINK t2
					       else raise Unify(t1,t2)

		       | (APPLY(tt1), APPLY(tt2)) =>
			 (* Note that we do not allow general lambdas during
			  * unification, so application is considered to be
			  * in normal form. *)
			 recur unifyPair (tt1,tt2)

		       | (LAMBDA(a1,t11), LAMBDA(a2,t21)) =>
			 (* The only place lambdas might occur during
			  * unification is below mu. The type functions
			  * must be equal in that case. *)
			 recurBinder(a1, a2, t11, t21)

		       | (ALL(a1,t11), ALL(a2,t21)) =>
			 raise Crash.Crash "Type.unify: universal quantifier"

		       | (EXIST(a1,t11), EXIST(a2,t21)) =>
			 raise Crash.Crash "Type.unify: existential quantifier"

		       | (ABBREV(t11,t12), _) =>
			 unify (t12,t2)

		       | (_, ABBREV(t21,t22)) =>
			 unify (t1,t22)

		       | _ => raise Unify(t1,t2)
		end

	    and unifyPair((t11,t12), (t21,t22)) =
		( unify(t11,t21) ; unify(t12,t22) )

	    and unifyList(  [],      []   ) = ()
	      | unifyList(t1::ts1, t2::ts2) =
		( unify(t1,t2) ; unifyList(ts1,ts2) )
	      | unifyList _ = raise Domain

	    and unifyRow(t1, t2, r1, r2, PRODorSUM) =
		let
(*DEBUG
val timer = Timer.startRealTimer()
*)
		    fun loop(RHO(n1,r1), _,  RHO(n2,r2), _ ) =
			    RHO(ref(Int.min(!n1, !n2)), loop(r1,true, r2,true))
		      | loop(RHO(_,r1), _, r2, _) =
			    loop(r1, true, r2, false)
		      | loop(r1, _, RHO(_,r2), _) =
			    loop(r1, false, r2, true)
		      | loop(NIL, _, NIL, _) =
			    NIL
		      | loop(NIL, true, FIELD(l,ts,r2'), b2) =
			    FIELD(l,ts, loop(NIL, true, r2', b2))
		      | loop(FIELD(l,ts,r1'), b1, NIL, true) =
			    FIELD(l,ts, loop(r1', b1, NIL, true))
		      | loop(r1 as FIELD(l1,ts1,r1'), b1,
			     r2 as FIELD(l2,ts2,r2'), b2) =
			(case Label.compare(l1,l2)
			   of EQUAL   => ( unifyList(ts1,ts2)
					   handle Domain => raise Unify(t1,t2)
					 ; FIELD(l1,ts1, loop(r1',b1, r2',b2)) )
			    | LESS    => if not b2 then raise Unify(t1,t2) else
					 FIELD(l1,ts1, loop(r1',b1, r2,b2))
			    | GREATER => if not b1 then raise Unify(t1,t2) else
					 FIELD(l2,ts2, loop(r1,b1, r2',b2))
			)
		      | loop _ = raise Unify(t1,t2)
		in
		    t2 := PRODorSUM(loop(r1, false, r2, false))
(*DEBUG*)
(*before let val ms = LargeInt.toInt(Time.toMilliseconds(Timer.checkRealTimer timer)) in
print("Row unification took "^Int.toString ms^"ms\n")
before ignore(TextIO.inputLine(TextIO.stdIn))
end*)
		end
	in
	    unify(t1,t2)
	    handle Unify tt => ( List.app op:= (!trail) ; raise Unify tt )
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


  (* Matching *)

    fun matches(t1,t2) =
	let
	    val t1' = instance t1
	    val t2' = skolem t2
	in
	    ( unify(t1',t2') ; true ) handle Unify _ => false
	end


  (* Comparison *)

    (* To maximise sharing, links are kept after comparison iff the
     * types are equal. If the types are not equal, all updates are
     * undone. This could be optimized by keeping equal subtrees, but what for?
     *)

    fun equals(t1,t2) =
	let
	    val trail = ref []

	    fun equals(t1,t2) =
		let
		    val t1 as ref t1' = follow t1
		    val t2 as ref t2' = follow t2

		    fun recur p x =
			( t1 := LINK t2
			; trail := (t1,!t1) :: !trail
			; p x
			)

		    fun recurBinder(a1 as ref a1', a2, t1, t2) =
			( a1 := LINK a2
			; trail := (a1,a1') :: !trail
			; recur equals (t1,t2)
			)
		in
		    t1 = t2 orelse
		    case (t1',t2')
		      of (MU(t11), MU(t21)) =>
			 recur equals (t11,t21)
		       (*
		       | (MU(t11), _) =>
			 recur equals (t11,t2)

		       | (_, MU(t21)) =>
			 recur equals (t1,t21)
		       *)
		       | (FUN(tt1), FUN(tt2)) =>
			 recur equalsPair (tt1,tt2)

		       | (TUPLE(ts1), TUPLE(ts2)) =>
			 recur equalsList (ts1,ts2)

		       | ( (TUPLE(ts), PROD(r))
			 | (PROD(r),   TUPLE(ts)) ) =>
			 recur equalsRow (r, tupToRow ts)

		       | ( (PROD(r1), PROD(r2))
			 | (SUM(r1),  SUM(r2)) ) =>
			 recur equalsRow (r1,r2)

		       | (CON(_,_,p1), CON(_,_,p2)) =>
			 Path.equals(p1,p2)

		       | (APPLY(tt1), APPLY(tt2)) =>
			 recur equalsPair (tt1,tt2)

		       | ( (LAMBDA _, _) | (_, LAMBDA _) ) =>
			 ( reduceEta t1
			 ; reduceEta t2
			 ; case (!t1,!t2)
			     of (LAMBDA(a1,t11), LAMBDA(a2,t21)) =>
				recurBinder(a1, a2, t11, t21)
			      | ( (LAMBDA _, _) | (_, LAMBDA _) ) => false
			      | _ => equals(t1,t2)
			 )
		       | ( (ALL(a1,t11),   ALL(a2,t21))
			 | (EXIST(a1,t11), EXIST(a2,t21)) ) =>
			 recurBinder(a1, a2, t11, t21)

		       | (ABBREV(t11,t12), _) =>
			 equals(t12,t2)

		       | (_, ABBREV(t21,t22)) =>
			 equals(t1,t22)

		       | _ => false
		end

	    and equalsPair((t11,t12), (t21,t22)) =
		equals(t11,t21) andalso equals (t12,t22)

	    and equalsList(  [],      []   ) = true
	      | equalsList(t1::ts1, t2::ts2) =
		equals(t1,t2) andalso equalsList(ts1,ts2)
	      | equalsList _ = false

(*DEBUG
and equalsRow rr =
let
val timer = Timer.startRealTimer()
fun f() = ()
*)
	    and equalsRow(NIL,              NIL)              = true
	      | equalsRow(RHO(_,r1),        RHO(_,r2))        = equalsRow(r1,r2)
	      | equalsRow(FIELD(l1,ts1,r1), FIELD(l2,ts2,r2)) =
		l1 = l2 andalso equalsList(ts1,ts2) andalso equalsRow(r1,r2)
	      | equalsRow _ = false
(*in
equalsRow rr
before let val ms = LargeInt.toInt(Time.toMilliseconds(Timer.checkRealTimer timer)) in
print("Row equality took "^Int.toString ms^"ms\n")
before ignore(TextIO.inputLine(TextIO.stdIn))
end
end
*)
	in
	    equals(t1,t2) orelse ( List.app op:= (!trail) ; false )
	end


  (* Realisation *)

    fun realisePath(rea, t) =
	let
	    fun subst(t1 as ref(CON(k,s,p))) =
		( case PathMap.lookup(rea, p)
		    of SOME p' => t1 := CON(k,s,p')
		     | NONE    => ()
		)
	      | subst t1 = ()
	in
	    app subst t
	end

    (*
     * Realisations need not be idempotent (fully expanded). Would be nice
     * to have this property because it would make substitution more efficient,
     * but full expansion is difficult to achieve for the intersect function).
     *)

    fun realise(rea, t) =
	let
	    val apps = ref[]

	    fun subst(t1 as ref(CON(k,s,p))) =
		(case PathMap.lookup(rea, p)
		   of SOME t2 => t1 := LINK t2		(* expand *)
					(*UNFINISHED: do we have to clone t2? *)
		    | NONE    => ()
		)
	      | subst(t1 as ref(APPLY _)) = apps := t1::(!apps)
	      | subst t1 = ()
	in
	    app subst t ; List.app reduce (!apps)
	end


  (* Intersection *)

    exception Intersect

    fun intersect(t1,t2) = ()
    (* UNFINISHED *)

  end


structure Type : (*DEBUG :>*) TYPE = TypePrivate
