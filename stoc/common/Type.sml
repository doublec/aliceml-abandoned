structure Type :> TYPE =
  struct

    datatype con_sort = OPEN | CLOSED

    datatype kind = STAR | ARROW of kind * kind		(* [kappa,k] *)

    type lab  = Lab.t					(* [lab,l] *)
    type path = Path.t					(* [pi,p] *)
    type con  = kind * con_sort * path			(* [chi,c] *)

    datatype typ' =					(* [tau',t'] *)
	  HOLE of int		(* variable for inference (has kind STAR) *)
	| LINK of typ		(* forward (needed for unification) *)
	| MARK of typ'		(* for traversal *)
	| ARR  of typ * typ	(* arrow type *)
	| TUP  of typ list	(* tuple *)
	| ROW  of row		(* record *)
	| SUM  of row		(* sum type (datatype) *)
	| VAR  of kind		(* bound variable or skolem types *)
	| CON  of con		(* constructor (of arbitrary kind) *)
	| ALL  of alpha * typ	(* universal quantification *)
	| EX   of alpha * typ	(* existential quantification *)
	| LAM  of alpha * typ	(* abstraction (type function) *)
	| APP  of typ * typ	(* application *)
	| REC  of typ		(* recursive type barrier *)

    and row = NIL | RHO | FLD of lab * typ * row	(* [rho,r] *)

    withtype typ = typ' ref				(* [tau,t] *)
    and    alpha = typ' ref				(* [alpha,a] *)

    type t = typ

    type rea = typ PathMap.t

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

    fun follow(t as ref(LINK u))= let val v = follow' u in t := LINK v ; v end
      | follow t		= t


    (* Infer the kind of a type *)

    fun rangeKind(ARROW(k1,k2))	= k2
      | rangeKind  _		= Crash.crash "Type.ranKind: kind mismatch"


    fun kind(ref t')		= kind' t'

    and kind'(LINK t | REC t)	= kind t
      | kind'(VAR k)		= k
      | kind'(CON(k,_,_))	= k
      | kind'(LAM(a,t))		= ARROW(kind a, kind t)
      | kind'(APP(t1,t2))	= rangeKind(kind t2)
      | kind' _			= STAR

    val kindVar = kind


    (* Reduction to some form of head normal form *)

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


    (* Creation and injections *)

    fun unknown'()	= HOLE(!level)
    fun unknown()	= ref(unknown'())

    fun inArrow tt	= ref(ARR tt)
    fun inTuple ts	= ref(TUP ts)
    fun inRow r		= ref(ROW r)
    fun inSum r		= ref(SUM r)
    fun inVar a		= a
    fun inCon c		= ref(CON c)
    fun inAll at	= ref(ALL at)
    fun inExist at	= ref(EX at)
    fun inLambda at	= let val t = ref(LAM at) in reduce1 t ; t end
    fun inApp tt	= let val t = ref(APP tt) in reduce1 t ; t end
    fun inRec t		= ref(REC t)

    fun var k		= ref(VAR k)


    (* Projections and extractions *)

    exception Type

    fun asType(ref(LINK t | REC t))	= asType t
      | asType(t as ref(APP _ | LAM _))	= ( reduce1 t ; asType t )
      | asType(ref t')			= t'

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

    fun pathCon(_,_,p)		= p
    fun path t			= pathCon(asCon t)


    (* Cloning under a type realisation *)

    fun realise phi t =
	let
	    (* We want to be able to handle recursive types, so we have to
	     * implement graph copying here.
	     *)

	    val trail = ref []

	    fun dup(t1 as ref t1') =
		let
		    val _   = trail := (t1,t1') :: !trail
		    val t2  = ref(MARK t1')
		    val _   = t1 := LINK t2
		    val t2' = clone' t1'
		    val _   = t2 := t2'
		in
		    t2
		end

	    and clone t =
		let val t1 = follow t in
		    case !t1 of MARK _ => t
			      | t1'    => dup t1
		end

	    and clone'(ARR(t1,t2))	= ARR(clone t1, clone t2)
	      | clone'(TUP ts)		= TUP(List.map clone ts)
	      | clone'(ROW r)		= ROW(cloneRow r)
	      | clone'(SUM r)		= SUM(cloneRow r)
	      | clone'(VAR k)		= VAR k
	      | clone'(CON c)		= CON c (*UNFINISHED: do rea+red*)
	      | clone'(ALL(a,t))	= ALL(dup a, clone t)
	      | clone'(EX(a,t))		= EX(dup a, clone t)
	      | clone'(LAM(a,t))	= LAM(dup a, clone t)
	      | clone'(APP(t1,t2))	= APP(clone t1, clone t2)
	      | clone'(REC t)		= REC(clone t)
	      | clone'(HOLE _ | LINK _ | MARK _) = Crash.crash "Type.clone"

	    and cloneRow(FLD(l,t,r))	= FLD(l, clone t, cloneRow r)
	      | cloneRow r		= r
	in
	    clone t before List.app op:= (!trail)
	end


    fun clone t = realise (PathMap.new()) t


    (* Instantiation: instantiate universally quantified types, skolemise
     * existentially qualified types. If there is any quantification,
     * then we have to copy the type.
     * Skolemisation does it the other way round (needed for checking rank 2
     * signature applications).
     *)

    fun instance'(ref(ALL(a,t)))	= ( a := unknown'() ; instance' t )
      | instance'(ref(EX(a,t)))		= instance' t
      | instance' t			= t

    fun instance(t as ref(ALL _| EX _))	= instance'(clone t)
      | instance t			= t


    fun skolem'(ref(ALL(a,t)))		= skolem' t
      | skolem'(ref(EX(a,t)))		= ( a := unknown'() ; skolem' t )
      | skolem' t			= t

    fun skolem(t as ref(ALL _| EX _))	= skolem'(clone t)
      | skolem t			= t



    (* Traversal helpers *)

    fun fold1'(( HOLE _
	       | VAR _
	       | CON _ ), f, a)		= a
      | fold1'(( LINK t1
	       | REC t1
	       | ALL(_,t1)
	       | EX (_,t1)
	       | LAM(_,t1)), f, a)	= f(t1,a)
      | fold1'(( ARR(t1,t2)
	       | APP(t1,t2)), f, a)	= f(t2, f(t1,a))
      | fold1'(( TUP ts ), f, a)	= List.foldl f a ts
      | fold1'(( ROW r
	       | SUM r ), f, a)		= foldRow(r,f,a)
      | fold1'(( MARK _ ), f, a)	= Crash.crash "Type.fold: bypassed MARK"

    and foldRow(FLD(_,t,r), f, a)	= foldRow(r, f, f(t,a))
      | foldRow(_, f, a)		= a


    fun app1'(t,f)			= fold1'(t, f o #1, ())


    fun unmark(t as ref(MARK t')) 	= t := t'
      | unmark(ref t')            	= app1'(t', unmark)



    (* Folding a type by pre-order traversal. *)

    fun foldl f a t =
	let
	    fun fold(ref(MARK _), a) = a
	      | fold(t as ref t', a) =
		let
		    val a' = f(t,a)
		    val _  = t := MARK t'
		in
		    fold1'(t',fold,a')
		end
	in
	    fold(t,a) before unmark t
	end

    fun app f = foldl (f o #1) ()


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


    (* Operations on rows *)

    exception Row

    fun unknownRow()			= RHO
    fun emptyRow()			= NIL

    fun extendRow(l,t, r as (RHO|NIL))	= FLD(l,t,r)
      | extendRow(l1,t1, r1 as FLD(l2,t2,r2)) =
	case Lab.compare(l1,l2)
	  of EQUAL   => raise Row
	   | LESS    => FLD(l1, t1, r1)
	   | GREATER => FLD(l2, t2, extendRow(l1,t2,r2))


    (* Closure *)

    fun close t =
	(*UNFINISHED*) ()


    (* Unification *)

    exception Unify of typ * typ


    fun occurs(t1,t2) =
	let
	    exception Occurs

	    fun occurs(ref(REC _))    = ()
	      | occurs(t2 as ref t2') =
		if t1 = t2 then
		    raise Occurs
		else
		    ( t2 := MARK t2' ; app1'(t2', occurs) )
	in
	    (( occurs t2 ; false ) handle Occurs => true) before unmark t1
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

	    fun unifyRow(r1,r2,ROWorSUM) =
		let
		    fun loop(NIL, false, NIL, false) = NIL
		      | loop(NIL, false, RHO, _    ) = NIL
		      | loop(RHO, _,     NIL, false) = NIL
		      | loop(RHO, _,     RHO, _    ) = RHO
		      | loop(RHO, _, FLD(l,t,r), b2) =
			    FLD(l,t, loop(RHO, true, r, b2))
		      | loop(FLD(l,t,r), b1, RHO, _) =
			    FLD(l,t, loop(r, b1, RHO, true))
		      | loop(r1 as FLD(l1,t1,r1'),b1, r2 as FLD(l2,t2,r2'),b2) =
			(case Lab.compare(l1,l2)
			   of EQUAL   => ( unify(t1,t2)
					 ; FLD(l1,t1, loop(r1',b1, r2',b2)) )
			    | LESS    => FLD(l1,t1, loop(r1',b1, r2,true))
			    | GREATER => FLD(l2,t2, loop(r1,true, r2',b2))
			)
		      | loop _ = raise Unify(t1,t2)
		in
		    t2 := ROWorSUM(loop(r1,false, r2,false))
		end
	in
	    if t1 = t2 then () else
	    case (t1',t2')
	      of (HOLE(n1), HOLE(n2)) =>
		 if n1 < n2 then t2 := LINK t1
			    else t1 := LINK t2

	       | (HOLE(_), _) =>
		 if occurs(t1,t2) then raise Unify(t1,t2)
				  else t1 := LINK t2

	       | (_, HOLE(_)) =>
		 if occurs(t2,t1) then raise Unify(t1,t2)
				  else t2 := LINK t1

	       | (ARR(tt1), ARR(tt2)) =>
		 recurse unifyPair (tt1,tt2)

	       | (TUP(ts1), TUP(ts2)) =>
		 recurse (ListPair.app unify) (ts1,ts2)

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

  end
