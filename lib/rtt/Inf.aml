structure InfPrivate =
  struct

  (* Types *)

    type path = Path.t

    datatype inf' =
	  ANY					(* top *)
	| CON of con				(* interface constructor *)
	| SIG of sign				(* signature *)
	| ARR of path * inf * inf		(* arrow (functor) *)
	| LAM of path * inf * inf		(* abstraction (dep. function)*)
	| APP of inf * path * inf		(* application *)
	| LINK of inf

    and kind' =					(* [kappa,k] *)
	  GROUND				(* ordinary interface *)
	| DEP of path * inf * kind		(* dependent *)

    withtype inf  = inf' ref			(* [jota,j] *)
    and      kind = kind' ref			(* [kappa,k] *)
    and      con  = kind' ref * path		(* [chi,c] *)
    and      sign = (inf, kind' ref) Sign.t	(* [sigma,s] *)

    type t = inf

    type rea  = Sign.rea
    type rea' = (inf,kind) Sign.rea'



  (* Follow a path of links (performing path compression on the fly) *)

    fun follow(ref(LINK j))	= follow j
      | follow j		= j

(*DEBUG
    fun follow'(ref(LINK j))	= follow' j
      | follow' j		= j

    fun follow(j as ref(LINK k))= let val l = follow' k in j := LINK l ; l end
      | follow j		= j
*)


  (* Realisation *)

    fun realise (rea, ref j')	= realise'(rea, j')
    and realise'(rea, LINK j)	= realise(rea, j)
      | realise'(rea, ANY)	= ()
      | realise'(rea, CON c)	= realiseCon(rea, c)
      | realise'(rea, SIG s)	= realiseSig(rea, s)
      | realise'(rea, ( ARR(_,j1,j2) | LAM(_,j1,j2) )) =
	    ( realise(rea, j1) ; realise(rea, j2) )
      | realise'(rea, APP(j1,p,j2)) =
	    ( realise(rea, j1) ; realisePath(rea, p) ; realise(rea, j2) )

    and realiseKind (rea, ref k')     = realiseKind'(rea, k')
    and realiseKind'(rea, GROUND)     = ()
      | realiseKind'(rea, DEP(_,j,k)) = ( realise(rea, j) ; realiseKind(rea, k))

    and realiseCon(rea, (k,p))	= ( realiseKind(rea, k) ; realisePath(rea, p) )
    and realiseSig(rea, s)	= Sign.realise (realise, realiseKind) (rea, s)
    and realisePath(rea, p)	= Path.realise PathMap.lookup (rea, p)


  (* Cloning *)

    fun cloneInf(rea, ref j')	= ref(cloneInf'(rea, j'))
    and cloneInf'(rea, LINK j)	= cloneInf'(rea, !j)
      | cloneInf'(rea, ANY)	= ANY
      | cloneInf'(rea, CON c)	= CON(cloneCon(rea, c))
      | cloneInf'(rea, SIG s)	= SIG(cloneSig(rea, s))
      | cloneInf'(rea, ARR(p,j1,j2)) =
	    ARR(clonePathBinder(rea, p), cloneInf(rea, j1), cloneInf(rea, j2))
      | cloneInf'(rea, LAM(p,j1,j2)) =
	    LAM(clonePathBinder(rea, p), cloneInf(rea, j1), cloneInf(rea, j2) )
      | cloneInf'(rea, APP(j1,p,j2)) =
	    APP(cloneInf(rea, j1), clonePath(rea, p), cloneInf(rea, j2) )

    and cloneCon(rea, (k,p))	= (cloneKind(rea, k), clonePath(rea, p))
    and cloneSig(rea, s)	= Sign.clone (cloneInf, cloneKind) (rea, s)
    and clonePath(rea, p)	= Path.cloneFree PathMap.lookup (rea, p)

    and cloneKind (rea, ref k')	= ref(cloneKind'(rea, k'))
    and cloneKind'(rea, GROUND)	= GROUND
      | cloneKind'(rea, DEP(p,j,k)) =
	    DEP(clonePathBinder(rea, p), cloneInf(rea, j), cloneKind(rea, k))

    and clonePathBinder(rea, p) =
	let val p' = Path.cloneBinder PathMap.lookup (rea, p) in
	    PathMap.insert(rea, p, p') ; p'
	end

    fun clone j = cloneInf(PathMap.new(), j)


  (* Reduction to head normal form *)

    (*UNFINISHED: avoid multiple cloning of curried lambdas somehow *)

    fun reduce(j as ref(APP(j1,p,j2)))	= reduceApp(j, j1, p, j2)
      | reduce _			= ()

    and reduceApp(j, j1 as ref(LAM _), p, j2) =
	( case !(clone j1)
	    of LAM(p1, j11, j12) =>
		(*UNFINISHED: do realisation *)
		(*Path.replace(p1, p)*)
		( j := LINK j12
		; reduce j
		)
	    | _ => Crash.crash "Type.reduceApp"
	)
      | reduceApp(j, j1, p, j2) = ()


  (* Creation and injections *)

    fun inAny()		= ref ANY
    fun inCon c		= ref(CON c)
    fun inSig s		= ref(SIG s)
    fun inArrow pjj	= ref(ARR pjj)
    fun inLambda pjj	= ref(LAM pjj)
    fun inApp jpj	= let val j = ref(APP jpj) in reduce j ; j end


  (* Projections and extractions *)

    exception Interface

    fun asInf j		= !(follow j)

    fun isAny j		= case asInf j of ANY   => true | _ => false
    fun isCon j		= case asInf j of CON _ => true | _ => false
    fun isSig j		= case asInf j of SIG _ => true | _ => false
    fun isArrow j	= case asInf j of ARR _ => true | _ => false
    fun isLambda j	= case asInf j of LAM _ => true | _ => false
    fun isApp j		= case asInf j of APP _ => true | _ => false

    fun asCon j		= case asInf j of CON c   => c   | _ => raise Interface
    fun asSig j		= case asInf j of SIG s   => s   | _ => raise Interface
    fun asArrow j	= case asInf j of ARR xjj => xjj | _ => raise Interface
    fun asLambda j	= case asInf j of LAM xjj => xjj | _ => raise Interface
    fun asApp j		= case asInf j of APP jpj => jpj | _ => raise Interface

    fun pathCon(_,p)	= p
    fun path j		= pathCon(asCon j)


  (* Strengthening *)

    fun strengthen(p, ref(SIG s)) = Sign.strengthen inCon (p, s)
      | strengthen(p, _)          = ()


  (* Matching *)
(*
    exception Mismatch of inf * inf

    fun match(j1,j2) =
	let
	    fun matchInf(_, ANY) = ()
	      | matchInf(j1 as CON(s1,p1), j2 as CON(s2,p2)) =
		    if p1 = p2 then () else raise Mismatch(j1,j2)

	      | matchInf(j1 as SIG(is1,m1), j2 as SIG(is2,m2)) =
		let
		    val iis = pairItems(m1,is2,,[])
		in
		    ListPair.app matchItem iis
		end

	      | matchInf(

	    and matchItem

	    and pairItems(m1, i2::is2, subst, missing) =
		(case i2
		   of VAL_ITEM(l,t,d) =>
			LabMap.lookupExistent(t1, l)
		) handle LabMap.Lookup
		(* via stamp->item map*)
	in
	    matchInf(j1,j2)
	end
*)
  end


structure Inf :> INF = InfPrivate
