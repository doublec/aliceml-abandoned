(* Interfaces contain state. This means that they must be cloned
   at each occurance. *)

structure InfPrivate =
  struct

  (* Types *)

    type lab	= Lab.t
    type name	= Name.t
    type stamp	= Stamp.t
    type path	= Path.t
    type typ	= Type.t
    type tkind	= Type.kind

    datatype val_sort = VALUE | CONSTRUCTOR		(* [w] *)
    datatype typ_sort = datatype Type.sort		(* [w] *)

    type id	= path * lab * int			(* [x] *)
    type 'a def	= 'a option				(* [d] *)


    (* A map for signatures *)

    datatype dom = VAL' | TYP' | MOD' | INF'

    structure Map = MakeHashImpMap(struct type t = dom * lab
					  fun hash(_,l) = Lab.hash l end)


    datatype inf' =
	  ANY					(* top *)
	| CON of con				(* interface constructor *)
	| SIG of sign				(* signature *)
	| ARR of path * inf * inf		(* arrow (functor) *)
	| LAM of path * inf * inf		(* abstraction (dep. function)*)
	| APP of inf * path * inf		(* application *)
	| LINK of inf

    and item' =
	  VAL of id *  typ  * val_sort * path def	(* value *)
	| TYP of id * tkind * typ_sort * typ def	(* type *)
	| MOD of id *  inf  * path def			(* module *)
	| INF of id *  kind * inf def			(* interface *)

    and kind' =						(* [kappa,k] *)
	  GROUND					(* ordinary interface *)
	| DEP of path * inf * kind			(* dependent *)

    withtype inf  = inf' ref				(* [jota,j] *)
    and      kind = kind' ref				(* [kappa,k] *)
    and      con  = kind' ref * path			(* [chi,c] *)
    and      item = item' ref				(* [item] *)
    and      sign = item' ref list ref * item' ref list Map.t	(* [sigma,s] *)

    type t = inf


  (* Realisations *)

    type rea	 = path PathMap.t

    type val_rea = path PathMap.t
    type typ_rea = typ  PathMap.t
    type mod_rea = sign PathMap.t
    type inf_rea = inf  PathMap.t
    type rea'	 = val_rea * typ_rea * mod_rea * inf_rea


  (* Simple accessors *)

    fun idPath(p,l,n)		= p
    fun idLab(p,l,n)		= l
    fun idIndex(p,l,n)		= n

    fun itemId(ref item')	= itemId' item'
    and itemId'(VAL(id,_,_,_))	= id
      | itemId'(TYP(id,_,_,_))	= id
      | itemId'(MOD(id,_,_))	= id
      | itemId'(INF(id,_,_))	= id

    fun itemPath  item		= idPath(itemId item)
    fun itemLab   item		= idLab(itemId item)
    fun itemIndex item		= idIndex(itemId item)

    fun itemDom(ref item')	= itemDom' item'
    and itemDom'(VAL _)		= VAL'
      | itemDom'(TYP _)		= TYP'
      | itemDom'(MOD _)		= MOD'
      | itemDom'(INF _)		= INF'


  (* Follow a path of links (performing path compression on the fly) *)

    fun follow(ref(LINK j))	= follow j
      | follow j		= j

(*DEBUG
    fun follow'(ref(LINK j))	= follow' j
      | follow' j		= j

    fun follow(j as ref(LINK k))= let val l = follow' k in j := LINK l ; l end
      | follow j		= j
*)


  (* Signature construction *)

    fun empty() = (ref [], Map.new())

    fun newItem(s, l)		= Path.fromLab l
    val newVal			= newItem
    val newTyp			= newItem
    val newMod			= newItem
    val newInf			= newItem

    fun hideId (p,l,n)		= (p,l,n+1)
    fun hide item		= item := hide'(!item)
    and hide'(VAL(id,t,w,d))	= VAL(hideId id, t, w, d)
      | hide'(TYP(id,k,w,d))	= TYP(hideId id, k, w, d)
      | hide'(MOD(id,j,d))	= MOD(hideId id, j, d)
      | hide'(INF(id,k,d))	= INF(hideId id, k, d)

    fun extend((itemsr,map), dom, p, makeItem') =
	let
	    val l    = Path.toLab p
	    val item = ref(makeItem'(p,l,0))

	    val _ = itemsr := item :: !itemsr
	    val _ = Map.insertWith (fn(items,_) =>
					( List.app hide items ; item::items )
				   ) (map, (dom,l), [item])
	in
	    p
	end

    fun extendVal(s,p,t,w,d)	= extend(s, VAL', p, fn x => VAL(x,t,w,d))
    fun extendTyp(s,p,k,w,d)	= extend(s, TYP', p, fn x => TYP(x,k,w,d))
    fun extendMod(s,p,j,d)	= extend(s, MOD', p, fn x => MOD(x,j,d))
    fun extendInf(s,p,k,d)	= extend(s, INF', p, fn x => INF(x,k,d))


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
    and realisePath(rea, p)	= Path.realise PathMap.lookup (rea, p)

    and realiseSig(rea, (ref items, _)) =
	    List.app (fn item => realiseItem(rea, item)) items

    and realiseItem(rea, item as ref(VAL(x, t, w, d))) =
	( Type.realise(rea, t) ; item := VAL(x, t, w, realisePathDef(rea, d)))
      | realiseItem(rea, ref(TYP(x, k, w, d))) =
	  realiseTypDef(rea, d)
      | realiseItem(rea, item as ref(MOD(x, j, d))) =
	( realise(rea, j) ; item := MOD(x, j, realisePathDef(rea, d)) )
      | realiseItem(rea, ref(INF(x, k, d))) =
	( realiseKind(rea, k) ; realiseInfDef(rea, d) )

    and realisePathDef(rea,      NONE  ) = NONE
      | realisePathDef(rea, d as SOME p) = case PathMap.lookup(rea, p)
					     of NONE => d
					      | d'   => d'
    and realiseTypDef(rea, NONE  ) = ()
      | realiseTypDef(rea, SOME t) = Type.realise(rea, t)

    and realiseInfDef(rea, NONE  ) = ()
      | realiseInfDef(rea, SOME j) = realise(rea, j)



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
    and clonePath(rea, p)	= Path.cloneFree PathMap.lookup (rea, p)

    and cloneKind (rea, ref k')	= ref(cloneKind'(rea, k'))
    and cloneKind'(rea, GROUND)	= GROUND
      | cloneKind'(rea, DEP(p,j,k)) =
	    DEP(clonePathBinder(rea, p), cloneInf(rea, j), cloneKind(rea, k))

    and clonePathBinder(rea, p) =
	let
	    val p' = Path.cloneBinder PathMap.lookup (rea, p)
	in
	    if p' <> p then PathMap.insert(rea, p, p') else () ;
	    p'
	end

    and cloneSig(rea, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(doml, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, doml, [item])
		)

	    fun cloneItem(ref(VAL((p,l,n), t, w, d))) =
		let
		    val p'   = clonePathBinder(rea, p)
		    val t'   = cloneTyp(rea, t)
		    val d'   = clonePathDef(rea, d)
		    val item = ref(VAL((p',l,n), t', w, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | cloneItem(ref(TYP((p,l,n), k, w, d))) =
		let
		    val p'   = clonePathBinder(rea, p)
		    val d'   = cloneTypDef(rea, d)
		    val item = ref(TYP((p',l,n), k, w, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | cloneItem(ref(MOD((p,l,n), j, d))) =
		let
		    val p'   = clonePathBinder(rea, p)
		    val j'   = cloneInf(rea, j)
		    val d'   = clonePathDef(rea, d)
		    val item = ref(MOD((p',l,n), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | cloneItem(ref(INF((p,l,n), k, d))) =
		let
		    val p'   = clonePathBinder(rea, p)
		    val k'   = cloneKind(rea, k)
		    val d'   = cloneInfDef(rea, d)
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end
	in
	    Misc.List_appr cloneItem items ;
	    s
	end

    and clonePathDef(rea,      NONE  )	= NONE
      | clonePathDef(rea, d as SOME p)	= case PathMap.lookup(rea, p)
					    of NONE => d
					     | d'   => d'
    and cloneTypDef(rea, NONE  )	= NONE
      | cloneTypDef(rea, SOME t)	= SOME(cloneTyp(rea, t))

    and cloneInfDef(rea, NONE  )	= NONE
      | cloneInfDef(rea, SOME j)	= SOME(cloneInf(rea, j))

    and cloneTyp(rea, t) =
	let
	    val t' = Type.clone t
	in
	    Type.realise(rea, t') ;
	    t'
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

    fun strengthen(p, ref(SIG s)) = strengthenSig(p, s)
      | strengthen(p, _)          = ()

    and strengthenSig(p, (ref items, _)) =
	    List.app (fn item => strengthenItem(p, item)) items

    and strengthenItem(p, item as ref(VAL(x, t, w, d))) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenPathDef(idPath x, d)
	in
	    item := VAL(x, t, w, d')
	end

      | strengthenItem(p, item as ref(TYP(x, k, w, d))) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenTypDef(idPath x, k, d)
	in
	    item := TYP(x, k, w, d')
	end

      | strengthenItem(p, item as ref(MOD(x, j, d))) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenPathDef(idPath x, d)
	in
	    item := MOD(x, j, d')
	end

      | strengthenItem(p, item as ref(INF(x, k, d))) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenInfDef(idPath x, k, d)
	in
	    item := INF(x, k, d')
	end

    and strengthenId(p, pln)		= Path.substituteDot(p, pln)

    and strengthenPathDef(p, NONE)	= SOME p
      | strengthenPathDef(p, d)		= d

    and strengthenTypDef(p, k, NONE)	= SOME(Type.inCon(k, Type.CLOSED, p))
      | strengthenTypDef(p, k, d)	= d

    and strengthenInfDef(p, k, NONE)	= SOME(inCon(k, p))
      | strengthenInfDef(p, k, d)	= d


  (* Kinds *)

    exception Kind

    fun inGround ()		= ref GROUND
    fun inDependent pjk		= ref(DEP pjk)

    fun isGround(ref GROUND)	= true
      | isGround _		= false

    fun isDependent k		= not(isGround k)
    fun asDependent(ref(DEP x))	= x
      | asDependent _		= raise Kind

    fun kind(ref j')		= kind' j'
    and kind'( ANY
	     | SIG _
	     | ARR _ )		= inGround()
      | kind'(CON(k,p))		= k
      | kind'(LAM(p,j1,j2))	= inDependent(p, j1, kind j2)
      | kind'(APP(j1,p,j2))	= (*UNFINISHED*) inGround()
      | kind'(LINK j)		= kind j


  (* Signature lookup *)

    fun selectVal(VAL(x, t, w, d))	= t
      | selectVal _			= raise Crash.Crash "Inf.selectVal"

    fun selectTyp(TYP(x, k, w, SOME t))	= t
      | selectTyp(TYP(x, k, w, NONE))	= Type.inCon(k, w, idPath x)
      | selectTyp _			= raise Crash.Crash "Inf.selectTyp"

    fun selectMod(MOD(x, j, d))		= j
      | selectMod _			= raise Crash.Crash "Inf.selectMod"

    fun selectInf(INF(x, k, SOME j))	= j
      | selectInf(INF(x, k, NONE))	= inCon(k, idPath x)
      | selectInf _			= raise Crash.Crash "Inf.selectInf"


    fun lookup dom ((_,m), l) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) => !item
	   | _                 => raise Crash.Crash "Inf.lookup"

    fun lookup' dom ((_,m), l, n) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) =>
		!(Option.valOf(List.find (fn item => itemIndex item = n) items))
	   | _ => raise Crash.Crash "Inf.lookup'"

    fun lookupVal args	= (selectVal o lookup VAL') args
    fun lookupTyp args	= (selectTyp o lookup TYP') args
    fun lookupMod args	= (selectMod o lookup MOD') args
    fun lookupInf args	= (selectInf o lookup INF') args

    fun lookupVal' args	= (selectVal o lookup' VAL') args
    fun lookupTyp' args	= (selectTyp o lookup' TYP') args
    fun lookupMod' args	= (selectMod o lookup' MOD') args
    fun lookupInf' args	= (selectInf o lookup' INF') args


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
    datatype mismatch =
	  MissingVal  of lab
	| MissingTyp  of lab
	| MissingMod  of lab
	| MissingInf  of lab
	| ManifestVal of lab
	| ManifestTyp of lab
	| ManifestMod of lab
	| ManifestInf of lab

    exception Mismatch of mismatch

    fun matchDef (equals, err) (l ,_,       NONE   ) = ()
      | matchDef (equals, err) (l, NONE,    SOME _ ) = raise Mismatch(err l)
      | matchDef (equals, err) (l, SOME x1, SOME x2) =
	    if equals(x1,x2) then () else raise Mismatch(err l)

    val matchValDef = matchDef(Path.equals, ManifestVal)
    val matchTypDef = matchDef((*Type.equals*)fn(_:typ*typ)=>true, ManifestTyp)
    val matchModDef = matchDef(Path.equals, ManifestMod)
    fun matchInfDef x = matchDef(equals, ManifestInf) x

    and matchSig((ref items1, m1), (ref items2, m2)) =
	let
	    fun pair(     [],      pairs) = List.rev pairs
	      | pair(item2::items, pairs) =
		let
		    val (p2,l,n) = itemId item2
		    val  item1   = List.hd(Map.lookupExistent
						(m1, (itemDom item2,l)))
		    val  p1      = itemPath item1
		in
		    Path.substitute(p2, p1) ;
		    pair(items, (item1,item2)::pairs)
		end
		handle Map.Lookup (VAL',l) => raise Mismatch(MissingVal l)
		     | Map.Lookup (TYP',l) => raise Mismatch(MissingTyp l)
		     | Map.Lookup (MOD',l) => raise Mismatch(MissingMod l)
		     | Map.Lookup (INF',l) => raise Mismatch(MissingInf l)
	in
	    List.app matchItem (pair(items2, []))
	end

    and matchItem(ref item1', ref item2') = matchItem'(item1', item2')

    and matchItem'(VAL(x1,t1,s1,d1), VAL(x2,t2,s2,d2)) =
	    ( matchTyp(t1,t2) ; matchValDef(idLab x2, d1, d2) )
      | matchItem'(TYP(x1,k1,s1,d1), TYP(x2,k2,s2,d2)) =
	    ( matchTKind(k1,k2) ; matchTypDef(idLab x2, d1, d2) )
      | matchItem'(MOD(x1,j1,d1), MOD(x2,j2,d2)) =
	    ( matchInf(j1,j2) ; matchModDef(idLab x2, d1, d2) )
      | matchItem'(INF(x1,k1,d1), INF(x2,k2,d2)) =
	    ( matchKind(k1,k2) ; matchInfDef(idLab x2, d1, d2) )
      | matchItem' _ = raise Crash.crash "Inf.matchItem"

    and matchTyp(t1,t2)  = (*UNFINISHED*) ()
    and matchTKind(k1,k2) = (*UNFINISHED*) ()
    and matchInf(j1,j2)  = (*UNFINISHED*) ()
    and matchKind(k1,k2) = (*UNFINISHED*) ()

  (* Comparison *)

    and equals(j1,j2) = (*UNFINISHED*) true

  end


structure Inf :> INF = InfPrivate
