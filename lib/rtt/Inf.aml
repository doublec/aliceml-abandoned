(* Interfaces contain state. This means that they must be instantiated
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

    datatype space = VAL' | TYP' | MOD' | INF'

    structure Map = MakeHashImpMap(struct type t = space * lab
					  fun hash(_,l) = Lab.hash l end)


    datatype inf' =
	  TOP					(* top *)
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

    type val_rea = path PathMap.t
    type typ_rea = typ  PathMap.t
    type mod_rea = path PathMap.t
    type inf_rea = inf  PathMap.t

    type rea	 = { val_rea : val_rea
		   , typ_rea : typ_rea
		   , mod_rea : mod_rea
		   , inf_rea : inf_rea
		   }

    fun emptyRea() = { val_rea = PathMap.new()
		     , typ_rea = PathMap.new()
		     , mod_rea = PathMap.new()
		     , inf_rea = PathMap.new()
		     } : rea


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

    fun itemSpace(ref item')	= itemSpace' item'
    and itemSpace'(VAL _)	= VAL'
      | itemSpace'(TYP _)	= TYP'
      | itemSpace'(MOD _)	= MOD'
      | itemSpace'(INF _)	= INF'


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

    fun empty()			= (ref [], Map.new())

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

    fun extend((itemsr,map), space, p, makeItem') =
	let
	    val l    = Path.toLab p
	    val item = ref(makeItem'(p,l,0))
	in
	    itemsr := item :: !itemsr ;
	    Map.insertWith (fn(items,_) => (List.app hide items ; item::items))
			   (map, (space,l), [item])
	end

    fun extendVal(s,p,t,w,d)	= extend(s, VAL', p, fn x => VAL(x,t,w,d))
    fun extendTyp(s,p,k,w,d)	= extend(s, TYP', p, fn x => TYP(x,k,w,d))
    fun extendMod(s,p,j,d)	= extend(s, MOD', p, fn x => MOD(x,j,d))
    fun extendInf(s,p,k,d)	= extend(s, INF', p, fn x => INF(x,k,d))


  (* Signature lookup *)

    fun selectVal'(VAL(x, t, w, d))	= t
      | selectVal' _			= raise Crash.Crash "Inf.selectVal'"

    fun selectVal(VAL(x, t, w, SOME p))	= p
      | selectVal(VAL(x, t, w, NONE))	= idPath x
      | selectVal _			= raise Crash.Crash "Inf.selectVal"

    fun selectTyp(TYP(x, k, w, SOME t))	= t
      | selectTyp(TYP(x, k, w, NONE))	= Type.inCon(k, w, idPath x)
      | selectTyp _			= raise Crash.Crash "Inf.selectTyp"

    fun selectMod'(MOD(x, j, d))	= j
      | selectMod' _			= raise Crash.Crash "Inf.selectMod'"

    fun selectMod(MOD(x, j, SOME p))	= p
      | selectMod(MOD(x, j, NONE))	= idPath x
      | selectMod _			= raise Crash.Crash "Inf.selectMod"

    fun selectInf(INF(x, k, SOME j))	= j
      | selectInf(INF(x, k, NONE))	= ref(CON(k, idPath x))	(* inCon *)
      | selectInf _			= raise Crash.Crash "Inf.selectInf"


    fun lookup space ((_,m), l) =
	case Map.lookup(m, (space,l))
	  of SOME(item::items) => !item
	   | _                 => raise Crash.Crash "Inf.lookup"

    fun lookup' space ((_,m), l, n) =
	case Map.lookup(m, (space,l))
	  of SOME(item::items) =>
		!(Option.valOf(List.find (fn item => itemIndex item = n) items))
	   | _ => raise Crash.Crash "Inf.lookup'"

    fun lookupVal args	= (selectVal' o lookup VAL') args
    fun lookupTyp args	= (selectTyp  o lookup TYP') args
    fun lookupMod args	= (selectMod' o lookup MOD') args
    fun lookupInf args	= (selectInf  o lookup INF') args

    fun lookupVal' args	= (selectVal' o lookup' VAL') args
    fun lookupTyp' args	= (selectTyp  o lookup' TYP') args
    fun lookupMod' args	= (selectMod' o lookup' MOD') args
    fun lookupInf' args	= (selectInf  o lookup' INF') args


  (* Reduction to head normal form *)

    (*UNFINISHED: avoid multiple cloning of curried lambdas somehow *)

    fun reduce(j as ref(APP(j1,p,j2)))	= reduceApp(j, j1, p, j2)
      | reduce(ref(LINK j))		= reduce j
      | reduce _			= ()

    and reduceApp(j, j1 as ref(LAM _), p, j2) =
	( case !(instance j1)
	    of LAM(p1, j11, j12) =>
		(*UNFINISHED: do realisation *)
		(*Path.replace(p1, p)*)
		( j := LINK j12
		; reduce j
		)
	    | _ => Crash.crash "Type.reduceApp"
	)
      | reduceApp(j, j1, p, j2) = ()


  (* Realisation *)

    (* Applied realisations have to be complete:
     *
     *   Whenever there is a p in Dom(#mod_rea rea), then rea must also
     *   contain substitutions for all subitems of module p.
     *
     * This way we avoid substitutions inside paths (which would require
     * passing around a suitable environment), because if there is a
     * substitution for a subpath then there also is a substitution for
     * the complete path.
     *
     * Realisations need not be fully expanded (would be nice to have this
     * property because it would make substitution more efficient, but
     * full expansion is difficult to achieve for the intersect function).
     *)

    and realise (rea: rea, j as ref j')	= j := realise'(rea, j')

    and realise'(rea, LINK j)		= realise'(rea, !j)
      | realise'(rea, TOP)		= TOP
      | realise'(rea, CON c)		= realiseCon(rea, c)
      | realise'(rea, j' as SIG s)	= ( realiseSig(rea, s)
					  ; j'
					  )
      | realise'(rea, j' as ( ARR(_,j1,j2) | LAM(_,j1,j2) )) =
					  ( realise(rea, j1)
					  ; realise(rea, j2)
					  ; j'
					  )
      | realise'(rea, APP(j1,p,j2))	= ( realise(rea, j1)
					  ; realise(rea, j2)
					  (* UNFINISHED: do reduction *)
					  ; APP(j1, realisePath(#mod_rea rea, p), j2)
					  )

    and realiseKind (rea, ref k')	= realiseKind'(rea, k')
    and realiseKind'(rea, GROUND)	= ()
      | realiseKind'(rea, DEP(_,j,k))	= ( realise(rea, j)
					  ; realiseKind(rea, k)
					  )

    and realiseSig(rea, (ref items, _))	=
	    List.app (fn item => realiseItem(rea, item)) items

    and realiseItem(rea, item as ref(VAL(x, t, w, d))) =
	( realiseTyp(rea, t) ; item := VAL(x, t, w, realiseValDef(rea, d)))
      | realiseItem(rea, ref(TYP(x, k, w, d))) =
	  realiseTypDef(rea, d)
      | realiseItem(rea, item as ref(MOD(x, j, d))) =
	( realise(rea, j) ; item := MOD(x, j, realiseModDef(rea, d)) )
      | realiseItem(rea, ref(INF(x, k, d))) =
	( realiseKind(rea, k) ; realiseInfDef(rea, d) )

    and realiseCon(rea, kp as (k,p)) =
	case PathMap.lookup(#inf_rea rea, p)
	  of SOME j => let val j' = instance j in
			  realise(rea, j') ; LINK j'		(* expand *)
		       end
	   | NONE   => ( realiseKind(rea, k) ; CON kp )

    and realisePath(rea', p)		=
	 case PathMap.lookup(rea', p)
					    of NONE    => p
					     | SOME p' => realisePath(rea', p')
    and realisePathDef(rea', NONE  )	= NONE
      | realisePathDef(rea', SOME p)	= SOME(realisePath(rea', p))

    and realiseValDef(rea, d)		= realisePathDef(#val_rea rea, d)
    and realiseModDef(rea, d)		= realisePathDef(#mod_rea rea, d)

    and realiseTypDef(rea, NONE  )	= ()
      | realiseTypDef(rea, SOME t)	= realiseTyp(rea, t)

    and realiseInfDef(rea, NONE  )	= ()
      | realiseInfDef(rea, SOME j)	= realise(rea, j)

    and realiseTyp(rea, t)		= Type.realise(#typ_rea rea, t)


  (* Instantiation *)

    and instance j			= instanceInf(PathMap.new(), j)

    and instanceInf (rea, ref j')	= ref(instanceInf'(rea, j'))
    and instanceInf'(rea, LINK j)	= instanceInf'(rea, !j)
      | instanceInf'(rea, TOP)		= TOP
      | instanceInf'(rea, CON c)	= CON(instanceCon(rea, c))
      | instanceInf'(rea, SIG s)	= SIG(instanceSig(rea, s))
      | instanceInf'(rea, ARR(p,j1,j2))	= ARR(instancePath(rea, p),
					      instanceInf(rea, j1),
					      instanceInf(rea, j2))
      | instanceInf'(rea, LAM(p,j1,j2))	= LAM(instancePath(rea, p),
					      instanceInf(rea, j1),
					      instanceInf(rea, j2))
      | instanceInf'(rea, APP(j1,p,j2))	= APP(instanceInf(rea, j1),
					      realisePath(rea, p),
					      instanceInf(rea, j2))

    and instanceCon(rea, (k,p))		= ( instanceKind(rea, k),
					    realisePath(rea, p) )

    and instanceKind (rea, ref k')	= ref(instanceKind'(rea, k'))
    and instanceKind'(rea, GROUND)	= GROUND
      | instanceKind'(rea, DEP(p,j,k))	= DEP(instancePath(rea, p),
					      instanceInf(rea, j),
					      instanceKind(rea, k))
    and instancePath(rea, p) =
	let
	    val p' = Path.instance PathMap.lookup (rea, p)
	in
	   (*UNFINISHED: do we need to make the check? *)
	    if p' <> p then PathMap.insert(rea, p, p') else () ;
	    p'
	end

    and instanceSig(rea, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(space_l, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, space_l, [item])
		)

	    fun instanceItem(ref(VAL((p,l,n), t, w, d))) =
		let
		    val p'   = instancePath(rea, p)
		    val t'   = instanceTyp(rea, t)
		    val d'   = instancePathDef(rea, d)
		    val item = ref(VAL((p',l,n), t', w, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | instanceItem(ref(TYP((p,l,n), k, w, d))) =
		let
		    val p'   = instancePath(rea, p)
		    val d'   = instanceTypDef(rea, d)
		    val item = ref(TYP((p',l,n), k, w, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | instanceItem(ref(MOD((p,l,n), j, d))) =
		let
		    val p'   = instancePath(rea, p)
		    val j'   = instanceInf(rea, j)
		    val d'   = instancePathDef(rea, d)
		    val item = ref(MOD((p',l,n), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | instanceItem(ref(INF((p,l,n), k, d))) =
		let
		    val p'   = instancePath(rea, p)
		    val k'   = instanceKind(rea, k)
		    val d'   = instanceInfDef(rea, d)
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end
	in
	    Misc.List_appr instanceItem items ;
	    s
	end

    and instancePathDef(rea, NONE  )	= NONE
      | instancePathDef(rea, SOME p)	= SOME(realisePath(rea, p))

    and instanceTypDef(rea, NONE  )	= NONE
      | instanceTypDef(rea, SOME t)	= SOME(instanceTyp(rea, t))

    and instanceInfDef(rea, NONE  )	= NONE
      | instanceInfDef(rea, SOME j)	= SOME(instanceInf(rea, j))

    and instanceTyp(rea, t)		= let val t' = Type.clone t in
					     Type.realisePath(rea, t') ; t'
					  end


  (* Creation of singleton (shallow instantiation) *)

    and singleton j			= singletonInf(PathMap.new(), j)

    and singletonInf (rea, ref j')	= ref(singletonInf'(rea, j'))
    and singletonInf'(rea, LINK j)	= singletonInf'(rea, !j)
      | singletonInf'(rea, TOP)		= TOP
      | singletonInf'(rea, CON c)	= CON(singletonCon(rea, c))
      | singletonInf'(rea, SIG s)	= SIG(singletonSig(rea, s))
      | singletonInf'(rea,ARR(p,j1,j2))	= ARR(singletonPath(rea, p),
					      singletonInf(rea, j1),
					      singletonInf(rea, j2))
      | singletonInf'(rea,LAM(p,j1,j2))	= LAM(singletonPath(rea, p),
					      singletonInf(rea, j1),
					      singletonInf(rea, j2))
      | singletonInf'(rea,APP(j1,p,j2))	= APP(singletonInf(rea, j1),
					      realisePath(rea, p),
					      singletonInf(rea, j2))

    and singletonCon(rea, (k,p))	= ( singletonKind(rea, k),
					    realisePath(rea, p) )

    and singletonKind (rea, ref k')	= ref(singletonKind'(rea, k'))
    and singletonKind'(rea, GROUND)	= GROUND
      | singletonKind'(rea, DEP(p,j,k))	= DEP(singletonPath(rea, p),
					      singletonInf(rea, j),
					      singletonKind(rea, k))

    and singletonPath(rea, p) = Path.instance PathMap.lookup (rea, p)

    and singletonSig(rea, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(space_l, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, space_l, [item])
		)

	    fun singletonItem(ref(VAL((p,l,n), t, w, d))) =
		let
		    val p'   = singletonPath(rea, p)
		    val t'   = singletonTyp(rea, t)
		    val d'   = singletonPathDef(rea, d)
		    val item = ref(VAL((p',l,n), t', w, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | singletonItem(ref(TYP((p,l,n), k, w, d))) =
		let
		    val p'   = singletonPath(rea, p)
		    val d'   = singletonTypDef(rea, d)
		    val item = ref(TYP((p',l,n), k, w, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | singletonItem(ref(MOD((p,l,n), j, d))) =
		let
		    val p'   = singletonPath(rea, p)
		    val j'   = singletonInf(rea, j)
		    val d'   = singletonPathDef(rea, d)
		    val item = ref(MOD((p',l,n), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | singletonItem(ref(INF((p,l,n), k, d))) =
		let
		    val p'   = singletonPath(rea, p)
		    val k'   = singletonKind(rea, k)
		    val d'   = singletonInfDef(rea, d)
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end
	in
	    Misc.List_appr singletonItem items ;
	    s
	end

    and singletonPathDef(rea, NONE  )	= NONE
      | singletonPathDef(rea, SOME p)	= SOME(realisePath(rea, p))

    and singletonTypDef(rea, NONE  )	= NONE
      | singletonTypDef(rea, SOME t)	= SOME(singletonTyp(rea, t))

    and singletonInfDef(rea, NONE  )	= NONE
      | singletonInfDef(rea, SOME j)	= SOME(singletonInf(rea, j))

    and singletonTyp(rea, t)		= let val t' = Type.clone t in
					     Type.realisePath(rea, t') ; t'
					  end


  (* Cloning (does not instantiate paths!) *)

    fun clone(ref j')		= ref(clone' j')
    and clone'(LINK j)		= clone'(!j)
      | clone'(TOP)		= TOP
      | clone'(CON c)		= CON(cloneCon c)
      | clone'(SIG s)		= SIG(cloneSig s)
      | clone'(ARR(p,j1,j2))	= ARR(p, clone j1, clone j2)
      | clone'(LAM(p,j1,j2))	= LAM(p, clone j1, clone j2)
      | clone'(APP(j1,p,j2))	= APP(clone j1, p, clone j2)

    and cloneCon (k,p)		= (cloneKind k, p)

    and cloneKind (ref k')	= ref(cloneKind' k')
    and cloneKind'(GROUND)	= GROUND
      | cloneKind'(DEP(p,j,k))	= DEP(p, clone j, cloneKind k)

    and cloneSig (ref items, _)	=
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(space_l, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, space_l, [item])
		)

	    fun cloneItem(ref(item' as VAL(x,t,w,d))) =
		    extendSig((VAL', idLab x), ref item')
	      | cloneItem(ref(item' as TYP(x,k,w,d))) =
		    extendSig((TYP', idLab x), ref item')
	      | cloneItem(ref(MOD(x,j,d))) =
		let val item' = MOD(x, clone j, d) in
		    extendSig((MOD', idLab x), ref item')
		end
	      | cloneItem(ref(INF(x,k,d))) =
		let val item' = INF(x, cloneKind k, Option.map clone d) in
		    extendSig((INF', idLab x), ref item')
		end
	in
	    Misc.List_appr cloneItem items ;
	    s
	end


  (* Creation and injections *)

    fun inTop()		= ref TOP
    fun inCon c		= ref(CON c)
    fun inSig s		= ref(SIG s)
    fun inArrow pjj	= ref(ARR pjj)
    fun inLambda pjj	= ref(LAM pjj)
    fun inApp jpj	= let val j = ref(APP jpj) in reduce j ; j end

    fun pathToPath  p	= p
    fun pathToTyp k p	= Type.inCon(k, Type.CLOSED, p)
    fun pathToInf k p	= inCon(k,p)


  (* Projections and extractions *)

    exception Interface

    fun asInf j		= !(follow j)

    fun isTop j		= case asInf j of TOP   => true | _ => false
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

    fun strengthen(p, ref(SIG s))  = strengthenSig(p, s)
      | strengthen(p, ref(LINK j)) = strengthen(p, j)
      | strengthen(p, _)           = ()

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
	    val _  = strengthen(idPath x, j)
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

    and strengthenId(p, pln)		= Path.strengthen(p, pln)

    and strengthenPathDef(p, NONE)	= SOME p
      | strengthenPathDef(p, d)		= d

    and strengthenTypDef(p, k, NONE)	= SOME(pathToTyp k p)
      | strengthenTypDef(p, k, d)	= d

    and strengthenInfDef(p, k, NONE)	= SOME(pathToInf k p)
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
    and kind'( TOP
	     | SIG _
	     | ARR _ )		= inGround()
      | kind'(CON(k,p))		= k
      | kind'(LAM(p,j1,j2))	= inDependent(p, j1, kind j2)
      | kind'(APP(j1,p,j2))	= (*UNFINISHED*) inGround()
      | kind'(LINK j)		= kind j


  (* Matching *)

    datatype mismatch =
	  MissingVal  of lab
	| MissingTyp  of lab
	| MissingMod  of lab
	| MissingInf  of lab
	| ManifestVal of lab
	| ManifestTyp of lab
	| ManifestMod of lab
	| ManifestInf of lab
	| MismatchVal of lab * typ * typ
	| MismatchTyp of lab * tkind * tkind
	| MismatchMod of lab * mismatch
	| MismatchInf of lab * mismatch
	| MismatchValSort of lab * val_sort * val_sort
	| MismatchTypSort of lab * typ_sort * typ_sort
	| Incompatible    of inf * inf
	| IncompatibleArg of path * path

    exception Mismatch of mismatch


    fun matchDef (equals, err) (l ,_,       NONE   ) = ()
      | matchDef (equals, err) (l, NONE,    SOME _ ) = raise Mismatch(err l)
      | matchDef (equals, err) (l, SOME x1, SOME x2) =
	    if equals(x1,x2) then () else raise Mismatch(err l)

    fun matchValDef x = matchDef(op=, ManifestVal) x
    fun matchTypDef x = matchDef(Type.equals, ManifestTyp) x
    fun matchModDef x = matchDef(op=, ManifestMod) x
    fun matchInfDef x = matchDef(equals, ManifestInf) x

    and matchSig(rea, (ref items1, m1), s2 as (ref items2, m2)) =
	let
	    val {val_rea, typ_rea, mod_rea, inf_rea} = rea

	    fun pair(m1,      [],      pairs) = List.rev pairs
	      | pair(m1, item2::items, pairs) =
		let
		    val (p,l,n) = itemId item2
		    val  space  = itemSpace item2
		    val  item1  = List.hd(Map.lookupExistent(m1, (space,l)))
		in
		    if p = itemPath item1 then () else
		    case space
		     of VAL' => PathMap.insert(val_rea, p, selectVal(!item1))
		      | TYP' => PathMap.insert(typ_rea, p, selectTyp(!item1))
		      | INF' => PathMap.insert(inf_rea, p, selectInf(!item1))
		      | MOD' => let val p1 = selectMod(!item1)
				    val j1 = selectMod'(!item1)
				    val j2 = selectMod'(!item2)
				in
				    PathMap.insert(mod_rea, p, p1) ;
				    matchNested(j1, j2)
				    handle Mismatch mismatch =>
					raise Mismatch(MismatchMod(l, mismatch))
				end ;
		    pair(m1, items, (item1,item2)::pairs)
		end
		handle Map.Lookup (VAL',l) => raise Mismatch(MissingVal l)
		     | Map.Lookup (TYP',l) => raise Mismatch(MissingTyp l)
		     | Map.Lookup (MOD',l) => raise Mismatch(MissingMod l)
		     | Map.Lookup (INF',l) => raise Mismatch(MissingInf l)

	    (* Necessary to create complete realisation. *)
	    and matchNested(ref(SIG(_,m1)), ref(SIG(ref items2,_))) =
		    ignore(pair(m1, items2, []))
	      | matchNested(ref(ARR _), ref(ARR _)) =
		(*UNFINISHED: when introducing functor paths*) ()
	      | matchNested(ref(LINK j1), j2) = matchNested(j1, j2)
	      | matchNested(j1, ref(LINK j2)) = matchNested(j1, j2)
	      | matchNested _ = ()

	    val pairs = pair(m1, items2, [])
	in
	    realiseSig(rea, s2) ;
	    List.app matchItem pairs
	end

    and matchItem(ref item1', item2 as ref item2') =
	( matchItem'(item1', item2') ; item2 := item1' )

    and matchItem'(VAL(x1,t1,w1,d1), VAL(x2,t2,w2,d2)) =
	let val l = idLab x2 in
	    matchTyp(l, t1, t2) ;
	    matchValSort(l, w1, w2) ;
	    matchValDef(l, d1, d2)
	end
      | matchItem'(TYP(x1,k1,w1,d1), TYP(x2,k2,w2,d2)) =
	let val l = idLab x2 in
	    matchTKind(l, k1, k2) ;
	    matchTypSort(l, w1, w2) ;
	    matchTypDef(l, d1, d2)
	end
      | matchItem'(MOD(x1,j1,d1), MOD(x2,j2,d2)) =
	let val l = idLab x2 in
	    matchInf(l, j1, j2) ;
	    matchModDef(l, d1, d2)
	end
      | matchItem'(INF(x1,k1,d1), INF(x2,k2,d2)) =
	let val l = idLab x2 in
	    matchKind(l, k1, k2) ;
	    matchInfDef(l, d1, d2)
	end
      | matchItem' _ = raise Crash.crash "Inf.matchItem"

    and matchTyp(l,t1,t2) =
	if Type.matches(t1,t2) then () else
	    raise Mismatch(MismatchVal(l,t1,t2))

    and matchTKind(l,k1,k2) =
	if k1 = k2 then () else
	    raise Mismatch(MismatchTyp(l,k1,k2))

    and matchInf(l,j1,j2) =
	match'(emptyRea(), j1, j2)
	handle Mismatch mismatch =>
	    raise Mismatch(MismatchMod(l, mismatch))

    and matchKind(l,k1,k2) =
	(*UNFINISHED: match contravariant*)
	equaliseKind(k1,k2)
	handle Mismatch mismatch =>
	    raise Mismatch(MismatchInf(l, mismatch))

    and matchValSort(l,w1,w2) =
	if w1 = CONSTRUCTOR orelse w2 = VALUE then () else
	    raise Mismatch(MismatchValSort(l, w1, w2))

    and matchTypSort(l,w1,w2) =
	if w1 = OPEN orelse w2 = CLOSED then () else
	    raise Mismatch(MismatchTypSort(l, w1, w2))


    and match'(rea, _, ref TOP) = ()
      | match'(rea, j1 as ref(CON(_,p1)), j2 as ref(CON(_,p2))) =
	if p1 = p2 then
	    ()
	else
	    raise Mismatch(Incompatible(j1,j2))

      | match'(rea, ref(SIG s1), ref(SIG s2)) = matchSig(rea, s1, s2)

      | match'(rea, ref(ARR(p1,j11,j12)), ref(ARR(p2,j21,j22))) =
	(*UNFINISHED*)
	    ()

      | match'(rea, ref(LAM(p1,j11,j12)), ref(LAM(p2,j21,j22))) =
	(*UNFINISHED*)
	    ()

      | match'(rea, ref(APP(j11,p1,j12)), ref(APP(j21,p2,j22))) =
	( match'(rea, j11, j21) ;
	  if p1 = p2 then
	      ()
	  else
	      raise Mismatch(IncompatibleArg(p1,p2))
	)

      | match'(rea, ref(LINK j1), j2)	= match'(rea, j1, j2)
      | match'(rea, j1, ref(LINK j2))	= match'(rea, j1, j2)
      | match'(rea, j1,j2)		= raise Mismatch(Incompatible(j1,j2))


    and equals(j1,j2) = (*UNFINISHED*) true

    and equaliseKind(k1,k2) = (*UNFINISHED*) ()


    fun match(j1,j2) =
	let
	    val rea = emptyRea()
	in
	    match'(rea, j1, j2) ;
	    rea
	end



  (* Intersection *)

    (* UNFINISHED: does ignore dependencies on second argument signature *)

    fun intersectDef (equals, err) (l, NONE,    NONE   ) = NONE
      | intersectDef (equals, err) (l, NONE,    SOME z ) = SOME z
      | intersectDef (equals, err) (l, SOME z,  NONE   ) = SOME z
      | intersectDef (equals, err) (l, SOME z1, SOME z2) =
	    if equals(z1,z2) then SOME z1 else raise Mismatch(err l)

    fun intersectValDef x = intersectDef(op=, ManifestVal) x
    fun intersectTypDef x = intersectDef(Type.equals, ManifestTyp) x
    fun intersectModDef x = intersectDef(op=, ManifestMod) x
    fun intersectInfDef x = intersectDef(equals, ManifestInf) x

    fun intersectSig(rea, s1 as (itemsr1 as ref items1, m1),
			  s2 as (itemsr2 as ref items2, m2)) =
	let
	    fun pairDef(rea', toZ, b, x1, NONE, x2, SOME z) =
		    ( if b then PathMap.insert(rea', idPath x1, z) else ()
		    ; false )
	      | pairDef(rea', toZ, b, x1, SOME z, x2, NONE) =
		    ( if b then PathMap.insert(rea', idPath x2, z) else ()
		    ; true )
	      | pairDef(rea', toZ, b, x1, SOME z1, x2, SOME z2) =
		    ( if b then PathMap.insert(rea', idPath x2, z1) else ()
		    ; true )
	      | pairDef(rea', toZ, b, x1, NONE, x2, NONE) =
		    ( if b then PathMap.insert(rea', idPath x2, toZ(idPath x1))
			   else ()
		    ; true )

	    fun pair1(b, VAL(x1,t1,w1,d1), VAL(x2,t2,w2,d2)) =
		    pairDef(#val_rea rea, pathToPath, b, x1, d1, x2, d2)
	      | pair1(b, TYP(x1,k1,w1,d1), TYP(x2,k2,w2,d2)) =
		    pairDef(#typ_rea rea, pathToTyp k1, b, x1, d1, x2, d2)
	      | pair1(b, MOD(x1,j1,d1), MOD(x2,j2,d2)) =
		    pairDef(#mod_rea rea, pathToPath, b, x1, d1, x2, d2)
		    before pairNested(j1,j2)
	      | pair1(b, INF(x1,k1,d1), INF(x2,k2,d2)) =
		    pairDef(#inf_rea rea, pathToInf k1, b, x1, d1, x2, d2)
	      | pair1 _ =
		    raise Crash.crash "Inf.intersectSig: pairing"

	    and pair(m1, [], pairs, left) = ( List.rev pairs, List.rev left )
	      | pair(m1, item2::items, pairs, left) =
		case Map.lookup(m1, (itemSpace item2, itemLab item2))
		  of NONE => pair(m1, items, pairs, item2::left)
		   | SOME [] => raise Crash.crash "Inf.intersectSig: lookup"
		   | SOME(item1::_) =>
		     (* Nested structures are already realised.
		      * We would loop during realisation if we inserted
		      * identity realisations. *)
		     ( if pair1(itemPath item1 <> itemPath item2,
				!item1, !item2) then () else
			  Misc.General_swap(item1, item2)
		     ; pair(m1, items, (item1,item2)::pairs, left)
		     )

	    (* Necessary to create complete realisation. *)
	    and pairNested(ref(SIG(_,m1)), ref(SIG(ref items2,_))) =
		    ignore(pair(m1, items2, [], []))
	      | pairNested(ref(ARR _), ref(ARR _)) =
		(*UNFINISHED: when introducing functor paths*) ()
	      | pairNested(ref(LINK j1), j2) = pairNested(j1, j2)
	      | pairNested(j1, ref(LINK j2)) = pairNested(j1, j2)
	      | pairNested _ = ()

	    val (pairs,left) = pair(m1, items2, [], [])
	in
	    realiseSig(rea, s1) ;
	    realiseSig(rea, s2) ;
	    List.app (intersectItem rea) pairs ;
	    itemsr1 := items1 @ left ;
	    itemsr2 := !itemsr1
	end
    and intersectItem rea (item1 as ref item1', ref item2') =
	    item1 := intersectItem'(rea, item1', item2')

    and intersectItem'(rea, VAL(x1,t1,w1,d1), VAL(x2,t2,w2,d2)) =
	let
	    val l = idLab x1
	    val t = intersectTyp(l, t1, t2)
	    val w = intersectValSort(l, w1, w2)
	    val d = intersectValDef(l, d1, d2)
	in
	    VAL(x1,t,w,d)
	end
      | intersectItem'(rea, TYP(x1,k1,w1,d1), TYP(x2,k2,w2,d2)) =
	let
	    val l = idLab x1
	    val k = intersectTKind(l, k1, k2)
	    val w = intersectTypSort(l, w1, w2)
	    val d = intersectTypDef(l, d1, d2)
	in
	    TYP(x1,k,w,d)
	end
      | intersectItem'(rea, MOD(x1,j1,d1), MOD(x2,j2,d2)) =
	let
	    val l = idLab x1
	    val j = intersectInf(l, j1, j2)
	    val d = intersectModDef(l, d1, d2)
	in
	    MOD(x1,j,d)
	end
      | intersectItem'(rea, INF(x1,k1,d1), INF(x2,k2,d2)) =
	let
	    val l = idLab x1
	    val k = intersectKind(l, k1, k2)
	    val d = intersectInfDef(l, d1, d2)
	in
	    INF(x1,k,d)
	end
      | intersectItem' _ = raise Crash.crash "Inf.intersectItem"

    and intersectTyp(l,t1,t2) =
	( Type.intersect(t1,t2) ; t1 )
	handle Type.Intersect =>
	    raise Mismatch(MismatchVal(l,t1,t2))

    and intersectTKind(l,k1,k2) =
	if k1 = k2 then k1 else
	    raise Mismatch(MismatchTyp(l,k1,k2))

    and intersectInf(l,j1,j2) =
	intersect'(emptyRea(), j1, j2)
	handle Mismatch mismatch =>
	    raise Mismatch(MismatchMod(l, mismatch))

    and intersectKind(l,k1,k2) =
	( equaliseKind(k1,k2) ; k1 )
	handle Mismatch mismatch =>
	    raise Mismatch(MismatchInf(l, mismatch))

    and intersectValSort(l,w1,w2) =
	if w1 = CONSTRUCTOR orelse w2 = CONSTRUCTOR then
	    CONSTRUCTOR
	else
	    VALUE

    and intersectTypSort(l,w1,w2) =
	if w1 = OPEN orelse w2 = OPEN then
	    OPEN
	else
	    CLOSED


    and intersect'(rea, j1, ref TOP) = j1
      | intersect'(rea, ref TOP, j2) = j2
      | intersect'(rea, j1 as ref(CON(_,p1)), j2 as ref(CON(_,p2))) =
	if p1 = p2 then
	    j1
	else
	    raise Mismatch(Incompatible(j1,j2))

      | intersect'(rea, j1 as ref(SIG s1), ref(SIG s2)) =
	    ( intersectSig(rea, s1, s2) ; j1 )

      | intersect'(rea, ref(ARR(p1,j11,j12)), ref(ARR(p2,j21,j22))) =
	(*UNFINISHED*)
	    raise Crash.crash "Inf.intersect: ARR"

      | intersect'(rea, ref(LAM(p1,j11,j12)), ref(LAM(p2,j21,j22))) =
	(*UNFINISHED*)
	    raise Crash.crash "Inf.intersect: LAM"

      | intersect'(rea, j1 as ref(APP(j11,p1,j12)), ref(APP(j21,p2,j22))) =
	(*UNFINISHED*)
	    raise Crash.crash "Inf.intersect: APP"

      | intersect'(rea, ref(LINK j1), j2) = intersect'(rea, j1, j2)
      | intersect'(rea, j1, ref(LINK j2)) = intersect'(rea, j1, j2)
      | intersect'(rea, j1,j2)            = raise Mismatch(Incompatible(j1,j2))


    fun intersect(j1,j2) =
	let
	    val j1' = clone j1
	    val j2' = clone j2
	    val rea = emptyRea()
	in
	    intersect'(rea, j1', j2')
	end

  end


structure Inf : INF = InfPrivate
