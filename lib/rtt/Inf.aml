(* Interfaces contain state. This means that they must be instantiated
   at each occurance. *)

structure InfPrivate =
  struct

  (* Types *)

    type lab	= Label.t
    type name	= Name.t
    type path	= Path.t
    type typ	= Type.t
    type tkind	= Type.kind
    type fix    = Fixity.t

    datatype val_sort = VALUE | CONSTRUCTOR of int	(* [w] *)
    datatype typ_sort = datatype Type.sort		(* [w] *)

    type id	= path * lab * int			(* [x] *)
    type 'a def	= 'a option				(* [d] *)


    (* A map for signatures *)

    datatype space = VAL' | TYP' | MOD' | INF' | FIX'

    structure Map = MakeHashImpMap(struct type t = space * lab
					  val equals = op=
					  fun hash(_,l) = Label.hash l end)


    datatype inf' =
	  TOP					(* top *)
	| CON    of con				(* interface constructor *)
	| SIG    of sign			(* signature *)
	| FUN    of path * inf * inf		(* arrow (functor) *)
	| LAMBDA of path * inf * inf		(* abstraction (dep. function)*)
	| APPLY  of inf * path * inf		(* application *)
	| LINK   of inf				(* forward (for substitution) *)
	| ABBREV of inf * inf			(* abbreviations *)

    and item' =
	  VAL of id *  typ  * val_sort * path def	(* value *)
	| TYP of id * tkind * typ_sort * typ def	(* type *)
	| MOD of id *  inf  * path def			(* module *)
	| INF of id *  kind * inf def			(* interface *)
	| FIX of id *  fix				(* fixity *)

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
    and itemId'(VAL(x,_,_,_))	= x
      | itemId'(TYP(x,_,_,_))	= x
      | itemId'(MOD(x,_,_))	= x
      | itemId'(INF(x,_,_))	= x
      | itemId'(FIX(x,_))	= x

    fun itemPath  item		= idPath(itemId item)
    fun itemPath' item'		= idPath(itemId' item')
    fun itemLab   item		= idLab(itemId item)
    fun itemIndex item		= idIndex(itemId item)

    fun itemSpace(ref item')	= itemSpace' item'
    and itemSpace'(VAL _)	= VAL'
      | itemSpace'(TYP _)	= TYP'
      | itemSpace'(MOD _)	= MOD'
      | itemSpace'(INF _)	= INF'
      | itemSpace'(FIX _)	= FIX'


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
    val newFix			= newItem

    fun hideId (p,l,n)		= (p,l,n+1)
    fun hide item		= item := hide'(!item)
    and hide'(VAL(x,t,w,d))	= VAL(hideId x, t, w, d)
      | hide'(TYP(x,k,w,d))	= TYP(hideId x, k, w, d)
      | hide'(MOD(x,j,d))	= MOD(hideId x, j, d)
      | hide'(INF(x,k,d))	= INF(hideId x, k, d)
      | hide'(FIX(x,q))		= FIX(hideId x, q)

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
    fun extendFix(s,p,q)	= extend(s, FIX', p, fn x => FIX(x,q))


  (* Signature inspection *)

    exception Item

    fun items(ref items, _) = List.filter (fn item => itemIndex item = 0) items

    fun isValItem(ref(VAL _))		= true
      | isValItem _			= false
    fun isTypItem(ref(TYP _))		= true
      | isTypItem _			= false
    fun isModItem(ref(MOD _))		= true
      | isModItem _			= false
    fun isInfItem(ref(INF _))		= true
      | isInfItem _			= false
    fun isFixItem(ref(FIX _))		= true
      | isFixItem _			= false

    fun asValItem(ref(VAL(x,t,s,d)))	= (idLab x, t, s, d)
      | asValItem _			= raise Item
    fun asTypItem(ref(TYP(x,k,s,d)))	= (idLab x, k, s, d)
      | asTypItem _			= raise Item
    fun asModItem(ref(MOD(x,j,d)))	= (idLab x, j, d)
      | asModItem _			= raise Item
    fun asInfItem(ref(INF(x,k,d)))	= (idLab x, k, d)
      | asInfItem _			= raise Item
    fun asFixItem(ref(FIX(x,q)))	= (idLab x, q)
      | asFixItem _			= raise Item


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

    fun selectFix(FIX(x, f))		= f
      | selectFix _			= raise Crash.Crash "Inf.selectFix"

    fun selectValSort(VAL(x, t, w, d))	= w
      | selectValSort _			= raise Crash.Crash "Inf.selectValSort"

    fun selectTypSort(TYP(x, k, w, d))	= w
      | selectTypSort _			= raise Crash.Crash "Inf.selectTypSort"


    exception Lookup

    fun lookup space ((_,m), l) =
	case Map.lookup(m, (space,l))
	  of SOME(item::items) => !item
	   | _                 => raise Lookup

    fun lookup' space ((_,m), l, n) =
	case Map.lookup(m, (space,l))
	  of SOME(item::items) =>
		!(Option.valOf(List.find (fn item => itemIndex item = n) items))
	   | _ => raise Lookup

    fun pathVal args	= (itemPath' o lookup VAL') args
    fun pathTyp args	= (itemPath' o lookup TYP') args
    fun pathMod args	= (itemPath' o lookup MOD') args
    fun pathInf args	= (itemPath' o lookup INF') args
    fun pathFix args	= (itemPath' o lookup FIX') args

    fun lookupVal args	= (selectVal' o lookup VAL') args
    fun lookupTyp args	= (selectTyp  o lookup TYP') args
    fun lookupMod args	= (selectMod' o lookup MOD') args
    fun lookupInf args	= (selectInf  o lookup INF') args
    fun lookupFix args	= (selectFix  o lookup FIX') args

    fun lookupVal' args	= (selectVal' o lookup' VAL') args
    fun lookupTyp' args	= (selectTyp  o lookup' TYP') args
    fun lookupMod' args	= (selectMod' o lookup' MOD') args
    fun lookupInf' args	= (selectInf  o lookup' INF') args
    fun lookupFix' args	= (selectFix  o lookup' FIX') args

    fun lookupValSort args = (selectValSort o lookup VAL') args
    fun lookupTypSort args = (selectTypSort o lookup TYP') args

    fun lookupValPath args = (selectVal o lookup VAL') args
    fun lookupModPath args = (selectMod o lookup MOD') args


  (* Closure check *)

    exception Unclosed of lab * int * typ

    fun close (ref items,_) = ()(*List.app closeItem items*)

    and closeItem(ref(VAL((p,l,n), t, w, d))) =
	if Type.isClosed t then () else
	    raise Unclosed(l,n,t)
      | closeItem _ = ()
	(* ASSUME that nested structures are always closed *)


  (* Reduction to head normal form *)

    (*UNFINISHED: avoid multiple cloning of curried lambdas somehow *)

    fun reduce(j as ref(APPLY(j1,p,j2))) =
	let
	    fun reduceApply(j1 as ref(LAMBDA _), jo) =
		( case !(instance j1)
		    of LAMBDA(p1, j11, j12) =>
			(*UNFINISHED: do realisation *)
			(*Path.replace(p1, p)*)
			( j := LINK j12
			; reduce j
			)
		    | _ => raise Crash.Crash "Inf.reduceApply"
		)
	      | reduceApply(ref(LINK j11), jo) =
		    reduceApply(follow j11, jo)

	      | reduceApply(ref(ABBREV(j11,j12)), jo) =
		    reduceApply(follow j12, SOME(Option.getOpt(jo,j11)))

	      | reduceApply _ = ()
	in
	    reduceApply(j1, NONE)
	end

      | reduce(ref(LINK j | ABBREV(_,j))) = reduce j

      | reduce _ = ()


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
     * Realisations need not be idempotent (fully expanded). Would be nice
     * to have this property because it would make substitution more efficient,
     * but full expansion is difficult to achieve for the intersect function).
     *)

    and realise (rea: rea, j as ref j')	= j := realise'(rea, j')

    and realise'(rea, LINK j)		= realise'(rea, !j)
      | realise'(rea, TOP)		= TOP
      | realise'(rea, CON c)		= realiseCon(rea, c)
      | realise'(rea, j' as SIG s)	= ( realiseSig(rea, s)
					  ; j'
					  )
      | realise'(rea, j' as (FUN(_,j1,j2) | LAMBDA(_,j1,j2) | ABBREV(j1,j2))) =
					  ( realise(rea, j1)
					  ; realise(rea, j2)
					  ; j'
					  )
      | realise'(rea, APPLY(j1,p,j2))	= ( realise(rea, j1)
					  ; realise(rea, j2)
					  (* UNFINISHED: do reduction *)
					  ; APPLY(j1, realisePath(#mod_rea rea,
								  p), j2)
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
      | realiseItem(rea, ref(FIX _)) = ()

    and realiseCon(rea, kp as (k,p)) =
	case PathMap.lookup(#inf_rea rea, p)
	  of SOME j => let val j' = instance j in
			  realise(rea, j') ; LINK j'		(* expand *)
		       end
	   | NONE   => ( realiseKind(rea, k) ; CON kp )

    and realisePath(rea', p)		= case PathMap.lookup(rea', p)
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

    (*
     * This is really ugly! To maintain sharing of types in signatures
     * at signature instantiation we have to clone types with those special
     * functions from the Type module. This implies that we cannot apply
     * the path realisation build up during instantiation on-the-fly to
     * types (types cloned by Type.cloneCont may not be touched before
     * executing Type.cloneFinish). Consequently, we have to do a second
     * walk over the interface to perform those realisations. :-(
     *)

    and instance j =
	let
	    val rea'       = PathMap.new()
	    val cloneState = Type.cloneStart()
	    val j1         = instanceInf(rea', cloneState, j)
	in
	    Type.cloneFinish cloneState ;
	    realiseT(rea', j1) ;
	    j1
	end

    and instanceInf (r,z, ref j')		= ref(instanceInf'(r,z, j'))
    and instanceInf'(r,z, LINK j)		= instanceInf'(r,z, !j)
      | instanceInf'(r,z, TOP)			= TOP
      | instanceInf'(r,z, CON c)		= CON(instanceCon(r,z, c))
      | instanceInf'(r,z, SIG s)		= SIG(instanceSig(r,z, s))
      | instanceInf'(r,z, FUN(p,j1,j2))		= FUN(instancePath(r, p),
						      instanceInf(r,z, j1),
						      instanceInf(r,z, j2))
      | instanceInf'(r,z, LAMBDA(p,j1,j2))	= LAMBDA(instancePath(r, p),
							 instanceInf(r,z, j1),
							 instanceInf(r,z, j2))
      | instanceInf'(r,z, APPLY(j1,p,j2))	= APPLY(instanceInf(r,z, j1),
							realisePath(r, p),
							instanceInf(r,z, j2))
      | instanceInf'(r,z, ABBREV(j1,j2))	= ABBREV(instanceInf(r,z, j1),
							 instanceInf(r,z, j2))

    and instanceCon(r,z, (k,p))			= ( instanceKind(r,z, k),
						    realisePath(r, p) )

    and instanceKind (r,z, ref k')		= ref(instanceKind'(r,z, k'))
    and instanceKind'(r,z, GROUND)		= GROUND
      | instanceKind'(r,z, DEP(p,j,k))		= DEP(instancePath(r, p),
						      instanceInf(r,z, j),
						      instanceKind(r,z, k))
    and instancePath(rea, p) =
	let
	    val p' = Path.instance PathMap.lookup (rea, p)
	in
	    (*UNFINISHED: do we need to make the check? *)
	    if Path.equals(p',p) then () else PathMap.insert(rea, p, p') ;
	    p'
	end

    and instanceSig(r,z, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(space_l, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, space_l, [item])
		)

	    fun instanceItem(ref item') = instanceItem' item'

	    and instanceItem'(VAL((p,l,n), t, w, d)) =
		let
		    val p'   = instancePath(r, p)
		    val t'   = instanceTyp(r,z, t)
		    val d'   = instancePathDef(r, d)
		    val item = ref(VAL((p',l,n), t', w, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | instanceItem'(TYP((p,l,n), k, w, d)) =
		let
		    val p'   = instancePath(r, p)
		    val d'   = instanceTypDef(r,z, d)
		    val item = ref(TYP((p',l,n), k, w, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | instanceItem'(MOD((p,l,n), j, d)) =
		let
		    val p'   = instancePath(r, p)
		    val j'   = instanceInf(r,z, j)
		    val d'   = instancePathDef(r, d)
		    val item = ref(MOD((p',l,n), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | instanceItem'(INF((p,l,n), k, d)) =
		let
		    val p'   = instancePath(r, p)
		    val k'   = instanceKind(r,z, k)
		    val d'   = instanceInfDef(r,z, d)
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end

	      | instanceItem'(FIX((p,l,n), q)) =
		let
		    val p'   = instancePath(r, p)
		    val item = ref(FIX((p',l,n), q))
		in
		    extendSig((FIX',l), item)
		end
	in
	    Misc.List_appr instanceItem items ;
	    s
	end

    and instancePathDef(rea, NONE  )	= NONE
      | instancePathDef(rea, SOME p)	= SOME(realisePath(rea, p))

    and instanceTypDef(r,z, NONE  )	= NONE
      | instanceTypDef(r,z, SOME t)	= SOME(instanceTyp(r,z, t))

    and instanceInfDef(r,z, NONE  )	= NONE
      | instanceInfDef(r,z, SOME j)	= SOME(instanceInf(r,z, j))

    and instanceTyp(r,z, t)		= Type.cloneCont z t
					  (* Cannot do 
						Type.realisePath(r,t')
					     here! *)

    and realiseT (rea', ref j')			= realiseT'(rea', j')

    and realiseT'(rea', LINK j)			= realiseT(rea', j)
      | realiseT'(rea', (TOP | CON _))		= ()
      | realiseT'(rea', SIG s)			= realiseTSig(rea', s)
      | realiseT'(rea', (FUN(_,j1,j2) | LAMBDA(_,j1,j2) | ABBREV(j1,j2))) =
						  ( realiseT(rea', j1)
						  ; realiseT(rea', j2)
						  )
      | realiseT'(rea', APPLY(j1,p,j2))		= ( realiseT(rea', j1)
						  ; realiseT(rea', j2)
						  )

    and realiseTKind (rea', ref k')		= realiseTKind'(rea', k')
    and realiseTKind'(rea, GROUND)		= ()
      | realiseTKind'(rea, DEP(_,j,k))		= ( realiseT(rea, j)
						  ; realiseTKind(rea, k)
						  )

    and realiseTSig(rea', (ref items, _))	=
	    List.app (fn item => realiseTItem(rea', item)) items

    and realiseTItem(rea', ref item')		= realiseTItem'(rea', item')
    and realiseTItem'(rea', VAL(x, t, w, d))	= realiseTTyp(rea', t)
      | realiseTItem'(rea', TYP(x, k, w, d))	= realiseTTypDef(rea', d)
      | realiseTItem'(rea', MOD(x, j, d))	= realiseT(rea', j)
      | realiseTItem'(rea', INF(x, k, d))	= ( realiseTKind(rea', k)
						  ; realiseTInfDef(rea', d)
						  )
      | realiseTItem'(rea', FIX _)		= ()

    and realiseTCon(rea', (k,p))		= realiseTKind(rea', k)

    and realiseTTypDef(rea', NONE  )		= ()
      | realiseTTypDef(rea', SOME t)		= realiseTTyp(rea', t)

    and realiseTInfDef(rea', NONE  )		= ()
      | realiseTInfDef(rea', SOME j)		= realiseT(rea', j)

    and realiseTTyp(rea', t)			= Type.realisePath(rea', t)



  (* Creation of singleton (shallow instantiation) *)

    (* Creates an instance of an interface where every item is equal
     * to the original one.
     *)

    and singleton j =
	let
	    val cloneState = Type.cloneStart()
	    val j1         = singletonInf(cloneState, j)
	in
	    Type.cloneFinish cloneState ;
	    j1
	end

    and singletonInf (z, ref j')		= ref(singletonInf'(z, j'))
    and singletonInf'(z, LINK j)		= singletonInf'(z, !j)
      | singletonInf'(z, TOP)			= TOP
      | singletonInf'(z, CON c)			= CON(singletonCon(z, c))
      | singletonInf'(z, SIG s)			= SIG(singletonSig(z, s))
      | singletonInf'(z, FUN(p,j1,j2))		= FUN(Path.clone p,
						      singletonInf(z, j1),
						      singletonInf(z, j2))
      | singletonInf'(z, LAMBDA(p,j1,j2))	= LAMBDA(Path.clone p,
							 singletonInf(z, j1),
							 singletonInf(z, j2))
      | singletonInf'(z, APPLY(j1,p,j2))	= APPLY(singletonInf(z, j1),
							p,
							singletonInf(z, j2))
      | singletonInf'(z, ABBREV(j1,j2))		= ABBREV(singletonInf(z, j1),
							 singletonInf(z, j2))

    and singletonCon(z, (k,p))			= ( singletonKind(z, k), p )

    and singletonKind (z, ref k')		= ref(singletonKind'(z, k'))
    and singletonKind'(z, GROUND)		= GROUND
      | singletonKind'(z, DEP(p,j,k))		= DEP(Path.clone p,
						      singletonInf(z, j),
						      singletonKind(z, k))

    and singletonSig(z, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(space_l, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, space_l, [item])
		)

	    fun singletonItem(ref item') = singletonItem' item'

	    and singletonItem'(VAL((p,l,n), t, w, d)) =
		let
		    val p'   = Path.clone p
		    val t'   = singletonTyp(z, t)
		    val item = ref(VAL((p',l,n), t', w, d))
		in
		    extendSig((VAL',l), item)
		end

	      | singletonItem'(TYP((p,l,n), k, w, d)) =
		let
		    val p'   = Path.clone p
		    val d'   = singletonTypDef(z, d)
		    val item = ref(TYP((p',l,n), k, w, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | singletonItem'(MOD((p,l,n), j, d)) =
		let
		    val p'   = Path.clone p
		    val j'   = singletonInf(z, j)
		    val item = ref(MOD((p',l,n), j', d))
		in
		    extendSig((MOD',l), item)
		end

	      | singletonItem'(INF((p,l,n), k, d)) =
		let
		    val p'   = Path.clone p
		    val k'   = singletonKind(z, k)
		    val d'   = singletonInfDef(z, d)
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end

	      | singletonItem'(FIX((p,l,n), q)) =
		let
		    val p'   = Path.clone p
		    val item = ref(FIX((p',l,n), q))
		in
		    extendSig((FIX',l), item)
		end
	in
	    Misc.List_appr singletonItem items ;
	    s
	end

    and singletonTypDef(z, NONE  )	= NONE
      | singletonTypDef(z, SOME t)	= SOME(singletonTyp(z, t))

    and singletonInfDef(z, NONE  )	= NONE
      | singletonInfDef(z, SOME j)	= SOME(singletonInf(z, j))

    and singletonTyp(z, t)		= Type.cloneCont z t


  (* Cloning (does not instantiate paths!) *)

    fun clone(ref j')		= ref(clone' j')
    and clone'(LINK j)		= clone'(!j)
      | clone'(TOP)		= TOP
      | clone'(CON c)		= CON(cloneCon c)
      | clone'(SIG s)		= SIG(cloneSig s)
      | clone'(FUN(p,j1,j2))	= FUN(p, clone j1, clone j2)
      | clone'(LAMBDA(p,j1,j2))	= LAMBDA(p, clone j1, clone j2)
      | clone'(APPLY(j1,p,j2))	= APPLY(clone j1, p, clone j2)
      | clone'(ABBREV(j1,j2))	= ABBREV(clone j1, clone j2)

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

	    fun cloneItem(ref item') = cloneItem' item'
	    and cloneItem'(item' as VAL(x,t,w,d)) =
		    extendSig((VAL', idLab x), ref item')
	      | cloneItem'(item' as TYP(x,k,w,d)) =
		    extendSig((TYP', idLab x), ref item')
	      | cloneItem'(MOD(x,j,d)) =
		let val item' = MOD(x, clone j, d) in
		    extendSig((MOD', idLab x), ref item')
		end
	      | cloneItem'(INF(x,k,d)) =
		let val item' = INF(x, cloneKind k, Option.map clone d) in
		    extendSig((INF', idLab x), ref item')
		end
	      | cloneItem'(item' as FIX(x,q)) =
		    extendSig((FIX', idLab x), ref item')
	in
	    Misc.List_appr cloneItem items ;
	    s
	end


  (* Creation and injections *)

    fun inTop()		= ref TOP
    fun inCon c		= ref(CON c)
    fun inSig s		= ref(SIG s)
    fun inArrow pjj	= ref(FUN pjj)
    fun inLambda pjj	= ref(LAMBDA pjj)
    fun inApply jpj	= let val j = ref(APPLY jpj) in reduce j ; j end
    fun inAbbrev jj	= ref(ABBREV jj)

    fun pathToPath  p	= p
    fun pathToTyp k p	= Type.inCon(k, Type.CLOSED, p)
    fun pathToInf k p	= inCon(k,p)


  (* Projections and extractions *)

    exception Interface

    fun asInf(ref(LINK j))	= asInf j
      | asInf(ref(ABBREV(_,j)))	= asInf j
      | asInf(ref j')		= j'

    fun isTop j		= case asInf j of TOP      => true | _ => false
    fun isCon j		= case asInf j of CON _    => true | _ => false
    fun isSig j		= case asInf j of SIG _    => true | _ => false
    fun isArrow j	= case asInf j of FUN _    => true | _ => false
    fun isLambda j	= case asInf j of LAMBDA _ => true | _ => false
    fun isApply j	= case asInf j of APPLY _  => true | _ => false

    fun asCon j		= case asInf j of CON z    => z | _ => raise Interface
    fun asSig j		= case asInf j of SIG z    => z | _ => raise Interface
    fun asArrow j	= case asInf j of FUN z    => z | _ => raise Interface
    fun asLambda j	= case asInf j of LAMBDA z => z | _ => raise Interface
    fun asApply j	= case asInf j of APPLY z  => z | _ => raise Interface

    fun isAbbrev j	= case !(follow j) of ABBREV _  => true | _ => false
    fun asAbbrev j	= case !(follow j) of ABBREV jj => jj   | _ =>
								raise Interface
    fun pathCon(_,p)	= p
    fun path j		= pathCon(asCon j)


  (* Strengthening *)

    fun strengthen(p, ref(SIG s))		= strengthenSig(p, s)
      | strengthen(p, ref(LINK j))		= strengthen(p, j)
      | strengthen(p, ref(ABBREV(j1,j2)))	= ( strengthen(p, j1)
						  ; strengthen(p, j2) )
      | strengthen(p, _)			= ()

    and strengthenSig(p, (ref items, _)) =
	    List.app (fn item => strengthenItem(p, item)) items

    and strengthenItem(p, item as ref item') =
	    item := strengthenItem'(p, item')

    and strengthenItem'(p, VAL(x, t, w, d)) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenPathDef(idPath x, d)
	in
	    VAL(x, t, w, d')
	end

      | strengthenItem'(p, TYP(x, k, w, d)) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenTypDef(idPath x, k, d)
	in
	    TYP(x, k, w, d')
	end

      | strengthenItem'(p, MOD(x, j, d)) =
	let
	    val _  = strengthenId(p, x)
	    val _  = strengthen(idPath x, j)
	    val d' = strengthenPathDef(idPath x, d)
	in
	    MOD(x, j, d')
	end

      | strengthenItem'(p, INF(x, k, d)) =
	let
	    val _  = strengthenId(p, x)
	    val d' = strengthenInfDef(idPath x, k, d)
	in
	    INF(x, k, d')
	end

      | strengthenItem'(p, FIX(x, q)) =
	let
	    val _  = strengthenId(p, x)
	in
	    FIX(x, q)
	end

    and strengthenId(p, (p',l,n))	= Path.strengthen(p,l,n, p')

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
	     | FUN _ )		= inGround()
      | kind'(CON(k,p))		= k
      | kind'(LAMBDA(p,j1,j2))	= inDependent(p, j1, kind j2)
      | kind'(APPLY(j1,p,j2))	= (*UNFINISHED*) inGround()
      | kind'(LINK j)		= kind j
      | kind'(ABBREV(j1,j2))	= kind j2


  (* Matching *)

    datatype mismatch =
	  MissingVal      of lab
	| MissingTyp      of lab
	| MissingMod      of lab
	| MissingInf      of lab
	| MissingFix      of lab
	| ManifestVal     of lab * path option * path
	| ManifestTyp     of lab * typ option * typ
	| ManifestMod     of lab * path option * path
	| ManifestInf     of lab * mismatch option
	| MismatchVal     of lab * typ * typ
	| MismatchTyp     of lab * tkind * tkind
	| MismatchMod     of lab * mismatch
	| MismatchInf     of lab * mismatch
	| MismatchFix     of lab * fix * fix
	| MismatchValSort of lab * val_sort * val_sort
	| MismatchTypSort of lab * typ_sort * typ_sort
	| MismatchDom     of mismatch
	| MismatchRan     of mismatch
	| Incompatible    of inf * inf
	| IncompatibleArg of path * path

    exception Mismatch of mismatch



    fun match(j1,j2) =
	let
	    val rea = emptyRea()
	in
	    match'(rea, j1, j2) ;
	    rea
	end

    and match'(rea, _, ref TOP) = ()
      | match'(rea, j1 as ref(CON(_,p1)), j2 as ref(CON(_,p2))) =
	if Path.equals(p1,p2) then
	    ()
	else
	    raise Mismatch(Incompatible(j1,j2))

      | match'(rea, ref(SIG s1), ref(SIG s2)) = matchSig(rea, s1, s2)

      | match'(rea, j1 as ref(FUN _), j2 as ref(FUN _)) =
	( realise(rea, j2)
	; case (instance j1, instance j2)
	   of (ref(FUN(p1,j11,j12)), ref(FUN(p2,j21,j22))) =>
	      ( match'(rea, j21, j11) handle Mismatch mismatch =>
		    raise Mismatch(MismatchDom mismatch)
	      ; match'(rea, j12, j22) handle Mismatch mismatch =>
		    raise Mismatch(MismatchRan mismatch)
	      )
	    | _ => raise Crash.Crash "Inf.match: funny instantiation"
	)

      | match'(rea, ref(LAMBDA(p1,j11,j12)), ref(LAMBDA(p2,j21,j22))) =
	(*UNFINISHED*)
	    ()

      | match'(rea, ref(APPLY(j11,p1,j12)), ref(APPLY(j21,p2,j22))) =
	( match'(rea, j11, j21)
	; if Path.equals(p1,p2) then () else
	      raise Mismatch(IncompatibleArg(p1,p2))
	)

      | match'(rea, ref(LINK j1), j2)		= match'(rea, j1, j2)
      | match'(rea, j1, ref(LINK j2))		= match'(rea, j1, j2)
      | match'(rea, ref(ABBREV(_,j1)), j2)	= match'(rea, j1, j2)
      | match'(rea, j1, j2 as ref(ABBREV(j21,j22))) =
	( match'(rea, j1, j22)
(*	; j2 := ABBREV(j21,j1)
*)	)

      | match'(rea, j1,j2) = raise Mismatch(Incompatible(j1,j2))


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
		    if Path.equals(p, itemPath item1) then () else
		    case space
		     of VAL' => PathMap.insert(val_rea, p, selectVal(!item1))
		      | TYP' => PathMap.insert(typ_rea, p, selectTyp(!item1))
		      | INF' => PathMap.insert(inf_rea, p, selectInf(!item1))
		      | FIX' => ()
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
		     | Map.Lookup (FIX',l) => raise Mismatch(MissingFix l)

	    (* Necessary to create complete realisation. *)
	    and matchNested(ref(SIG(_,m1)), ref(SIG(ref items2,_))) =
		    ignore(pair(m1, items2, []))
	      | matchNested(ref(FUN _), ref(FUN _)) =
		(*UNFINISHED: when introducing functor paths*) ()
	      | matchNested(ref(LINK j1), j2) = matchNested(j1, j2)
	      | matchNested(j1, ref(LINK j2)) = matchNested(j1, j2)
	      | matchNested(ref(ABBREV(_,j1)), j2) = matchNested(j1, j2)
	      | matchNested(j1, ref(ABBREV(_,j2))) = matchNested(j1, j2)
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
      | matchItem'(FIX(x1,q1), FIX(x2,q2)) =
	let val l = idLab x2 in
	    matchFix(l, q1, q2)
	end
      | matchItem' _ = raise Crash.Crash "Inf.matchItem"

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

    and matchFix(l,q1,q2) =
	if q1 = q2 then () else
	    raise Mismatch(MismatchFix(l,q1,q2))

    and matchValSort(l, w1, VALUE) = ()
      | matchValSort(l, w1 as CONSTRUCTOR k1, w2 as CONSTRUCTOR k2) = 
	if k1 = k2 then () else
	    raise Mismatch(MismatchValSort(l, w1, w2))
      | matchValSort(l,w1,w2) =
	    raise Mismatch(MismatchValSort(l, w1, w2))

    and matchTypSort(l,w1,w2) =
	if w1 = OPEN orelse w2 = CLOSED then () else
	    raise Mismatch(MismatchTypSort(l, w1, w2))


    and matchValDef(l, _,    NONE)	= ()
      | matchValDef(l, NONE, SOME p2)	= raise Mismatch(ManifestVal(l,NONE,p2))
      | matchValDef(l, SOME p1, SOME p2) =
	    if Path.equals(p1,p2) then () else
(*DEBUG*)
()(*
		raise Mismatch(ManifestVal(l, SOME p1, p2))
*)

    and matchTypDef(l, _,    NONE)	= ()
      | matchTypDef(l, NONE, SOME t2)	= raise Mismatch(ManifestTyp(l,NONE,t2))
      | matchTypDef(l, SOME t1, SOME t2) =
	    if Type.equals(t1,t2) then () else
		raise Mismatch(ManifestTyp(l, SOME t1, t2))

    and matchModDef(l, _,    NONE)	= ()
      | matchModDef(l, NONE, SOME p2)	= raise Mismatch(ManifestMod(l,NONE,p2))
      | matchModDef(l, SOME p1, SOME p2) =
	    if Path.equals(p1,p2) then () else
(*DEBUG*)
()(*
		raise Mismatch(ManifestMod(l, SOME p1, p2))
*)

    and matchInfDef(l, _,    NONE)	= ()
      | matchInfDef(l, NONE, SOME j2)	= raise Mismatch(ManifestInf(l,NONE))
      | matchInfDef(l, SOME j1, SOME j2) =
	    equalise(j1,j2) handle Mismatch mismatch =>
		raise Mismatch(ManifestInf(l, SOME mismatch))


    and equalise(j1,j2) = (*UNFINISHED*) ()
    and equaliseKind(k1,k2) = (*UNFINISHED*) ()



  (* Intersection *)

    fun intersect(j1,j2) =
	let
	    val j1' = clone j1
	    val j2' = clone j2
	    val rea = emptyRea()
	in
	    intersect'(rea, j1', j2')
	end

    and intersect'(rea, j1, ref TOP) = j1
      | intersect'(rea, ref TOP, j2) = j2
      | intersect'(rea, j1 as ref(CON(_,p1)), j2 as ref(CON(_,p2))) =
	if Path.equals(p1,p2) then
	    j1
	else
	    raise Mismatch(Incompatible(j1,j2))

      | intersect'(rea, j1 as ref(SIG s1), ref(SIG s2)) =
	    ( intersectSig(rea, s1, s2) ; j1 )

      | intersect'(rea, ref(FUN(p1,j11,j12)), ref(FUN(p2,j21,j22))) =
	(*UNFINISHED*)
	    raise Crash.Crash "Inf.intersect: FUN"

      | intersect'(rea, ref(LAMBDA(p1,j11,j12)), ref(LAMBDA(p2,j21,j22))) =
	(*UNFINISHED*)
	    raise Crash.Crash "Inf.intersect: LAMBDA"

      | intersect'(rea, j1 as ref(APPLY(j11,p1,j12)), ref(APPLY(j21,p2,j22))) =
	(*UNFINISHED*)
	    raise Crash.Crash "Inf.intersect: APPLY"

      | intersect'(rea, ref(LINK j1), j2)	= intersect'(rea, j1, j2)
      | intersect'(rea, j1, ref(LINK j2))	= intersect'(rea, j1, j2)

      | intersect'(rea, j1 as ref(ABBREV(j11,j12)), j2)	=
	(*UNFINISHED: need some node for intersection... *)
	( intersect'(rea, j12, j2)
	; j1 := ABBREV(j11,j2)
	; j1
	)
      | intersect'(rea, j1, j2 as ref(ABBREV(j21,j22))) =
	(*UNFINISHED: need some node for intersection... *)
	( intersect'(rea, j1, j22)
	; j2 := ABBREV(j21,j1)
	; j2
	)
      | intersect'(rea, j1,j2) = raise Mismatch(Incompatible(j1,j2))


    (* UNFINISHED: does ignore dependencies on second argument signature *)

    and intersectSig(rea, s1 as (itemsr1 as ref items1, m1),
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
	      | pair1(b, FIX(x1,q1), FIX(x2,q2)) =
		    true
	      | pair1 _ =
		    raise Crash.Crash "Inf.intersectSig: pairing"

	    and pair(m1, [], pairs, left) = ( List.rev pairs, List.rev left )
	      | pair(m1, item2::items, pairs, left) =
		case Map.lookup(m1, (itemSpace item2, itemLab item2))
		  of NONE => pair(m1, items, pairs, item2::left)
		   | SOME [] => raise Crash.Crash "Inf.intersectSig: lookup"
		   | SOME(item1::_) =>
		     (* Nested structures are already realised.
		      * We would loop during realisation if we inserted
		      * identity realisations. *)
		     ( if pair1(not(Path.equals(itemPath item1,itemPath item2)),
				!item1, !item2) then () else
			  Misc.General_swap(item1, item2)
		     ; pair(m1, items, (item1,item2)::pairs, left)
		     )

	    (* Necessary to create complete realisation. *)
	    and pairNested(ref(SIG(_,m1)), ref(SIG(ref items2,_))) =
		    ignore(pair(m1, items2, [], []))
	      | pairNested(ref(FUN _), ref(FUN _)) =
		(*UNFINISHED: when introducing functor paths*) ()
	      | pairNested(ref(LINK j1), j2) = pairNested(j1, j2)
	      | pairNested(j1, ref(LINK j2)) = pairNested(j1, j2)
	      | pairNested(ref(ABBREV(_,j1)), j2) = pairNested(j1, j2)
	      | pairNested(j1, ref(ABBREV(_,j2))) = pairNested(j1, j2)
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
      | intersectItem'(rea, FIX(x1,q1), FIX(x2,q2)) =
	let
	    val l = idLab x1
	    val q = intersectFix(l, q1, q2)
	in
	    FIX(x1,q)
	end
      | intersectItem' _ = raise Crash.Crash "Inf.intersectItem"

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

    and intersectFix(l,q1,q2) =
	if q1 = q2 then q1 else
	    raise Mismatch(MismatchFix(l,q1,q2))

    and intersectValSort(l,w1,VALUE) = w1
      | intersectValSort(l,VALUE,w2) = w2
      | intersectValSort(l,w1,w2) =
	if w1 = w2 then w1 else
	    raise Mismatch(MismatchValSort(l,w1,w2))

    and intersectTypSort(l,w1,w2) =
	if w1 = OPEN orelse w2 = OPEN then
	    OPEN
	else
	    CLOSED


    and intersectValDef(l, NONE,    NONE)	= NONE
      | intersectValDef(l, SOME p1, NONE)	= SOME p1
      | intersectValDef(l, NONE,    SOME p2)	= SOME p2
      | intersectValDef(l, SOME p1, SOME p2)	=
	    if Path.equals(p1,p2) then SOME p1 else
		raise Mismatch(ManifestVal(l, SOME p1, p2))

    and intersectTypDef(l, NONE,    NONE)	= NONE
      | intersectTypDef(l, SOME t1, NONE)	= SOME t1
      | intersectTypDef(l, NONE,    SOME t2)	= SOME t2
      | intersectTypDef(l, SOME t1, SOME t2)	=
	    if Type.equals(t1,t2) then SOME t1 else
		raise Mismatch(ManifestTyp(l, SOME t1, t2))

    and intersectModDef(l, NONE,    NONE)	= NONE
      | intersectModDef(l, SOME p1, NONE)	= SOME p1
      | intersectModDef(l, NONE,    SOME p2)	= SOME p2
      | intersectModDef(l, SOME p1, SOME p2)	=
	    if Path.equals(p1,p2) then SOME p1 else
		raise Mismatch(ManifestMod(l, SOME p1, p2))

    and intersectInfDef(l, NONE,    NONE)	= NONE
      | intersectInfDef(l, SOME j1, NONE)	= SOME j1
      | intersectInfDef(l, NONE,    SOME j2)	= SOME j2
      | intersectInfDef(l, SOME j1, SOME j2)	=
	    ( equalise(j1,j2) ; SOME j1 ) handle Mismatch mismatch =>
		raise Mismatch(ManifestInf(l, SOME mismatch))
  end


structure Inf : INF = InfPrivate
