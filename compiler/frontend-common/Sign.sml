(* UNFINISHED: How should we handle mutually recursice items? *)

(* Signatures contain state. This means that they must be instantiated (cloned)
   at each occurance. *)

structure SignPrivate =
  struct

  (* Types *)

    type lab   = Lab.t
    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type kind  = Type.kind

    type id    = lab * int * stamp

    type 'a def = 'a option

    datatype ('inf,'sort) item' =
	  VAL of id * typ * bool * path def	(* value or constructor *)
	| TYP of id * kind * typ def		(* type *)
	| MOD of id * 'inf * path def		(* module *)
	| INF of id * 'sort * 'inf def		(* interface *)

    datatype dom = VAL' | CON' | TYP' | MOD' | INF'

    structure Map = MakeHashImpMap(struct type t = dom * lab
					  fun hash(_,l) = Lab.hash l end)

    type ('inf,'sort) item = ('inf,'sort) item' ref
    type ('inf,'sort) sign = ('inf,'sort) item list ref
			   * ('inf,'sort) item list Map.t	(* [sigma,s] *)
    type ('inf,'sort) t    = ('inf,'sort) sign

    (* Invariants for sign:
     * - each item in the map must appear in the list, and vice versa
     * - the list contains all items of the signature in reversed order
     * - each list in the range of the map contains all items with the same
     *   label and sort, also in reversed order (so the visible one comes first)
     *)


  (* Substitutions and realisations *)

    type subst = Path.subst

    type val_rea      = path PathMap.t
    type typ_rea      = typ  PathMap.t
    type mod_rea      = path PathMap.t
    type 'inf inf_rea = 'inf PathMap.t
    type 'inf rea     = val_rea * typ_rea * mod_rea * 'inf inf_rea


  (* Accessors *)

    fun labOfId(l,n,z)		= l
    fun indexOfId(l,n,z)	= n
    fun stampOfId(l,n,z)	= z

    fun id(ref item')		= id' item'
    and id'(VAL(id,_,_,_))	= id
      | id'(TYP(id,_,_))	= id
      | id'(MOD(id,_,_))	= id
      | id'(INF(id,_,_))	= id

    fun index item		= indexOfId(id item)

    fun labToName l		= Name.ExId(Lab.toString l)


  (* Construction *)

    fun empty() = (ref [], Map.new())

    fun extend((itemsr,map), dom, l, makeItem') =
	let
	    val z    = Stamp.new()
	    val id   = ref(l,1,z)
	    val item = ref(makeItem'(!id))

	    val _ = itemsr := item :: !itemsr
	    val _ = Map.insertWith
			(fn(items,_) =>
			    let val id'   = (l, index(List.hd items) + 1, z)
			        val item' = makeItem' id'
			    in  id := id' ; item := item' ; item::items end
			) (map, (dom,l), [item])
	in
	    !id
	end

    fun extendVal(s,l,t,b,d) = extend(s, VAL', l, fn x => VAL(x,t,b,d))
    fun extendTyp(s,l,k,d)   = extend(s, TYP', l, fn x => TYP(x,k,d))
    fun extendMod(s,l,j,d)   = extend(s, MOD', l, fn x => MOD(x,j,d))
    fun extendInf(s,l,k,d)   = extend(s, INF', l, fn x => INF(x,k,d))


  (* Lookup *)

    fun selectVal(VAL(x, t, b, d))	= t
      | selectVal _			= raise Crash.Crash "Sign.selectVal"

    fun selectTyp(TYP(x, k, SOME t))	= t
      | selectTyp _			= raise Crash.Crash "Sign.selectTyp"

    fun selectMod(MOD(x, j, d))		= j
      | selectMod _			= raise Crash.Crash "Sign.selectMod"

    fun selectInf(INF(x, s, SOME j))	= j
      | selectInf _			= raise Crash.Crash "Sign.selectInf"


    fun lookup dom ((_,m), l) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) => !item
	   | _                 => raise Crash.Crash "Sign.lookup"

    fun lookup' dom ((_,m), l, n) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) => !(Option.valOf(
				   List.find (fn item => index item = n) items))
	   | _                 => raise Crash.Crash "Sign.lookup'"


    fun lookupVal args  = (selectVal o lookup VAL') args
    fun lookupTyp args  = (selectTyp o lookup TYP') args
    fun lookupMod args  = (selectMod o lookup MOD') args
    fun lookupInf args  = (selectInf o lookup INF') args

    fun lookupVal' args = (selectVal o lookup' VAL') args
    fun lookupTyp' args = (selectTyp o lookup' TYP') args
    fun lookupMod' args = (selectMod o lookup' MOD') args
    fun lookupInf' args = (selectInf o lookup' INF') args


  (* Substitution *)

    fun substitute (substituteInf, substituteSort) (subst, (ref items, _)) =
	let
	    fun substituteItem(item as ref(VAL(x, t, b, d))) =
		let
		    val _  = Type.substitute(subst, t)
		    val d' = substitutePathDef d
		in
		    item := VAL(x, t, b, d')
		end

	      | substituteItem(item as ref(TYP(x, k, d))) =
		let
		    val _ = substituteTypDef d
		in
		    item := TYP(x, k, d)
		end

	      | substituteItem(item as ref(MOD(x, j, d))) =
		let
		    val _  = substituteInf(subst, j)
		    val d' = substitutePathDef d
		in
		    item := MOD(x, j, d')
		end

	      | substituteItem(item as ref(INF(x, s, d))) =
		let
		    val _ = substituteSort(subst, s)
		    val _ = substituteInfDef d
		in
		    item := INF(x, s, d)
		end

	    and substitutePathDef NONE    = NONE
	      | substitutePathDef(SOME p) = SOME(Path.substitute(subst, p))

	    and substituteTypDef NONE     = ()
	      | substituteTypDef(SOME t)  = Type.substitute(subst, t)

	    and substituteInfDef NONE     = ()
	      | substituteInfDef(SOME j)  = substituteInf(subst, j)
	in
	    List.app substituteItem items
	end


  (* Strengthening *)

    fun strengthen (strengthenInf, substituteInf, substituteSort, infCon)
		   (subst, p, (ref items, _)) =
	let
	    fun extendSubst(z, l, n) =
		let val p' = Path.DOT(p,l,n) in
		    StampMap.insert(subst, z, p') ; p'
		end

	    fun strengthenItem(item as ref(VAL(x as (l,n,z), t, b, d))) =
		let
		    val p' = extendSubst(z, l, n)
		    val _  = Type.substitute(subst, t)
		    val d' = strengthenPathDef(p', d)
		in
		    item := VAL(x, t, b, d')
		end

	      | strengthenItem(item as ref(TYP(x as (l,n,z), k, d))) =
		let
		    val p' = extendSubst(z, l, n)
		    val d' = strengthenTypDef(p', k, d)
		in
		    item := TYP(x, k, d')
		end

	      | strengthenItem(item as ref(MOD(x as (l,n,z), j, d))) =
		let
		    val p' = extendSubst(z, l, n)
		    val _  = substituteInf(subst, j)
		    val d' = strengthenPathDef(p', d)
		in
		    item := MOD(x, j, d')
		end

	      | strengthenItem(item as ref(INF(x as (l,n,z), s, d))) =
		let
		    val p' = extendSubst(z, l, n)
		    val _  = substituteSort(subst, s)
		    val d' = strengthenInfDef(p', s, d)
		in
		    item := INF(x, s, d')
		end

	    and strengthenPathDef(p', NONE)   = SOME p'
	      | strengthenPathDef(p', SOME p) = SOME(Path.substitute(subst, p))

	    and strengthenTypDef(p', k, NONE) =
		    SOME(Type.inCon(k, Type.CLOSED, p'))
	      | strengthenTypDef(p', k, d as SOME t) =
		    ( Type.substitute(subst, t) ; d )

	    and strengthenInfDef(p', s, NONE) = SOME(infCon(s, p'))
	      | strengthenInfDef(p', s, d as SOME j) =
		    ( substituteInf(subst, j) ; d )
	in
	    List.app strengthenItem items
	end


  (* Instantiation *)

    fun instantiate (instantiateInf, instantiateSort) (subst, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSubst(l, n, z) =
		let val z' = Stamp.new() in
		    StampMap.insert(subst, z, Path.PLAIN(z',l,n)) ; z'
		end

	    fun extendSig(doml, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, doml, [item])
		)

	    fun instantiateTyp t =
		let val t2 = Type.clone t in
		    Type.substitute(subst,t2) ; t2
		end

	    fun instantiateItem(ref(VAL((l,n,z), t, b, d))) =
		let
		    val z'   = extendSubst(l, n, z)
		    val t'   = instantiateTyp t
		    val d'   = instantiatePathDef d
		    val item = ref(VAL((l,n,z'), t', b, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | instantiateItem(ref(TYP((l,n,z), k, d))) =
		let
		    val z'   = extendSubst(l, n, z)
		    val d'   = instantiateTypDef d
		    val item = ref(TYP((l,n,z'), k, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | instantiateItem(ref(MOD((l,n,z), j, d))) =
		let
		    val z'   = extendSubst(l, n, z)
		    val j'   = instantiateInf(subst, j)
		    val d'   = instantiatePathDef d
		    val item = ref(MOD((l,n,z'), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | instantiateItem(ref(INF((l,n,z), s, d))) =
		let
		    val z'   = extendSubst(l, n, z)
		    val s'   = instantiateSort(subst, s)
		    val d'   = instantiateInfDef d
		    val item = ref(INF((l,n,z'), s', d'))
		in
		    extendSig((INF',l), item)
		end

	    and instantiatePathDef NONE    = NONE
	      | instantiatePathDef(SOME p) = SOME(Path.substitute(subst, p))

	    and instantiateTypDef NONE     = NONE
	      | instantiateTypDef(SOME t)  = SOME(instantiateTyp t)

	    and instantiateInfDef NONE     = NONE
	      | instantiateInfDef(SOME j)  = SOME(instantiateInf(subst, j))

	    val _ = Misc.List_appr instantiateItem items
	in
	    s
	end

  end


structure Sign :> SIGN = SignPrivate
