(* UNFINISHED: How should we handle mutually recursice items? *)

(* Signatures contain state. This means that they must be cloned
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

    type id    = path * lab * int

    type 'a def = 'a option

    datatype ('inf,'kind) item' =
	  VAL of id * typ * bool * path def	(* value or constructor *)
	| TYP of id * kind * typ def		(* type *)
	| MOD of id * 'inf * path def		(* module *)
	| INF of id * 'kind * 'inf def		(* interface *)

    datatype dom = VAL' | CON' | TYP' | MOD' | INF'

    structure Map = MakeHashImpMap(struct type t = dom * lab
					  fun hash(_,l) = Lab.hash l end)

    type ('inf,'kind) item = ('inf,'kind) item' ref
    type ('inf,'kind) sign = ('inf,'kind) item list ref
			   * ('inf,'kind) item list Map.t	(* [sigma,s] *)
    type ('inf,'kind) t    = ('inf,'kind) sign

    (* Invariants for sign:
     * - each item in the map must appear in the list, and vice versa
     * - the list contains all items of the signature in reversed order
     * - each list in the range of the map contains all items with the same
     *   label and sort, also in reversed order (so the visible one comes first)
     *)


  (* Realisations *)

    type rea		 = path PathMap.t

    type val_rea	 = path PathMap.t
    type typ_rea	 = typ  PathMap.t
    type ('i,'k) mod_rea = ('i,'k) sign PathMap.t
    type ('i,'k) inf_rea = 'i PathMap.t
    type ('i,'k) rea'	 = val_rea * typ_rea * ('i,'k) mod_rea * ('i,'k) inf_rea


  (* Accessors *)

    fun pathOfId(p,l,n)		= p
    fun labOfId(p,l,n)		= l
    fun indexOfId(p,l,n)	= n

    fun id(ref item')		= id' item'
    and id'(VAL(id,_,_,_))	= id
      | id'(TYP(id,_,_))	= id
      | id'(MOD(id,_,_))	= id
      | id'(INF(id,_,_))	= id

    fun path  item		= pathOfId(id item)
    fun lab   item		= labOfId(id item)
    fun index item		= indexOfId(id item)


  (* Construction *)

    fun empty() = (ref [], Map.new())

    fun newItem(s, l)		= Path.fromLab l
    val newVal			= newItem
    val newTyp			= newItem
    val newMod			= newItem
    val newInf			= newItem

    fun hideId (p,l,n)		= (p,l,n+1)
    fun hide item		= item := hide'(!item)
    and hide'(VAL(id,t,b,d))	= VAL(hideId id, t, b, d)
      | hide'(TYP(id,k,d))	= TYP(hideId id, k, d)
      | hide'(MOD(id,j,d))	= MOD(hideId id, j, d)
      | hide'(INF(id,k,d))	= INF(hideId id, k, d)

    fun extend((itemsr,map), dom, p, makeItem') =
	let
	    val l    = Path.toLab p
(*DEBUG*)
val _=print("-- extending signature with `" ^ Lab.toString l ^ "'\n");
	    val item = ref(makeItem'(p,l,0))

	    val _ = itemsr := item :: !itemsr
	    val _ = Map.insertWith (fn(items,_) =>
					( List.app hide items ; item::items )
				   ) (map, (dom,l), [item])
	in
	    p
	end

    fun extendVal(s,p,t,b,d)	= extend(s, VAL', p, fn x => VAL(x,t,b,d))
    fun extendTyp(s,p,k,d)	= extend(s, TYP', p, fn x => TYP(x,k,d))
    fun extendMod(s,p,j,d)	= extend(s, MOD', p, fn x => MOD(x,j,d))
    fun extendInf(s,p,k,d)	= extend(s, INF', p, fn x => INF(x,k,d))


  (* Lookup *)

    fun selectVal(VAL(x, t, b, d))	= t
      | selectVal _			= raise Crash.Crash "Sign.selectVal"

    fun selectTyp(TYP(x, k, SOME t))	= t
      | selectTyp _			= raise Crash.Crash "Sign.selectTyp"

    fun selectMod(MOD(x, j, d))		= j
      | selectMod _			= raise Crash.Crash "Sign.selectMod"

    fun selectInf(INF(x, k, SOME j))	= j
      | selectInf _			= raise Crash.Crash "Sign.selectInf"


    fun lookup dom ((_,m), l) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) => !item
	   | _                 =>
(*DEBUG*)
(print("Lookup failed for `" ^ Lab.toString l ^ "'\n");
 raise Crash.Crash "Sign.lookup"
)
    fun lookup' dom ((_,m), l, n) =
	case Map.lookup(m, (dom,l))
	  of SOME(item::items) => !(Option.valOf(
				   List.find (fn item => index item = n) items))
	   | _                 =>
(*DEBUG*)
(print("Lookup failed for `" ^ Lab.toString l ^ "'\n");
 raise Crash.Crash "Sign.lookup'"
)


    fun lookupVal args  = (selectVal o lookup VAL') args
    fun lookupTyp args  = (selectTyp o lookup TYP') args
    fun lookupMod args  = (selectMod o lookup MOD') args
    fun lookupInf args  = (selectInf o lookup INF') args

    fun lookupVal' args = (selectVal o lookup' VAL') args
    fun lookupTyp' args = (selectTyp o lookup' TYP') args
    fun lookupMod' args = (selectMod o lookup' MOD') args
    fun lookupInf' args = (selectInf o lookup' INF') args


  (* Realisation *)

    fun realise (realiseInf,realiseKind) (rea, (ref items, _)) =
	let
	    fun realiseItem(item as ref(VAL(x, t, b, d))) =
		( Type.realise(rea, t) ; item := VAL(x,t,b, realisePathDef d))
	      | realiseItem(ref(TYP(x, k, d))) =
		  realiseTypDef d
	      | realiseItem(item as ref(MOD(x, j, d))) =
		( realiseInf(rea, j) ; item := MOD(x, j, realisePathDef d) )
	      | realiseItem(ref(INF(x, k, d))) =
		( realiseKind(rea, k) ; realiseInfDef d )

	    and realisePathDef      NONE    = NONE
	      | realisePathDef(d as SOME p) =
		case PathMap.lookup(rea, p)
		  of NONE => d
		   | d'   => d'

	    and realiseTypDef NONE    = ()
	      | realiseTypDef(SOME t) = Type.realise(rea, t)

	    and realiseInfDef NONE    = ()
	      | realiseInfDef(SOME j) = realiseInf(rea, j)
	in
	    List.app realiseItem items
	end


  (* Strengthening *)

    fun strengthen infCon (p, (ref items, _)) =
	let
	    fun strengthenId(p',l,n) = Path.substitute(p', p, l, n)

	    fun strengthenItem(item as ref(VAL(x, t, b, d))) =
		let
		    val _  = strengthenId x
		    val d' = strengthenPathDef(pathOfId x, d)
		in
		    item := VAL(x, t, b, d')
		end

	      | strengthenItem(item as ref(TYP(x, k, d))) =
		let
		    val _  = strengthenId x
		    val d' = strengthenTypDef(pathOfId x, k, d)
		in
		    item := TYP(x, k, d')
		end

	      | strengthenItem(item as ref(MOD(x, j, d))) =
		let
		    val _  = strengthenId x
		    val d' = strengthenPathDef(pathOfId x, d)
		in
		    item := MOD(x, j, d')
		end

	      | strengthenItem(item as ref(INF(x, k, d))) =
		let
		    val _  = strengthenId x
		    val d' = strengthenInfDef(pathOfId x, k, d)
		in
		    item := INF(x, k, d')
		end

	    and strengthenPathDef(p', NONE)   = SOME p'
	      | strengthenPathDef(p', d)      = d

	    and strengthenTypDef(p', k, NONE) = SOME(Type.inCon
							(k, Type.CLOSED, p'))
	      | strengthenTypDef(p', k, d)    = d

	    and strengthenInfDef(p', k, NONE) = SOME(infCon(k, p'))
	      | strengthenInfDef(p', k, d)    = d
	in
	    List.app strengthenItem items
	end


  (* Cloning *)

    fun clone (cloneInf, cloneKind) (rea, (ref items,_)) =
	let
	    val s as (itemsr,map) = empty()

	    fun extendSig(doml, item) =
		( itemsr := item :: !itemsr
		; Map.insertWith (fn(l1,l2) => l2 @ l1) (map, doml, [item])
		)

	    fun clonePath p =
		let val p' = Path.cloneBinder PathMap.lookup (rea, p) in
		    if p' <> p then PathMap.insert(rea, p, p') else () ; p'
		end

	    fun cloneTyp t =
		let val t2 = Type.clone t in
		    Type.realise(rea,t2) ; t2
		end

	    fun cloneItem(ref(VAL((p,l,n), t, b, d))) =
		let
		    val p'   = clonePath p
		    val t'   = cloneTyp t
		    val d'   = clonePathDef d
		    val item = ref(VAL((p',l,n), t', b, d'))
		in
		    extendSig((VAL',l), item)
		end

	      | cloneItem(ref(TYP((p,l,n), k, d))) =
		let
		    val p'   = clonePath p
		    val d'   = cloneTypDef d
		    val item = ref(TYP((p',l,n), k, d'))
		in
		    extendSig((TYP',l), item)
		end

	      | cloneItem(ref(MOD((p,l,n), j, d))) =
		let
		    val p'   = clonePath p
		    val j'   = cloneInf(rea, j)
		    val d'   = clonePathDef d
		    val item = ref(MOD((p',l,n), j', d'))
		in
		    extendSig((MOD',l), item)
		end

	      | cloneItem(ref(INF((p,l,n), k, d))) =
		let
		    val p'   = clonePath p
		    val k'   = cloneKind(rea, k)
		    val d'   = cloneInfDef d
		    val item = ref(INF((p',l,n), k', d'))
		in
		    extendSig((INF',l), item)
		end

	    and clonePathDef      NONE    = NONE
	      | clonePathDef(d as SOME p) =
		case PathMap.lookup(rea, p)
		  of NONE => d
		   | d'   => d'

	    and cloneTypDef NONE     = NONE
	      | cloneTypDef(SOME t)  = SOME(cloneTyp t)

	    and cloneInfDef NONE     = NONE
	      | cloneInfDef(SOME j)  = SOME(cloneInf(rea, j))

	    val _ = Misc.List_appr cloneItem items
	in
	    s
	end

  end


structure Sign :> SIGN = SignPrivate
