structure PPType :> PP_TYPE =
  struct

    (* Import *)

    open TypePrivate
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Helpers *)

    fun uncurry(ref(APP(t1,t2)))= let val (t,ts) = uncurry t1 in (t,ts@[t2]) end
      | uncurry t		= (t,[])

    fun makeVar(t as ref t') =
	let
	    val k = kindVar t
	    val x = Stamp.new()
	    val n = "'" ^ Stamp.toString x
	    val c = (k, CLOSED, Path.PLAIN(x, Name.ExId n))
	    val _ = t := CON c
	in
	    t'
	end


    (* Simple objects *)

    fun ppLab l			= text(Lab.toString l)
    fun ppName n		= text(Name.toString n)

    fun ppPath(Path.PLAIN(_,n))	= ppName n
      | ppPath(Path.DOT(p,l))	= ppPath p ^^ text "." ^^ ppLab l

    fun ppCon (k,_,p)		= ppPath p


    (* Kinds *)

    (* Precedence:
     *	0 : arrow (ty1 -> ty2)
     *	1 : star
     *)

    fun ppKind k = fbox(below(ppKindPrec 0 k))

    and ppKindPrec p  STAR		= text "*"
      | ppKindPrec p (ARROW(k1,k2))	= 
	let
	    val doc = ppKindPrec 1 k1 ^/^ text "->" ^/^ ppKindPrec 0 k2
	in
	    if p > 0 then
		paren doc
	    else
		doc
	end


    (* Types *)

    (* Precedence:
     *  0 : sums (con of ty1 | ... | con of tyn)
     *	1 : function arrow (ty1 -> ty2) and binders
     *	2 : tuple (ty1 * ... * tyn)
     *	3 : constructed type (tyseq tycon)
     *)

    fun ppType t = fbox(below(ppTypePrec 0 t))

    and (*ppTypePrec p (t as ref(REC t1)) =
	if occurs(t,t1) then
	    let
		val doc = text "MU" ^/^ ppBinder(t,t1)
	    in
		if p > 1 then
		    paren doc
		else
		    fbox(below(nest doc))
	    end
	else
	    ppTypePrec p t1

      | ppTypePrec p (t as ref(MARK(REC t1))) =
	if occurs(t,t1) then
	    let
		val doc = text "!MU" ^/^ ppBinder(t,t)
	    in
		if p > 1 then
		    paren doc
		else
		    fbox(below(nest doc))
	    end
	else
	    ppTypePrec p t1

(*DEBUG
      | ppTypePrec  p (ref t') = ppTypePrec' p t'
*)    |*) ppTypePrec  p (t as ref t') =
let
val _=print("pping " ^ pr t' ^ "\n")
in
	if foldl1'(t', fn(t1,b) => b orelse occurs(t,t1), false) then
	    let
		val a'  = makeVar t
		val doc = text "MU" ^/^
			    abox(
				hbox(
				    ppType t ^/^
				    text "."
				) ^^
				below(break ^^
				    ppTypePrec' 1 t'
				)
			    )
		val _   = t := a'
	    in
		if p > 1 then
		    paren doc
		else
		    fbox(below(nest doc))
	    end
	else
	    ppTypePrec' p t'
end

    and ppTypePrec' p (HOLE(k,n)) =
	if k = STAR then
	    text "'_?"
	else
	    paren (text "'_?" ^/^ text ":" ^/^ ppKind k)

      | ppTypePrec' p (LINK t) =
	    ppTypePrec p t

      | ppTypePrec' p (MARK t') =
	    text "!" ^^ ppTypePrec' p t'

      | ppTypePrec' p (ARR(t1,t2)) =
	let
	    val doc = ppTypePrec 2 t1 ^/^ text "->" ^/^ ppTypePrec 1 t2
	in
	    if p > 1 then
		paren doc
	    else
		doc
	end

      | ppTypePrec' p (TUP []) =
	    text "unit"

      | ppTypePrec' p (TUP ts) =
	let
	    val doc = ppStarList (ppTypePrec 3) ts
	in
	    if p > 2 then
		paren doc
	    else
		fbox(below(nest doc))
	end

      | ppTypePrec' p (ROW r) =
	    brace(break ^^ fbox(below(ppRow r)) ^^ break)

      | ppTypePrec' p (SUM r) =
	    paren(fbox(below(ppSum r)))

      | ppTypePrec' p (VAR(k,n)) =
	if k = STAR then
	    text "'?"
	else
	    paren (text "'?" ^/^ text ":" ^/^ ppKind k)

      | ppTypePrec' p (CON c) =
	    ppCon c

      | ppTypePrec' p (ALL(a,t)) =
	let
	    val doc = text "ALL" ^/^ ppBinder(a,t)
	in
	    if p > 1 then
		paren doc
	    else
		fbox(below doc)
	end

      | ppTypePrec' p (EX(a,t)) =
	let
	    val doc = text "EX" ^/^ ppBinder(a,t)
	in
	    if p > 1 then
		paren doc
	    else
		fbox(below doc)
	end

      | ppTypePrec' p (LAM(a,t)) =
	let
	    val doc = text "FN" ^/^ ppBinder(a,t)
	in
	    if p > 0 then
		paren doc
	    else
		fbox(below doc)
	end

      | ppTypePrec' p (t' as APP _) =
	let
	    val (t,ts) = uncurry(ref t')
	in
	    fbox(nest(ppSeqPrec ppTypePrec 3 ts ^/^ ppTypePrec 4 t))
	end

(*DEBUG
      | ppTypePrec' p (REC t) = Crash.crash "PPType.ppType: bypassed REC"
*)      | ppTypePrec' p (REC t) = ppTypePrec p t


    and ppRow NIL		= empty
      | ppRow RHO		= text "..."
      | ppRow(FLD(l,ts,NIL))	= ppField(l,ts)
      | ppRow(FLD(l,ts,r))	= ppField(l,ts) ^^ text "," ^/^ ppSum r

    and ppSum NIL		= empty
      | ppSum RHO		= text "..."
      | ppSum(FLD(l,ts,NIL))	= ppField(l,ts)
      | ppSum(FLD(l,ts,r))	= ppField(l,ts) ^/^ text "|" ^/^ ppSum r

    and ppField(l,[]) = ppLab l
      | ppField(l,ts) =
	    abox(
		hbox(
		    ppLab l ^/^
		    text ":"
		) ^^
		below(break ^^
		    ppCommaList ppType ts
		)
	    )

    and ppBinder(a,t) =
	let
	    val a' = makeVar a
	in
	    abox(
		hbox(
		    ppType a ^/^
(*DEBUG*)
text"(" ^^ ppTypePrec' 1 a' ^^ text")" ^/^
		    text "."
		) ^^
		below(break ^^
		    ppType t
		)
	    )
	    before a := a'
	end

  end
