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

    fun parenPrec p (p',doc) =
	if p > p' then
	    paren doc
	else
	    doc


    (* Simple objects *)

    fun ppLab l		= text(Lab.toString l)
    fun ppCon (k,_,p)	= PPPath.ppPath p

    fun varToString(isBound, n) =
	let
	    fun rep(0,c) = c
	      | rep(n,c) = c ^ rep(n-1,c)

	    val c = String.str(Char.chr(Char.ord #"a" + n mod 26))
	in
	    (if isBound then "'" else "'_") ^ rep(n div 26, c)
	end


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
	    parenPrec p (0, doc)
	end


    (* Types *)

    (* Precedence:
     *  0 : sums (con of ty1 | ... | con of tyn), kind annotation (ty : kind)
     *	1 : binders (LAM ty1 . ty2)
     *	2 : function arrow (ty1 -> ty2)
     *	3 : tuple (ty1 * ... * tyn)
     *	4 : constructed type (tyseq tycon)
     *)

    fun ppType t =
	let
	    val trail = ref []
	    val a     = ref 0

	    fun makeVar(isBound, t as ref t') =
		let
		    val k = kindVar t
		    val s = varToString(isBound, !a before a := !a+1)
		    val c = (k, CLOSED, Path.fromLab(Lab.fromString s))
		    val _ = t := CON c
		    val _ = if isBound then () else trail := (t,t')::(!trail)
		in
		    t'
		end

	    fun ppType t = fbox(below(ppTypePrec 0 t))

	    and ppTypePrec p (t as ref(HOLE(k,n))) =
		let
		    val t'  = makeVar(false, t)
		    val doc = ppTypePrec' p (!t)
		in
		    if k = STAR then
			doc
(*DEBUG*)
^^ text("_" ^ Int.toString n)
		    else
			parenPrec p (0, doc ^/^ text ":" ^/^ ppKind k)

		end

	      | ppTypePrec p (t as ref(REC t1 | MARK(REC t1))) =
(*DEBUG*)
((*print("[pp " ^ pr(!t) ^ "]");*)
		if occurs(t,t1) then
		    let
(*val _=print"recursive\n"
*)			val t'  = makeVar(true, t)
			val doc = (case t' of MARK _ => text "!MU"
					    | _      => text "MU") ^/^
				  abox(
					hbox(
					    ppType t ^/^
					    text "."
					) ^^
					below(break ^^
					    ppTypePrec 1 t1
					)
				  )
			val _   = t := t'
		    in
			parenPrec p (1, fbox(below(nest(doc))))
		    end
		else
(*(print"not recursive\n";*)
		    ppTypePrec p t1
)

	      | ppTypePrec p (t as ref(APP _)) =
	        ( reduce t ;
(*print("[pp APP]");*)
		  if isApp t then ppTypePrec' p (!t)
			     else ppTypePrec p t
		)

	      | ppTypePrec  p (ref t') = ppTypePrec' p t'
(*(*DEBUG*)
	      | ppTypePrec p (t as ref t') =
let
val _=print("[pp " ^ pr t' ^ "]")
(*val _=TextIO.inputLine TextIO.stdIn*)
in
	if foldl1'(t', fn(t1,b) => b orelse occursIllegally(t,t1), false) then
		    let
(*DEBUG*)
val _=print"RECURSIVE!\n"
			val a'  = makeVar(true, t)
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
			parenPrec p (1, fbox(below(nest(doc))))
		    end
		else
		    ppTypePrec' p t'
end
*)

	    and ppTypePrec' p (LINK t) =
(*DEBUG
text "@" ^^*)
		    ppTypePrec p t

	      | ppTypePrec' p (MARK t') =
		    text "!" ^^ ppTypePrec' p t'

	      | ppTypePrec' p (ARR(t1,t2)) =
		let
		    val doc = ppTypePrec 3 t1 ^/^ text "->" ^/^ ppTypePrec 2 t2
		in
		    parenPrec p (2, doc)
		end

	      | ppTypePrec' p (TUP [] | ROW NIL) =
		    text "unit"

	      | ppTypePrec' p (TUP ts) =
		let
		    val doc = ppStarList (ppTypePrec 4) ts
		in
		    parenPrec p (3, fbox(below(nest doc)))
		end

	      | ppTypePrec' p (ROW r) =
		    brace(fbox(below(ppRow r)))

	      | ppTypePrec' p (SUM r) =
		    paren(fbox(below(ppSum r)))

	      | ppTypePrec' p (VAR(k,n)) =
		if k = STAR then
		    text "'?"
(*DEBUG*)
^^ text("[" ^ Int.toString n ^ "]")
		else
		    paren (text "'?" ^/^ text ":" ^/^ ppKind k)

	      | ppTypePrec' p (CON c) =
		    ppCon c

	      | ppTypePrec' p (ALL(a,t)) =
		let
		    val doc = text "ALL" ^/^ ppBinder(a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypePrec' p (EX(a,t)) =
		let
		    val doc = text "EX" ^/^ ppBinder(a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypePrec' p (LAM(a,t)) =
		let
		    val doc = text "FN" ^/^ ppBinder(a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypePrec' p (t' as APP _) =
		let
		    val (t,ts) = uncurry(ref t')
		in
		    fbox(nest(ppSeqPrec ppTypePrec 4 ts ^/^ ppTypePrec 5 t))
		end

	      | ppTypePrec' p (HOLE _) =
		    Crash.crash "PPType.ppType: bypassed HOLE"

	      | ppTypePrec' p (REC _) =
		    Crash.crash"PPType.ppType: bypassed REC"


	    and ppRow NIL		= empty
	      | ppRow RHO		= text "..."
	      | ppRow(FLD(l,ts,NIL))	= ppField(l,ts)
	      | ppRow(FLD(l,ts,r))	= ppField(l,ts) ^^ text "," ^/^ ppRow r

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
		    val a' = makeVar(true, a)
		in
		    abox(
			hbox(
			    ppType a ^/^
(*DEBUG*)
(*text"(" ^^ ppTypePrec' 0 a' ^^ text")" ^/^*)
			    text "."
			) ^^
			below(break ^^
			    ppType t
			)
		    )
		    before a := a'
		end
	in
	    ppType t before List.app op:= (!trail)
	end

  end
