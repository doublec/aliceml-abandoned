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

    fun ppTyp t =
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

	    fun ppTyp t = fbox(below(ppTypPrec 0 t))

	    and ppTypPrec p (t as ref(HOLE(k,n))) =
		let
		    val t'  = makeVar(false, t)
		    val doc = ppTypPrec' p (!t)
		in
		    if k = STAR then
			doc
(*DEBUG*)
^^ text("_" ^ Int.toString n)
		    else
			parenPrec p (0, doc ^/^ text ":" ^/^ ppKind k)

		end

	      | ppTypPrec p (t as ref(REC t1 | MARK(REC t1))) =
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
					    ppTyp t ^/^
					    text "."
					) ^^
					below(break ^^
					    ppTypPrec 1 t1
					)
				  )
			val _   = t := t'
		    in
			parenPrec p (1, fbox(below(nest(doc))))
		    end
		else
(*(print"not recursive\n";*)
		    ppTypPrec p t1
)

	      | ppTypPrec p (t as ref(APP _)) =
	        ( reduce t ;
(*print("[pp APP]");*)
		  if isApp t then ppTypPrec' p (!t)
			     else ppTypPrec p t
		)

	      | ppTypPrec p (ref t') = ppTypPrec' p t'
(*(*DEBUG*)
	      | ppTypPrec p (t as ref t') =
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
					    ppTyp t ^/^
					    text "."
					) ^^
					below(break ^^
					    ppTypPrec' 1 t'
					)
				    )
			val _   = t := a'
		    in
			parenPrec p (1, fbox(below(nest(doc))))
		    end
		else
		    ppTypPrec' p t'
end
*)

	    and ppTypPrec' p (LINK t) =
(*DEBUG
text "@" ^^*)
		    ppTypPrec p t

	      | ppTypPrec' p (MARK t') =
		    text "!" ^^ ppTypPrec' p t'

	      | ppTypPrec' p (ARR(t1,t2)) =
		let
		    val doc = ppTypPrec 3 t1 ^/^ text "->" ^/^ ppTypPrec 2 t2
		in
		    parenPrec p (2, doc)
		end

	      | ppTypPrec' p (TUP [] | ROW NIL) =
		    text "unit"

	      | ppTypPrec' p (TUP ts) =
		let
		    val doc = ppStarList (ppTypPrec 4) ts
		in
		    parenPrec p (3, fbox(below(nest doc)))
		end

	      | ppTypPrec' p (ROW r) =
		    brace(fbox(below(ppRow r)))

	      | ppTypPrec' p (SUM r) =
		    paren(fbox(below(ppSum r)))

	      | ppTypPrec' p (VAR(k,n)) =
		if k = STAR then
		    text "'?"
(*DEBUG*)
^^ text("[" ^ Int.toString n ^ "]")
		else
		    paren (text "'?" ^/^ text ":" ^/^ ppKind k)

	      | ppTypPrec' p (CON c) =
		    ppCon c

	      | ppTypPrec' p (ALL(a,t)) =
		let
		    val doc = ppBinder("ALL",a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypPrec' p (EX(a,t)) =
		let
		    val doc = ppBinder("EX",a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypPrec' p (LAM(a,t)) =
		let
		    val doc = ppBinder("FN",a,t)
		in
		    parenPrec p (1, fbox(below doc))
		end

	      | ppTypPrec' p (t' as APP _) =
		let
		    val (t,ts) = uncurry(ref t')
		in
		    fbox(nest(ppSeqPrec ppTypPrec 4 ts ^/^ ppTypPrec 5 t))
		end

	      | ppTypPrec' p (HOLE _) =
		    Crash.crash "PPType.ppTyp: bypassed HOLE"

	      | ppTypPrec' p (REC _) =
		    Crash.crash"PPType.ppTyp: bypassed REC"


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
			    ppCommaList ppTyp ts
			)
		    )

	    and ppBinder(s,a,t) =
		let
		    val a' = makeVar(true, a)
		in
		    abox(
			hbox(
			    text s ^/^
			    ppTyp a ^/^
(*DEBUG*)
(*text"(" ^^ ppTypPrec' 0 a' ^^ text")" ^/^*)
			    text "."
			) ^^
			nest(break ^^
			    ppTyp t
			)
		    )
		    before a := a'
		end
	in
	    ppTyp t before List.app op:= (!trail)
	end

  end
