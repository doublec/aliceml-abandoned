structure PPInf :> PP_INF =
  struct

    (* Import *)

    open InfPrivate
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Helpers *)

    fun uncurry(ref(APP(j1,p,_)))= let val (j,ps) = uncurry j1 in (j,ps@[p]) end
      | uncurry j		 = (j,[])


    (* Simple objects *)

    fun ppLab l		= text(Label.toString l)
    fun ppCon (k,p)	= PPPath.ppPath p


    (* Interfaces *)

    (* Precedence:
     *	0 : binders (LAM(id : inf1) . inf2)
     *	1 : constructed type (inf(path))
     *)

    fun ppInf(ref j') = fbox(below(ppInf' j'))

    and ppInf'(TOP) =
	    text "TOP"

      | ppInf'(CON c) =
	    ppCon c

      | ppInf'(SIG s) =
	    ppSig' s

      | ppInf'(ARR(p,j1,j2)) =
	let
	    val doc = ppBinder("FCT",p,j1,j2)
	in
	    fbox(below doc)
	end

      | ppInf'(LAM(p,j1,j2)) =
	let
	    val doc = ppBinder("LAM",p,j1,j2)
	in
	    fbox(below doc)
	end

      | ppInf'(j' as APP _) =
	let
	    val (j,ps) = uncurry(ref j')
	in
	    fbox(nest(List.foldl (fn(p,d) => d ^/^ paren(PPPath.ppPath p))
				 (ppInf j) ps))
	end

      | ppInf'(LINK j) =
(*DEBUG
text "@" ^^*)
	    ppInf j

    and ppBinder(s,p,j1,j2) =
	    abox(
		fbox(
		    text s ^^
		    text "(" ^/^
		    below(break ^^
			PPPath.ppPath p ^/^
			text ":" ^^
			nest(break ^^
			    ppInf j1
			)
		    ) ^/^
		    text ")" ^/^
		    text "."
		) ^^
		nest(break ^^
		    ppInf j2
		)
	    )


    (* Signatures *)

    and ppSig' s =
	let
	    val doc = ppSig s
	in
	    abox(below(
		text "sig" ^^
		(if isEmpty doc then
		    empty
		 else
		    nest(vbox(break ^^ doc))
		) ^^ break ^^
		text "end"
	    ))
	end

    and ppSig(ref items, _) = vbox(List.foldl ppItem empty items)

    and ppItem(ref item', doc) = ppItem'(item', doc)

    and ppItem'(VAL((p,l,0), t, w, d), doc) =
	    abox(
		hbox(
		    text(if w = VALUE then "val" else "constructor") ^/^
		    ppLab l ^/^
(*DEBUG
text "(" ^^ PPPath.ppPath p ^^ text ")" ^/^*)
		    text ":"
		) ^^
		nest(break ^^
		    abox(PPType.ppTyp t)
		) ^^
		(case d of NONE => empty | SOME p' =>
		if p' = p then empty else
		nest(break ^^
		    abox(text "=" ^/^ PPPath.ppPath p')
		))
	    ) ^/^ doc

      | ppItem'(TYP((p,l,0), k, w, d), doc) =
	    abox(
		hbox(
		    text(if w = CLOSED then "type" else "datatype") ^/^
		    ppLab l ^/^
(*DEBUG
text "(" ^^ PPPath.ppPath p ^^ text ")" ^/^*)
		    text ":"
		) ^^
		nest(break ^^
		    abox(PPType.ppKind k)
		) ^^
		(case d of NONE => empty | SOME t =>
		if Type.isCon t andalso #3(Type.asCon t) = p then empty else
		nest(break ^^
		    abox(text "=" ^/^ PPType.ppTyp t)
		))
	    ) ^/^ doc

      | ppItem'(MOD((p,l,0), j, d), doc) =
	    abox(
		hbox(
		    text(if isArrow j then "functor" else "structure") ^/^
		    ppLab l ^/^
(*DEBUG
text "(" ^^ PPPath.ppPath p ^^ text ")" ^/^*)
		    text ":"
		) ^^
		nest(break ^^
		    abox(ppInf j)
		) ^^
		(case d of NONE => empty | SOME p' =>
		if p' = p then empty else
		nest(break ^^
		    abox(text "=" ^/^ PPPath.ppPath p')
		))
	    ) ^/^ doc

      | ppItem'(INF((p,l,0), k, d), doc) =
	    abox(
		hbox(
		    text "signature" ^/^
		    ppLab l ^/^
(*DEBUG
text "(" ^^ PPPath.ppPath p ^^ text ")" ^/^*)
		    text ":"
		) ^^
		nest(break ^^
		    abox(ppKind k)
		) ^^
		(case d of NONE => empty | SOME j =>
		if isCon j andalso #2(asCon j) = p then empty else
		nest(break ^^
		    abox(text "=" ^/^ ppInf j)
		))
	    ) ^/^ doc

      | ppItem'(_, doc) = doc		(* hidden item *)


    (* Kinds *)

    and ppKind(ref k') = fbox(below(ppKind' k'))

    and ppKind'(GROUND) =
	    text "*"

      | ppKind'(DEP(p,j,k)) =
	    fbox(below(
		abox(
		    fbox(
			text "PI" ^/^
			text "(" ^^
			below(break ^^
			    PPPath.ppPath p ^/^
			    text ":" ^^
			    nest(break ^^
				ppInf j
			    )
			) ^/^
			text ")" ^/^
			text "."
		    ) ^^
		    nest(break ^^
			ppKind k
		    )
		)
	    ))

  end
