(*
 * Standard ML infix resolution
 *
 * Definition, section 2.6
 *)


structure Infix :> INFIX =
  struct

    (* Import *)

    structure Grammar = PostParseGrammar_Core

    open BasicObjects_Core
    open Grammar


    (* Type definitions *)

    datatype Assoc = LEFT | RIGHT

    type InfStatus = (Assoc * int) option

    type InfEnv    = (Info * InfStatus) VIdSymtable.symtable


    (* Helper for error messages *)

    val error				= Error.error
    fun errorVId(VId(I,vid), s)		= error(I, s ^ VId.toString vid)



    (* Categorisation of atomic expressions and patterns *)

    datatype 'a FixityCategory = NONFIX of 'a
			       | INFIX  of Assoc * int * Grammar.VId

    fun categoriseVId (IE: InfEnv) (at, vid as VId(i,vid')) =
	case VIdSymtable.lookup(IE,vid')
	  of (_,NONE)             => NONFIX(at)
	   | (_,SOME(assoc,prec)) => INFIX(assoc, prec, vid)
        handle VIdSymtable.Lookup => NONFIX(at)

    fun categoriseLongVId IE (at, SHORTLong(i, vid)) =
	    categoriseVId IE (at, vid)
      | categoriseLongVId IE (at, longvid) = NONFIX(at)

    fun categoriseAtExp IE (atexp as LONGVIDAtExp(i, SANSOp, longvid)) =
	    categoriseLongVId IE (atexp, longvid)
      | categoriseAtExp IE (atexp) = NONFIX(atexp)

    fun categoriseAtPat IE (atpat as LONGVIDAtPat(i, SANSOp, longvid)) =
	    categoriseLongVId IE (atpat, longvid)
      | categoriseAtPat IE (atpat) = NONFIX(atpat)



    (* Converting app expressions and patterns into atomic lists *)

    fun flattenExp'(ATEXPExp(i,atexp))   = atexp :: []
      | flattenExp'(APPExp(i,exp,atexp)) = atexp :: flattenExp' exp
      | flattenExp' _ = Crash.crash "Infix.flattenExp: invalid expression"

    fun flattenExp exp = List.rev(flattenExp' exp)

    fun flattenPat'(ATPATPat(i,atpat))   = atpat :: []
      | flattenPat'(APPPat(i,pat,atpat)) = atpat :: flattenPat' pat
      | flattenPat' _ = Crash.crash "Infix.flattenPat: invalid pattern"

    fun flattenPat pat = List.rev(flattenPat' pat)



    (* Resolving infixed expressions and patterns *)

    fun parse (ATXx,APPx,TUPLEAtX,LONGVIDAtX, info,info_At,categorise,flatten)
	      IE x =
	let
	    fun atomic(atx)  = ATXx(info_At atx, atx)

	    fun apply(x,atx) = APPx(Source.over(info x, info_At atx), x, atx)

	    fun pair(x1,x2)  = TUPLEAtX(Source.over(info x1, info x2), [x1,x2])

	    fun infapply(x1,vid,x2) =
		let
		    val i_vid   = info_VId vid
		    val longvid	= SHORTLong(i_vid, vid)
		    val exp	= ATXx(i_vid, LONGVIDAtX(i_vid,SANSOp,longvid))
		    val atx	= pair(x1,x2)
		in
		    APPx(Source.between(info x1, info x2), x, atx)
		end


	    fun app(x, NONFIX(atx)::xs) = app(apply(x,atx), xs)
	      | app other               = other

	    fun inf(10, NONFIX(atx)::xs) = app(atomic(atx), xs)

	      | inf(p, xs as NONFIX(atx)::_) =
		let
		    val result as (x,xs') = inf(p+1,xs)
		in
		   case xs'
		     of INFIX(a,p',vid)::_ => if p'= p then inftail(a,p,x,xs')
						       else result
		      | _                  => result
		end

	      | inf(_, INFIX(_,_,vid)::_) =
		    errorVId(vid, "Misplaced infix identifier ")

	      | inf(_, []) =
		    (* Missing operands are caught in inftail! *)
		    Crash.crash "Infix.parse: empty expression"


	    and inftail(a, p, x1, xs as INFIX(a',p',vid)::x::xs') =
		if p' <> p then
		    (x1, xs)
		else if a <> a' then
		    errorVId(vid, "Conflicting infix associativity at infix \
				  \identifier ")
		else if a = LEFT then
		    let
			val (x2, xs'') = inf(p+1, x::xs')
		    in
			inftail(a, p, infapply(x1,vid,x2), xs'')
		    end
		else (* a = RIGHT *)
		    let
			val (x2, xs'')  = inf(p+1,x::xs')
			val (x2',xs''') = inftail(a,p,x2,xs'')
		    in
		        (infapply(x1,vid,x2), xs''')
		    end

	      | inftail(a, p, x1, INFIX(_,_,vid)::[]) =
		    errorVId(vid, "misplaced infix identifier ")

	      | inftail(a, p, x1, xs) = (x1, xs)
	in
	    #1 (inf(0, List.map (categorise IE) (flatten x)))
	end


    (* Expressions *)

    val exp = parse(ATEXPExp, APPExp, TUPLEAtExp, LONGVIDAtExp,
		    info_Exp, info_AtExp, categoriseAtExp, flattenExp)

    (* Patterns *)

    val pat = parse(ATPATPat, APPPat, TUPLEAtPat, LONGVIDAtPat,
		    info_Pat, info_AtPat, categoriseAtPat, flattenPat)

  end
