structure InfPrivate =
  struct

  (* Types *)

    type path = Path.t
    type id   = Sign.id

    datatype inf =				(* [jota,j] *)
	  ANY					(* top *)
	| CON of con				(* interface constructor *)
	| SIG of sign				(* signature *)
	| ARR of id * inf * inf			(* arrow (functor) *)
	| LAM of id * inf * inf			(* abstraction (dep. function)*)
	| APP of inf * inf			(* application *)

    and sort =					(* [s] *)
	  GROUND				(* ordinary interface *)
	| DEP of inf * sort			(* dependent *)

    withtype con  = sort * path			(* [chi,c] *)
    and      sign = (inf,sort) Sign.t		(* [sigma,s] *)

    type t = inf

    type subst = Path.subst
    type rea   = inf Sign.rea

(*
  (* Strengthening *)

    fun strengthen(p,j) =
	let
	    val modRea = StampMap.new()
	    val infRea = StampMap.new()

	    fun strengthenInf p' (SIG is) =
		   SIG(List.foldl (strengthenItem p') [] is))
	      | strengthenInf p' (CON c) =
		   CON c (*UNFINISHED??*)
	      | strengthenInf p'  j = j

	    and strengthenItem p' (VAL_ITEM(l,t,d), is') =
		   VAL(l, t, strengthenPathDef (Path.DOT(p',l)) d) :: is'
	      | strengthenItem p' (CON_ITEM(l,t,d), is') =
		   CON(l, t, strengthenPathDef (Path.DOT(p',l)) d) :: is'
	      | strengthenItem p' (TYP_ITEM(l,k,d), is') =
		   TYP(l, k, strengthenTypDef (Path.DOT(p',l), k) d) :: is'
	      | strengthenItem p' (MOD_ITEM(l,j,d), is') =
		let val p'' = Path.DOT(p',l) in
		   (*UNFINISHED: insert l -- how? need internal ids (stamps) *)
		   MOD(l, strengthenInf p'' j, strengthenPathDef p'' d) :: is'
		end
	      | strengthenItem p' (INF_ITEM(l,s,d), is') =
		let val p'' = Path.DOT(p',l) in
		   (*UNFINISHED: insert l -- how? need internal ids (stamps) *)
		   INF(l, strengthenSort p'' s, strengthenInfDef p'' d) :: is'
		end

	    and strengthenPathDef p'  NONE      = SOME p'
	      | strengthenPathDef p' (SOME p'') = SOME p'' (*UNFINISHED??*)

	    and strengthenTypDef (p',k)  NONE =
		   SOME(Type.inCon(k,Type.CLOSED,p'))
	      | strengthenTypDef (p',k) (SOME p'') = SOME p'' (*UNFINISHED??*)

	    and strengthenInfDef (p',s)  NONE      = SOME(CON(s,p'))
	      | strengthenInfDef (p',s) (SOME p'') = SOME p'' (*UNFINISHED??*)

	    and strengthenSort p'  GROUND = GROUND
	      | strengthenSort p' (DEP(j,s)) =
		   (* UNFINISHED: think about this... *)
		   DEP(strengthenInf p' j, strengthenSort p' s)
	in
	    strengthenInf p j
	end


  (* Matching *)

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
  end


structure Inf :> INF = InfPrivate
