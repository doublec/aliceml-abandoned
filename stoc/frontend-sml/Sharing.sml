(*
 * Translation of sharing constraints.
 *
 * The algorithm takes a list of specs and a list of longids inducing sharing
 * upon the specs and works as follows:
 * 1. Build annotated specs, that pair each longid with the spec it refers to.
 * 2. Look for the first spec now annotated with a longid, take this longid as
 *    the original object.
 * 3. Transform each remaining annotated spec to express the induced sharing
 *    equivalence. For a type or signature spec this is done by replacing it
 *    with an appropriate manifest spec (blindly overwriting the spec - there is
 *    no check for either rigidness or kind consistency). For a structures its
 *    signature is intersected with an appropriate specialisation. Note that
 *    even type sharing constraints with longids effect only structure specs.
 *)

structure Sharing :> SHARING =
  struct

    open AbstractGrammar

    nonfix mod

  (* Class *)

    datatype class = TYP | SIG | STR

  (* Error handling *)

    structure E = AbstractionError

    fun error(class, LongId(_, longid, _)) = error(STR, longid)
      | error(TYP, ShortId(i,id)) = E.error(i, E.SharingExternalTy id)
      | error(SIG, ShortId(i,id)) = E.error(i, E.SharingExternalSig id)
      | error(STR, ShortId(i,id)) = E.error(i, E.SharingExternalStr id)

  (* Find ids in a list of longids *)

    fun isRootedAt(ShortId(_, id),       stamp') = stamp id = stamp'
      | isRootedAt(LongId(_, longid, _), stamp') = isRootedAt(longid, stamp')

    fun findId(stamp, [], longids') = NONE
      | findId(stamp, longid::longids, longids') =
	if isRootedAt(longid, stamp)
	then SOME(longid, longids' @ longids)
	else findId(stamp, longids, longid::longids')


  (* Annotated specifications *)

    datatype annotated_spec =
	  Plain     of spec
	| Annotated of spec * longid
	| Recursive of Source.region * annotated_spec list
	| Local     of Source.region * annotated_spec list
	(* UNFINISHED: what about ExtSpec? *)


    fun cons1st(x, (xs,y)) = (x::xs, y)

    fun annotate( spec as ( TypSpec(_, id, _)
			  | DatSpec(_, id, _)
			  | ModSpec(_, id, _)
			  | InfSpec(_, id, _) ), longids) =
	(case findId(stamp id, longids, [])
	   of SOME(longid,longids') => ( Annotated(spec,longid), longids' )
	    | NONE                  => ( Plain(spec), longids )
	)
      | annotate(RecSpec(i, specs), longids) =
	let val (specs',longids') = annotateList(specs, longids) in
	    ( Recursive(i, specs'), longids' )
	end
      | annotate(LocalSpec(i, specs), longids) =
	let val (specs',longids') = annotateList(specs, longids) in
	    ( Local(i, specs'), longids' )
	end
      | annotate(spec, longids) =
	    ( Plain(spec), longids )

    and annotateList(    [],      longids) = ([], longids)
      | annotateList(spec::specs, longids) =
	let val (spec',longids') = annotate(spec, longids) in
	    cons1st(spec', annotateList(specs, longids'))
	end


  (* Convert annotated spec to spec with where constraints *)

    fun longidToMod(ShortId(i, id))         = VarMod(i, id)
      | longidToMod(LongId(i, longid, lab)) = SelMod(i, longidToMod longid, lab)

    fun singleton(inf, longid) =
	let
	    val i   = Source.over(infoInf inf, infoLongid longid)
	    val mod = AnnMod(i, longidToMod longid, inf)
	in
	    SingInf(i, mod)
	end

    fun constrain(class, inf1, ShortId _, longid) =
	    raise Crash.Crash "Sharing.constrain"
      | constrain(class, inf1, LongId(i, longid', lab), longid) =
	let
	    fun buildSig(ShortId(i, id), inf) = inf
	      | buildSig(LongId(_, longid, lab), inf) =
		let val i = infoLab lab in
		    SigInf(i, [ModSpec(i, labToId lab, inf)])
		end

	    val i0    = infoLab lab
	    val i1    = infoLongid longid
	    val id0   = labToId lab
	    val spec0 = case class
			  of TYP => TypSpec(i0, id0, ConTyp(i1, longid))
			   | SIG => InfSpec(i0, id0, ConInf(i1, longid))
			   | STR => ModSpec(i0, id0,
					    SingInf(i1, longidToMod longid))
	    val inf2  = buildSig(longid', SigInf(i0, [spec0]))
	in
	    CompInf(Source.over(infoInf inf1, i), inf1, inf2)
	end


    (* UNFINISHED: no error checks for non-qualified types and interfaces *)

    fun withWhere(TYP, TypSpec(i, id, typ), _, longid) =
	    TypSpec(i, id, ConTyp(infoLongid longid, longid))
      | withWhere(TYP, DatSpec(i, id, typ), _, longid) =
	    DatSpec(i, id, ConTyp(infoLongid longid, longid))
      | withWhere(SIG, InfSpec(i, id, inf), _, longid) =
	    InfSpec(i, id, ConInf(infoLongid longid, longid))
      | withWhere(STR, ModSpec(i, id, inf), ShortId _, longid) =
	    ModSpec(i, id, singleton(inf, longid))
      | withWhere(class, ModSpec(i, id, inf), longid', longid) =
	    ModSpec(i, id, constrain(class, inf, longid', longid))
      | withWhere _ = raise Crash.Crash "Sharing.withWhere"


  (* Map where constraints over list of annotated specs *)

    (* find 1st annotation *)
    fun mapWhere(class, []) = raise Crash.Crash "Sharing.mapWhere"
      | mapWhere(class, Plain(spec)::specs') =
	    spec :: mapWhere(class, specs')
      | mapWhere(class, Annotated(spec, longid)::specs') =
	    spec :: mapWhere''(class, specs', longid)
      | mapWhere(class, Recursive(i, specs'')::specs') =
	(case mapWhere'(class, specs'')
	   of (specs, NONE) =>
		RecSpec(i,specs) :: mapWhere(class, specs')
	    | (specs, SOME longid) =>
		RecSpec(i,specs) :: mapWhere''(class, specs', longid)
	)
      | mapWhere(class, Local(i, specs'')::specs') =
	(case mapWhere'(class, specs'')
	   of (specs, NONE) =>
		LocalSpec(i,specs) :: mapWhere(class, specs')
	    | (specs, SOME longid) => 
		LocalSpec(i,specs) :: mapWhere''(class, specs', longid)
	)

    (* find 1st annotation in nested lists *)
    and mapWhere'(class, []) = raise Crash.Crash "Sharing.mapWhere'"
      | mapWhere'(class, Plain(spec)::specs') =
	    cons1st(spec, mapWhere'(class, specs'))
      | mapWhere'(class, Annotated(spec, longid)::specs') =
	    ( spec :: mapWhere''(class, specs', longid), SOME longid )
      | mapWhere'(class, Recursive(i, specs'')::specs') =
	(case mapWhere'(class, specs'')
	   of (specs, NONE) =>
		cons1st(RecSpec(i,specs), mapWhere'(class, specs'))
	    | (specs, some as SOME longid) =>
		( RecSpec(i,specs) :: mapWhere''(class, specs', longid), some )
	)
      | mapWhere'(class, Local(i, specs'')::specs') =
	(case mapWhere'(class, specs'')
	   of (specs, NONE) =>
		cons1st(LocalSpec(i,specs), mapWhere'(class, specs'))
	    | (specs, some as SOME longid) =>
		( LocalSpec(i,specs) :: mapWhere''(class, specs',longid), some )
	)

    (* transform remaining annotations *)
    and mapWhere''(class, [], longid) = []
      | mapWhere''(class, Plain(spec)::specs', longid) =
	    spec :: mapWhere''(class, specs', longid)
      | mapWhere''(class, Annotated(spec, longid')::specs', longid) =
	    withWhere(class, spec, longid', longid)
		:: mapWhere''(class, specs', longid)
      | mapWhere''(class, Recursive(i, specs'')::specs', longid) =
	    RecSpec(i, mapWhere''(class, specs'',longid))
		:: mapWhere''(class, specs',longid)
      | mapWhere''(class, Local(i, specs'')::specs', longid) =
	    LocalSpec(i, mapWhere''(class, specs'',longid))
		:: mapWhere''(class, specs',longid)


  (* Sharing *)

    fun share class (specs, longids) =
	case annotateList(specs, longids)
	  of (specs', longid::_) => error(class, longid)
	   | (specs',       [] ) => mapWhere(class, specs')

    val shareTyp = share TYP
    val shareSig = share SIG
    val shareStr = share STR

  end
