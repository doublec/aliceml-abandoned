structure Backend=
    struct
	type stamp=IntermediateGrammar.stamp

	(* Hashtabelle für Stamps. *)
	structure StampHash = MakeHashImpMap(type t=stamp val hash=Stamp.hash)

	(* Scoped Sets für Stamps. Wird zur Berechnung der freien
	 Variablen benutzt. *)
	structure ScopedStampSet = MakeHashScopedImpSet(type t=stamp
							val hash=Stamp.hash)

	(* Hashtabelle für Listen von Strings. Wird benötigt bei der
	 statischen Berechnung der Recordaritäten. *)
	structure StringListHash = MakeHashImpMap(StringListHashKey)

	(* Hashtabelle für Integers. Wird benötigt zum statischen
	 Generieren von Integerkonstanten. *)
	structure IntHash = MakeHashImpMap (type t=int fun hash n=n)

	structure StampSet = MakeHashImpSet(type t=stamp val hash=Stamp.hash)

	val toplevel = Stamp.new()
	val illegalstamp = Stamp.new()

	structure Lambda = MakeLambda(structure StampSet=StampSet
				      structure StampHash=StampHash
				      val toplevel=toplevel)
end
