(*
 * Standard ML basic objects (shared between syntax and semantics)
 *
 * Definition, sections 2.2, 2.4, 3.2, 4.1, 5.1, 6.2, and 7.2
 *
 * Modifications:
 *   Longids are not included since they have been moved to the context-free
 *   grammar.
 *)


local
structure Stamp	= MakeStamp()
in
structure VId	= MakeId(Stamp)
structure TyCon	= MakeId(Stamp)
structure TyVar	= MakeId(Stamp)
structure StrId	= MakeId(Stamp)
structure SigId = MakeId(Stamp)
end
