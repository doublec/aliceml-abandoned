signature IMP_MAP =
  sig

    type key
    type 'a map
    type 'a t = 'a map

    exception Delete    of key
    exception Collision of key
    exception Lookup    of key

    val new :		unit -> 'a map
    val clone :		'a map -> 'a map

    val delete :	'a map * key -> unit
    val deleteExistent:	'a map * key -> unit		(* Delete *)
    val deleteWith :	(key -> unit) -> 'a map * key -> unit
    val deleteAll :	'a map -> unit

    val insert :	'a map * key * 'a -> unit
    val insertDisjoint:	'a map * key * 'a -> unit	(* Collision *)
    val insertWith :	('a * 'a -> 'a) -> 'a map * key * 'a -> unit
    val insertWithi :	(key * 'a * 'a -> 'a) -> 'a map * key * 'a -> unit

    val union :		'a map * 'a map -> unit
    val unionDisjoint :	'a map * 'a map -> unit		(* Collision *)
    val unionWith :	('a * 'a -> 'a) -> 'a map * 'a map -> unit
    val unionWithi :	(key * 'a * 'a -> 'a) -> 'a map * 'a map -> unit

    val lookup :	'a map * key -> 'a option
    val lookupExistent:	'a map * key -> 'a		(* Lookup *)

    val member :	'a map * key -> bool
    val size :		'a map -> int
    val isEmpty :	'a map -> bool

    val app :		('a -> unit) -> 'a map -> unit
    val fold :		('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val appi :		(key * 'a -> unit) -> 'a map -> unit
    val foldi :		(key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b

  end
