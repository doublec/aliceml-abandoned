(*
 * A stateful scoped map (a stateful stack of stateful maps).
 *)

signature SCOPED_IMP_MAP =
  sig

    type key
    type 'a map
    type 'a t = 'a map

    exception Delete    of key
    exception Collision of key

    val new :			unit -> 'a map

    val copy :			'a map -> 'a map
    val copyScope :		'a map -> 'a map

    val insertScope :		'a map -> unit
    val inheritScope :		'a map * 'a map -> unit
    val deleteScope :		'a map -> unit
    val delete2ndScope :	'a map -> unit
    val splitScope :		'a map -> 'a map

    val mergeScope :		'a map -> unit
    val mergeDisjointScope :	'a map -> unit			(* Collision *)
    val mergeScopeWith :	('a * 'a -> 'a) -> 'a map -> unit
    val mergeScopeWithi :	(key * 'a * 'a -> 'a) -> 'a map -> unit

    val delete :		'a map * key -> unit		(* Delete *)

    val insert :		'a map * key * 'a -> unit
    val insertDisjoint :	'a map * key * 'a -> unit	(* Collision *)
    val insertWith :		('a * 'a -> 'a) -> 'a map * key * 'a -> unit
    val insertWithi :		(key * 'a * 'a -> 'a) -> 'a map * key * 'a
									 -> unit
    val union :			'a map * 'a map -> unit
    val unionDisjoint :		'a map * 'a map -> unit		(* Collision *)
    val unionWith :		('a * 'a -> 'a) -> 'a map * 'a map -> unit
    val unionWithi :		(key * 'a * 'a -> 'a) -> 'a map * 'a map -> unit

    val lookup :		'a map * key -> 'a option
    val lookupScope :		'a map * key -> 'a option

    val size :			'a map -> int
    val sizeScope :		'a map -> int

    val isEmpty :		'a map -> bool
    val isEmptyScope :		'a map -> bool

    val app :			('a -> unit) -> 'a map -> unit
    val appScope :		('a -> unit) -> 'a map -> unit
    val appi :			(key * 'a -> unit) -> 'a map -> unit
    val appiScope :		(key * 'a -> unit) -> 'a map -> unit

    val fold :			('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldScope :		('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldi :			(key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldiScope :		(key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b

  end
