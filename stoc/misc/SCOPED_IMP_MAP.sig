(*
 * A stateful scoped map (a stateful stack of stateful maps).
 *)

signature SCOPED_IMP_MAP =
  sig

    type key
    type 'a map
    type 'a t = 'a map

    exception Delete
    exception Collision of key

    val new :		unit -> 'a map
    val copy :		'a map -> 'a map
    val copyScope:	'a map -> 'a map

    val insertScope :	'a map -> unit
    val deleteScope :	'a map -> unit
    val delete2ndScope:	'a map -> unit
    val mergeScope :	'a map -> unit

    val delete :	'a map * key -> unit		(* Delete *)
    val insert :	'a map * key * 'a -> unit
    val insertDisjoint:	'a map * key * 'a -> unit	(* Collision *)
    val union :		'a map * 'a map -> unit
    val unionDisjoint :	'a map * 'a map -> unit		(* Collision *)
    val insertWith :	('a * 'a -> 'a) -> 'a map * key * 'a -> unit
    val unionWith :	('a * 'a -> 'a) -> 'a map * 'a map -> unit
    val insertWithi :	(key * 'a * 'a -> 'a) -> 'a map * key * 'a -> unit
    val unionWithi :	(key * 'a * 'a -> 'a) -> 'a map * 'a map -> unit

    val lookup :	'a map * key -> 'a option
    val lookupScope :	'a map * key -> 'a option
    val isEmpty :	'a map -> bool
    val isEmptyScope :	'a map -> bool

    val app :		('a -> unit) -> 'a map -> unit
    val appScope :	('a -> unit) -> 'a map -> unit
    val fold :		('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldScope :	('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val appi :		(key * 'a -> unit) -> 'a map -> unit
    val appiScope :	(key * 'a -> unit) -> 'a map -> unit
    val foldi :		((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldiScope :	((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b

  end
