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
    val plus :		'a map * 'a map -> unit
    val plusDisjoint :	'a map * 'a map -> unit		(* Collision *)

    val lookup :	'a map * key -> 'a option
    val lookupScope :	'a map * key -> 'a option
    val isEmpty :	'a map -> bool
    val isEmptyScope :	'a map -> bool

    val app :		(key * 'a -> unit) -> 'a map -> unit
    val appScope :	(key * 'a -> unit) -> 'a map -> unit
    val fold :		((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldScope :	((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b

  end
