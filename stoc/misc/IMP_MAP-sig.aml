signature IMP_MAP =
  sig

    type key
    type 'a map
    type 'a t = 'a map

    exception Delete
    exception Collision of key

    val new :		int -> 'a map
    val copy :		'a map -> 'a map

    val delete :	'a map * key -> unit		(* Delete *)
    val insert :	'a map * key * 'a -> unit
    val insertDisjoint:	'a map * key * 'a -> unit	(* Collision *)
    val plus :		'a map * 'a map -> unit
    val plusDisjoint :	'a map * 'a map -> unit		(* Collision *)

    val lookup :	'a map * key -> 'a option
    val isEmpty :	'a map -> bool

    val app :		(key * 'a -> unit) -> 'a map -> unit
    val fold :		((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b

  end
