(*
 * Stuff that should be in the standard structures.
 *)

signature MISC =
  sig

    val Option_isNone :	'a option -> bool
    val Option_app :	('a -> unit) -> 'a option -> unit
    val Option_fold :	('a * 'b -> 'b) -> 'b -> 'a option -> 'b

    val ListPair_find :	('a * 'b -> bool) -> 'a list * 'b list -> ('a * 'b) option

    val Array_all :	('a -> bool) -> 'a array -> bool
    val Array_exists :	('a -> bool) -> 'a array -> bool

  end
