(*
 * Stuff that should be in the standard structures.
 *)

signature MISC =
  sig

    val Option_isNone :	'a option -> bool
    val Option_app :	('a -> unit) -> 'a option -> unit
    val Option_fold :	('a * 'b -> 'b) -> 'b -> 'a option -> 'b

    val List_appr :	('a -> unit) -> 'a list -> unit

    val ListPair_find :	('a * 'b -> bool) -> 'a list * 'b list -> ('a * 'b) option

    val Array_all :	('a -> bool) -> 'a array -> bool
    val Array_exists :	('a -> bool) -> 'a array -> bool

    val Char_toWide :		Char.char -> WideChar.char
    val Char_fromWide :		WideChar.char -> Char.char	(* Chr *)

    val String_toWide :		String.string -> WideString.string
    val String_fromWide :	WideString.string -> String.string (* Chr *)

  end
