(*
 * Stuff that should be in the standard structures.
 *)

signature MISC =
  sig

    val General_swap :	'a ref * 'a ref -> unit

    val Option_isNone :	'a option -> bool
    val Option_app :	('a -> unit) -> 'a option -> unit
    val Option_fold :	('a * 'b -> 'b) -> 'b -> 'a option -> 'b

    val List_appr :	('a -> unit) -> 'a list -> unit
    val List_foldli :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val List_foldri :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val List_mapi :	(int * 'a -> 'b) -> 'a list -> 'b list
    val List_appi :	(int * 'a -> unit) -> 'a list -> unit
    val List_appri :	(int * 'a -> unit) -> 'a list -> unit

    val ListPair_find :	('a * 'b -> bool) -> 'a list * 'b list -> ('a * 'b) option

    val Vector_append :	'a vector * 'a vector -> 'a vector
    val Vector_rev :	'a vector -> 'a vector
    val Vector_appr :	('a -> unit) -> 'a vector -> unit
    val Vector_appri :	(int * 'a -> unit) -> 'a vector * int * int option -> unit
    val Vector_all :	('a -> bool) -> 'a vector -> bool
    val Vector_exists :	('a -> bool) -> 'a vector -> bool
    val Vector_find :	('a -> bool) -> 'a vector -> 'a option

    val Array_fromVector : 'a vector -> 'a array
    val Array_swap :	'a array * int * int -> unit
    val Array_reverse :	'a array -> unit
    val Array_all :	('a -> bool) -> 'a array -> bool
    val Array_exists :	('a -> bool) -> 'a array -> bool
    val Array_find :	('a -> bool) -> 'a array -> 'a option

    val Char_toWide :		Char.char -> WideChar.char
    val Char_fromWide :		WideChar.char -> Char.char	(* Chr *)

    val String_toWide :		String.string -> WideString.string
    val String_fromWide :	WideString.string -> String.string (* Chr *)

  end
