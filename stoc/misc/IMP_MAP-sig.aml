signature HASH_KEY =
  sig
    eqtype t
    val hash :  t -> int
  end

signature HASHTABLE =
  sig

    type key (* = Key.t *)
    type 'a hashtable
    type 'a t = 'a hashtable

    exception Lookup
    exception Collision of key

    val new :		int -> 'a hashtable
    val copy :		'a hashtable -> 'a hashtable

    val insert :	'a hashtable * key * 'a -> unit
    val insertDisjoint:	'a hashtable * key * 'a -> unit		(* Collision *)
    val plus :		'a hashtable * 'a hashtable -> unit
    val plusDisjoint :	'a hashtable * 'a hashtable -> unit	(* Collision *)

    val lookup :	'a hashtable * key -> 'a		(* Lookup *)
    val isEmpty :	'a hashtable -> bool

    val app :		(key * 'a -> unit) -> 'a hashtable -> unit
    val fold :		((key * 'a) * 'b -> 'b) -> 'b -> 'a hashtable -> 'b

  end
