signature IMP_SET =
  sig

    type item
    type set
    type t = set

    exception Delete
    exception Collision of item

    val new :		int -> set
    val copy :		set -> set

    val delete :	set * item -> unit		(* Delete *)
    val insert :	set * item -> unit
    val insertDisjoint:	set * item -> unit		(* Collision *)
    val union :		set * set  -> unit
    val unionDisjoint :	set * set  -> unit		(* Collision *)

    val member :	set * item -> bool
    val isEmpty :	set -> bool

    val app :		(item -> unit) -> set -> unit
    val fold :		(item * 'a -> 'a) -> 'a -> set -> 'a

  end
