signature IMP_SET =
  sig

    eqtype item
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
    val insertWith :	(item -> unit) -> set * item -> unit
    val unionWith :	(item -> unit) -> set * set -> unit

    val member :	set * item -> bool
    val size :		set -> int
    val isEmpty :	set -> bool

    val app :		(item -> unit) -> set -> unit
    val fold :		(item * 'a -> 'a) -> 'a -> set -> 'a

  end
