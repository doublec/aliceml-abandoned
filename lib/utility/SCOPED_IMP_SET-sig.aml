(*
 * A stateful scoped map (a stateful stack of stateful maps).
 *)

signature SCOPED_IMP_SET =
  sig

    type item
    type set
    type t = set

    exception Delete
    exception Collision of item

    val new :		unit -> set
    val copy :		set -> set
    val copyScope:	set -> set

    val insertScope :	set -> unit
    val deleteScope :	set -> unit
    val delete2ndScope:	set -> unit
    val mergeScope :	set -> unit

    val delete :	set * item -> unit		(* Delete *)
    val insert :	set * item -> unit
    val insertDisjoint:	set * item -> unit		(* Collision *)
    val union :		set * set  -> unit
    val unionDisjoint :	set * set  -> unit		(* Collision *)
    val insertWith :	(item -> unit) -> set * item -> unit
    val unionWith :	(item -> unit) -> set * set -> unit

    val member :	set * item -> bool
    val memberScope :	set * item -> bool
    val isEmpty :	set -> bool
    val isEmptyScope :	set -> bool

    val app :		(item -> unit) -> set -> unit
    val appScope :	(item -> unit) -> set -> unit
    val fold :		(item * 'a -> 'a) -> 'a -> set -> 'a
    val foldScope :	(item * 'a -> 'a) -> 'a -> set -> 'a

  end
