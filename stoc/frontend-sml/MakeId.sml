(*
 * Standard ML identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic functor to represent all kinds of identifiers (except
 *   for labels tyvars).
 *)


functor Id(Stamp: STAMP) :> ID =
  struct

    type Id = string
    type t  = Id

    fun invent()    = "_id" ^ Stamp.toString(Stamp.new())
    fun inventAs id = "_" ^ id ^ Stamp.toString(Stamp.new())

    fun fromString s = s
    fun toString s   = s

    val compare = String.compare

  end
