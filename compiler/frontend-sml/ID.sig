(*
 * Standard ML identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of identifiers (except
 *   for labels and tyvars).
 *)


signature ID =
  sig

    eqtype Id
    type t = Id

    val invent:		unit -> Id
    val inventAs:	Id   -> Id

    val fromString:	string -> Id
    val toString:	Id -> string

    val compare:	Id * Id -> order

  end
(*DEBUG*) where type Id = string
