signature Set = sig
  type 'a set
  val emptyset : ('a * 'a -> order) -> 'a set

  val isEmpty : 'a set -> bool
  val insert  : 'a set * 'a -> 'a set
  val remove  : 'a set * 'a -> 'a set
  val member  : 'a set * 'a -> bool
  val union   : 'a set * 'a set -> 'a set
end
