signature STORE =
sig
    type word32 = Word32.word
    type reg = int
    type store

    exception Address

    val init : string -> store
    val load : string -> store
    val save : string * store -> unit

    val alloc : store * int -> word32
    val free : store * word32 -> unit
    val size : store * word32 -> int
    val get : store * {arr:word32, idx:word32} -> word32
    val set : store * {arr:word32, idx:word32, x:word32} -> unit
    val move : store * word32 -> unit

    val getreg : store * reg -> word32
    val setreg : store * reg * word32 -> unit
end

structure Store :> STORE =
struct
end
