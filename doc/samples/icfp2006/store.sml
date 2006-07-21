signature STORE =
sig
    type word32 = Word32.word
    type block = word32 array

    exception Address

    val init : string -> unit
    val load : string -> unit
    val save : string -> unit

    val alloc : int -> word32
    val free : word32 -> unit
    val size : word32 -> int
    val get : {arr:word32, idx:word32} -> word32
    val set : {arr:word32, idx:word32, x:word32} -> unit
    val move : word32 -> unit
end

structure Store : STORE =
struct
end
