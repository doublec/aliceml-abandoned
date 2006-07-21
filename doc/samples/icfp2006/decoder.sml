signature DECODER =
sig
    val decode : Store.store * Word32.word32 -> program
end

structure Decoder : DECODER =
struct
end
