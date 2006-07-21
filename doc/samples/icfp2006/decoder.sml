import "store"
import "code"

signature DECODER =
sig
    exception Decode
    val decode : Store.store * Word32.word -> Code.program
end

structure Decoder : DECODER =
struct
    open Code
    open Word32 infix << >> andb

    exception Decode

    val regmask = fromInt 0x00000007
    val valmask = fromInt 0x01ffffff
    val w3 = Word.fromInt 3
    val w6 = Word.fromInt 6
    val w24 = Word.fromInt 24
    val w25 = Word.fromInt 25

    fun a word = toInt ((word >> w6) andb regmask)
    fun b word = toInt ((word >> w3) andb regmask)
    fun c word = toInt (word andb regmask)
    fun a' word = toInt ((word >> w25) andb regmask)
    fun v word = word andb valmask

    fun decode' (st, adr, prog, i) =
	if i = Array.length prog then () else
	let
	    val w = Store.get (st, {arr=adr, idx=Word32.fromInt i})
	    val oper =
		case toInt (w >> w24) of
	              0 => Move {dst=a w, src=b w, cond=c w}
		    | 1 => Get {dst=a w, arr=b w, idx=c w}
		    | 2 => Set {arr=a w, idx=b w, src=c w}
		    | 3 => Add {dst=a w, x=b w, y=c w}
		    | 4 => Mul {dst=a w, x=b w, y=c w}
		    | 5 => Div {dst=a w, x=b w, y=c w}
		    | 6 => Nand {dst=a w, x=b w, y=c w}
		    | 7 => Halt
		    | 8 => Alloc {dst=b w, size=c w}
		    | 9 => Free {src=c w}
		    | 10 => Out {src=c w}
		    | 11 => In {dst=c w}
		    | 12 => Load {arr=b w, off=c w}
		    | 13 => Imm {dst=a' w, i=v w}
		    | _  => raise Decode
	in
	    Array.update (prog, i, oper);
	    decode' (st, adr, prog, Int.+(i,1))
	end

    fun decode (st, adr) =
	let
	    val len = Store.size (st, adr)
	    val prog = Array.array (len, Halt)
	in
	    decode' (st, adr, prog, 0);
	    prog
	end
end
