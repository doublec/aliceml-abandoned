signature DECODER =
sig
    exception Decode
    val decode : Store.store * Word32.word32 -> program
end

structure Decoder : DECODER =
struct
    open Code
    open Word32 infix << >> andb

    val regmask = fromWord 0w00000007
    val valmask = fromWord 0w01ffffff

    fun A word = toInt ((word >> 6) andb regmask)
    fun B word = toInt ((word >> 3) andb regmask)
    fun C word = toInt (word andb regmask)
    fun A' word = toInt ((word >> 25) andb regmask)
    fun V word = word andb valmask

    fun decode' (s, a, prog, i) =
	if i = Array.length prog then () else
	let
	    val w = Store.get (s, {arr=a, idx=Word32.fromInt i})
	    val oper =
		case Word32.toInt (Word32.>> (w, 24)) of
	              0 => Move {dst=A w, src=B w, cond=C w}
		    | 1 => Get {dst=A w, arr=B w, idx=C w}
		    | 2 => Set {arr=A w, idx=B w, src=C w}
		    | 3 => Add {dst=A w, x=B w, y=C w}
		    | 4 => Mul {dst=A w, x=B w, y=C w}
		    | 5 => Div {dst=A w, x=B w, y=C w}
		    | 6 => Nand {dst=A w, x=B w, y=C w}
		    | 7 => Halt
		    | 8 => Alloc {dst=B w, size=C w}
		    | 9 => Free {src=C w}
		    | 10 => Out {src=C w}
		    | 11 => In {dst=C w}
		    | 12 => Load {src=B w, off=C w}
		    | 13 => Imm {dst=A' w, i=V w}
		    | _  => raise Decode
	in
	    Array.update (prog, i, oper);
	    decode' (s, a, prog, i+1)
	end

    fun decode (s, a) =
	let
	    val len = Store.size (s, a)
	    val prog = Array.array (len, Halt)
	in
	    decode' (s, a, prog, 0)
	end
end
