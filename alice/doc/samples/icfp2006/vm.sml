signature STORE =
sig
    type word32 = Word32.word
    type addr = word32
    val init : string -> unit
    val alloc : word32 -> addr
    val free : addr -> unit
    val get : addr * word32 -> word32
    val set : addr * word32 * word32 -> unit
    val move : addr -> unit
end

structure Store : STORE =
struct
    open Array
    type word32 = Word32.word
    type addr = word32
    datatype block = UNUSED of addr | USED of word32 array

    fun unused(UNUSED x) = x | unused _ = raise Domain
    fun used(USED x) = x | used _ = raise Domain

    val i2w = Word32.fromInt
    val w2i = Word32.toInt

    val blocks = ref(array(1, USED(array(0, 0w0))))
    val freelist = ref 0w1

    fun getblock a = sub(!blocks, w2i a)
    fun setblock(a, block) = update(!blocks, w2i a, block)

    fun init filename =
	let
	    val file = BinIO.openIn filename
	    val raw  = BinIO.inputAll file
	    val size = Word8Vector.length raw div 4
	    val prog = tabulate(size, fn i => Word32.fromLarge(PackWord32Big.subVec(raw, i)))
	in
	    BinIO.closeIn file;
	    setblock(0w0, USED prog)
	end

    fun grow() =
	let
	    val newsize = 2 * length(!blocks)
	    val newblocks = tabulate(newsize, fn i => UNUSED(i2w i + 0w1))
	in
	    copy{src= !blocks, dst=newblocks, di=0};
	    blocks := newblocks
	end

    fun alloc size =
	let
	    val addr = !freelist
	in
	    if w2i addr = length(!blocks) then grow() else ();
	    freelist := unused(getblock addr);
	    setblock(addr, USED(array(w2i size, 0w0)));
	    addr
	end

    fun free addr = (setblock(addr, UNUSED(!freelist)); freelist := addr)

    fun get(addr, idx) = sub(used(getblock addr), w2i idx)
    fun set(addr, idx, x) = update(used(getblock addr), w2i idx, x)

    fun move 0w0 = ()
      | move addr =
	let
	    val src = used(getblock addr)
	    val dst = tabulate(length src, fn i => sub(src, i))
	in
	    setblock(0w0, USED dst)
	end
end

structure Machine : sig val run : unit -> unit end =
struct
    open Word32 infix >> andb
    val c2w = Word32.fromLarge o Word8.toLarge o Byte.charToByte
    val w2c = Byte.byteToChar o Word8.fromLarge o Word32.toLarge

    val pc = ref 0w0
    val regs = Array.array(8, 0w0)
    fun get n = Array.sub(regs, n)
    fun set(n, w) = Array.update(regs, n, w)

    val regmask = 0wx00000007
    val valmask = 0wx01ffffff
    fun a word = toInt((word >> 0w6) andb regmask)
    fun b word = toInt((word >> 0w3) andb regmask)
    fun c word = toInt(word andb regmask)
    fun a' word = toInt((word >> 0w25) andb regmask)
    fun v word = word andb valmask

    fun run() =
	let
	    val w = Store.get(0w0, !pc)
	in
	    pc := !pc + 0w1;
	    case w >> 0w28 of
		0w0 => if get(c w) = 0w0 then () else set(a w, get(b w))
	      | 0w1 => set(a w, Store.get(get(b w), get(c w)))
	      | 0w2 => Store.set(get(a w), get(b w), get(c w))
	      | 0w3 => set(a w, get(b w) + get(c w))
	      | 0w4 => set(a w, get(b w) * get(c w))
	      | 0w5 => set(a w, get(b w) div get(c w))
	      | 0w6 => set(a w, notb(get(b w) andb get(c w)))
	      | 0w7 => OS.Process.exit OS.Process.success
	      | 0w8 => set(b w, Store.alloc(get(c w)))
	      | 0w9 => Store.free(get(c w))
	      | 0w10 => TextIO.print(str(w2c(get(c w))))
	      | 0w11 => (case TextIO.input1 TextIO.stdIn of
			     NONE => set(c w, 0wxffffffff)
			   | SOME ch => set(c w, c2w ch))
	      | 0w12 => (Store.move(get(b w)); pc := get(c w))
	      | 0w13 => set(a' w, v w)
	      | _ => raise Fail "decode";
	    run()
	end
end

structure Main =
struct
    val _ = Store.init(List.hd(CommandLine.arguments()))
    val _ = Machine.run()
end
