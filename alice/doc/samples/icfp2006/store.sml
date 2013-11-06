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

    val getpc : store -> int
    val setpc : store * int -> unit
end

structure Store :> STORE =
struct

    type word32 = Word32.word
    type reg = int

    type store = {regs : word32 array,
		  pc : int ref,
		  blocks : (int, word32 array) alt array ref,
		  free : int ref}

    exception Address
    exception NotImplemented

    fun input (bin, i, arr) =
	if i = Array.length arr then () else
	let
	    val j = 4*i
	    val w4 = Word8.toLarge (Word8Vector.sub (bin, j))
	    val w3 = Word8.toLarge (Word8Vector.sub (bin, j+1))
	    val w2 = Word8.toLarge (Word8Vector.sub (bin, j+2))
	    val w1 = Word8.toLarge (Word8Vector.sub (bin, j+3))
	    val w =
		Word32.<<(w4, Word.fromInt 24) +
		Word32.<<(w3, Word.fromInt 16) +
		Word32.<<(w2, Word.fromInt 8) +
		w1
	in
	    Array.update (arr, i, w);
	    input (bin, i+1, arr)
	end

    fun readProgram filename =
	let
	    val file = BinIO.openIn filename
	    val bin  = BinIO.inputAll file
	    val size = Word8Vector.length bin div 4
	    val prog = Array.array (size, Word32.fromInt 0)
	in
	    BinIO.closeIn file;
	    input (bin, 0, prog);
	    prog
	end

    fun init filename =
	let
	    val prog = readProgram filename
	in
	    {regs=Array.array (8, Word32.fromInt 0),
	     pc=ref 0,
	     blocks=ref (Array.array (1, SND prog)),
	     free=ref 1}
	end

    fun load filename = raise NotImplemented
    fun save filename = raise NotImplemented

    fun grow (s : store) =
	let
	    val oldblocks = !(#blocks s)
	    val oldsize = Array.length oldblocks
	    val newsize = 2 * oldsize
	    val newblocks = Array.tabulate (newsize, fn i => FST (i+1))
	in
	    Array.copy {src=oldblocks, dst=newblocks, di=0};
	    #blocks s := newblocks
	end

    fun alloc (s : store, size) =
	let
	    val a = !(#free s)
	in
	    if a = Array.length (!(#blocks s)) then grow s else ();
	    #free s := Alt.fst (Array.sub (!(#blocks s), a));
	    Array.update (!(#blocks s), a,
			  SND (Array.array (size, Word32.fromInt 0)));
	    Word32.fromInt a
	end

    fun free (s : store, arr) =
	let
	    val a = Word32.toInt arr
	in
	    Array.update (!(#blocks s), a, FST (!(#free s)));
	    #free s := a
	end
	handle Option => raise Address

    fun size (s : store, arr) =
	Array.length (Alt.snd (Array.sub (!(#blocks s), Word32.toInt arr)))
	handle (Subscript|Alt) => raise Address

    fun get (s : store, {arr, idx}) =
	Array.sub (Alt.snd (Array.sub (!(#blocks s), Word32.toInt arr)),
		   Word32.toInt idx)
	handle (Subscript|Alt) => raise Address

    fun set (s : store, {arr, idx, x}) =
	Array.update (Alt.snd (Array.sub (!(#blocks s), Word32.toInt arr)),
		      Word32.toInt idx, x)
	handle (Subscript|Alt) => raise Address

    fun move (s : store, arr) =
	if arr = Word32.fromInt 0 then () else
	let
	    val src = Alt.snd (Array.sub (!(#blocks s), Word32.toInt arr))
	    val size = Array.length src
	    val old = Alt.snd (Array.sub (!(#blocks s), 0))
	    val old_size = Array.length old
	    val dst =
		if size > old_size then
		    Array.array (size, Word32.fromInt 0)
		else
		    old
	in
	    Array.copy {src, dst, di=0};
	    Array.update (!(#blocks s), 0, SND dst)
	end
	handle Alt => raise Address

    fun getreg (s : store, reg) =
	Array.sub (#regs s, reg)

    fun setreg (s : store, reg, x) =
	Array.update (#regs s, reg, x)
	
    fun getpc (s : store) = !(#pc s)
    fun setpc (s : store, pc) = (#pc s) := pc
end
