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

    type word32 = Word32.word
    type reg = int

    type store = {regs : word32 array,
		  blocks : word32 array option array ref,
		  maxblock : int ref}

    exception Address
    exception NotImplemented

    val initial_store = 10

    fun input (istr, idx, arr) =
	if BinIO.endOfStream istr then arr
	else
	let
	    val w4 = Word8.toLarge (valOf (BinIO.input1 istr))
	    val w3 = Word8.toLarge (valOf (BinIO.input1 istr))
	    val w2 = Word8.toLarge (valOf (BinIO.input1 istr))
	    val w1 = Word8.toLarge (valOf (BinIO.input1 istr))
	    val w =
		(Word32.<<(w4, Word.fromInt 24)) +
		(Word32.<<(w3, Word.fromInt 16)) +
		(Word32.<<(w2, Word.fromInt 8)) +
		 w1
	in
	    Array.update (arr, idx, w1);
	    input (istr, idx+1, arr)
	end

    fun readProgram filename =
	let
	    val size = (OS.FileSys.fileSize filename) div 4
	    val istr = BinIO.openIn filename
	    val prog = Array.array (size, Word32.fromInt 0)
	in
	    input (istr, 0, prog);
	    BinIO.closeIn istr;
	    prog
	end

    fun init filename =
	let
	    val prog = readProgram filename
	    val store = {regs=Array.array (8, Word32.fromInt 0),
			 blocks=ref (Array.array (initial_store, NONE)),
			 maxblock=ref (initial_store-1)}
	in
	    Array.update (!(#blocks store), 0, SOME prog);
	    store
	end

    fun load filename = raise NotImplemented
    fun save filename = raise NotImplemented

    (* find a currently unused address for a new block *)
    fun freshAddress (s : store) =
	case Array.findi (fn (_, NONE) => true | _ => false) (!(#blocks s)) of
	    SOME (i, _) => i
	  | NONE        =>
		let
		    val blocks = Array.array ((3 * (!(#maxblock s))) div 2,
					      NONE)
		in
		    Array.copy {src=(!(#blocks s)), dst=blocks, di=0};
		    (#blocks s) := blocks;
		    (#maxblock s) := ((3 * (!(#maxblock s))) div 2) - 1;
		    freshAddress s
		end

    fun alloc (s : store, size) =
	let
	    val a = freshAddress s
	in
	    Array.update (!(#blocks s), a,
			  SOME (Array.array (size, Word32.fromInt 0)));
	    Word32.fromInt a
	end

    fun free (s : store, arr) =
	Array.update (!(#blocks s), Word32.toInt arr, NONE)

    fun size (s : store, arr) =
	Array.length (valOf (Array.sub (!(#blocks s), Word32.toInt arr)))

    fun get (s : store, {arr, idx}) =
	Array.sub (valOf (Array.sub (!(#blocks s), Word32.toInt arr)),
		   Word32.toInt idx)

    fun set (s : store, {arr, idx, x}) =
	Array.update (valOf (Array.sub (!(#blocks s), Word32.toInt arr)),
		      Word32.toInt idx, x)

    fun move _ = raise NotImplemented

    fun getreg (s : store, reg) =
	Array.sub (#regs s, reg)

    fun setreg (s : store, reg, x) =
	Array.update (#regs s, reg, x)
end
