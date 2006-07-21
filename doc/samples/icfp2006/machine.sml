import "store"
import "code"
import "decoder"
import "disassemble"

signature MACHINE =
sig
    type machine

    exception InvalidInstruction
    exception Halted

    val machine : Store.store -> machine
    val step : machine -> unit
end

structure Machine :> MACHINE =
struct
    exception InvalidInstruction
    exception Halted

    type machine = {store: Store.store,
		    prog: Code.program}

    fun machine s = {store=s, prog=Decoder.decode (s, Word32.fromInt 0)}

    fun step (m : machine) =
	let
	    open Code
	    open Store
	    val s = #store m
	    val pc = getpc s
	    val instr = Array.sub (#prog m, pc)
	in
(*
print ("["^Word32.toString (Store.getreg(s, 0)));
print (","^Word32.toString (Store.getreg(s, 1)));
print (","^Word32.toString (Store.getreg(s, 2)));
print (","^Word32.toString (Store.getreg(s, 3)));
print (","^Word32.toString (Store.getreg(s, 4)));
print (","^Word32.toString (Store.getreg(s, 5)));
print (","^Word32.toString (Store.getreg(s, 6)));
print (","^Word32.toString (Store.getreg(s, 7))^"] \t");
Disassemble.disassemble (#prog m, pc, 1);
*)
	    setpc (s, pc+1);
	    case instr of
		Move {dst, src, cond} =>
		    if getreg (s, cond)<>Word32.fromInt 0 then
			setreg(s, dst, getreg (s, src))
		    else ()
	      | Get  {dst, arr, idx}  =>
			setreg (s, dst,
				get (s, {arr=getreg(s, arr),
					 idx=getreg(s, idx)}))
	      | Set  {arr, idx, src}  =>
			set (s, {arr=getreg(s, arr),
				 idx=getreg(s, idx),
				 x=getreg(s, src)})
	      | Add  {dst, x, y}      =>
			setreg (s, dst,
				Word32.+(getreg(s, x),
					 getreg(s, y)))
	      | Mul  {dst, x, y}      =>
			setreg (s, dst,
				Word32.*(getreg(s, x),
					 getreg(s, y)))
	      | Div  {dst, x, y}      =>
			setreg (s, dst,
				Word32.div(getreg(s, x),
					   getreg(s, y)))
	      | Nand {dst, x, y}      =>
			setreg (s, dst,
				Word32.~(Word32.andb(getreg(s, x),
						     getreg(s, y))))
	      | Halt                  => raise Halted
	      | Alloc {dst, size}     =>
			setreg (s, dst,
				alloc (s,
				       Word32.toInt (getreg (s, size))))
	      | Free {src}            =>
			free (s, getreg (s, src))
	      | Out {src}             =>
			TextIO.output1(TextIO.stdOut,
				       Byte.byteToChar (Word8.fromLarge
							(getreg (s, src))))
	      | In {dst}              =>
			(case TextIO.input1 TextIO.stdIn of
			     NONE => setreg (s, dst,
					     Word32.~(Word32.fromInt 0))
			   | SOME w =>
(*
				 if w= #"\n" then
				     setreg (s, dst,
					     Word32.~(Word32.fromInt 0))
				 else
*)
				     setreg (s, dst,
					     Word8.toLarge
					     (Byte.charToByte w)))
	      | Load {arr, off}       =>
			     (move (s, getreg (s, arr));
			      setpc (s, Word32.toInt (getreg (s, off))))

	      | Imm {dst, i}          =>
			     setreg (s, dst, i)
	      | Invalid               => raise InvalidInstruction
	end

end
