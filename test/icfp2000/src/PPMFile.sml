signature PPM_FILE =
sig
    type outfile
    type color = Word8.word * Word8.word * Word8.word

    val openOut :  string * int * int -> outfile
    val output1 :  outfile * color -> unit
    val closeOut : outfile -> unit
end


structure PPMFile :> PPM_FILE =
struct
    type outfile = BinIO.outstream
    type color   = Word8.word * Word8.word * Word8.word

    fun openOut(name, width, height) =
	let
	    val file   = BinIO.openOut name
	    val header = "P6\n# Helikopter\n" ^
			 Int.toString width ^" "^ Int.toString height ^ " 255\n"
	in
	    BinIO.output(file, Byte.stringToBytes header);
	    file
	end

    fun output1(file, (r,g,b)) = 
	( BinIO.output1(file, r)
	; BinIO.output1(file, g)
	; BinIO.output1(file, b)
	)

    val closeOut = BinIO.closeOut
end
