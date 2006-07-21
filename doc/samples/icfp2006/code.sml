structure Code =
struct
    type word32 = Word32.word
    type reg = int

    datatype oper =
	      Move of {dst:reg, src:reg, cond:reg}
	    | Get of {dst:reg, arr:reg, idx:reg}
	    | Set of {arr:reg, idx:reg, src:reg}
	    | Add of {dst:reg, x:reg, y:reg}
	    | Mul of {dst:reg, x:reg, y:reg}
	    | Div of {dst:reg, x:reg, y:reg}
	    | Nand of {dst:reg, x:reg, y:reg}
	    | Halt
	    | Alloc of {dst:reg, size:reg}
	    | Free of {src:reg}
	    | Out of {src:reg}
	    | In of {dst:reg}
	    | Load of {arr:reg, off:reg}
	    | Imm of {dst:reg, i:word32}

    type program = oper array
end
