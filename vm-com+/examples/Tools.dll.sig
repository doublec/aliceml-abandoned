signature TOOLS_COMPONENT =
  struct
    signature TOOLS =
	sig
	    type label = string

	    datatype ty =
		IntTy | WordTy | CharTy | StringTy | RealTy
	      | TupleTy of ty vector | RecordTy of (label * ty) vector
	      | SumTy of (label * ty option) vector
	      | ArrayTy of ty | VectorTy of ty
	      | ArrowTy

	    val toString: 'a * ty -> string
	    val inspect:  'a * ty -> unit
	end

    structure Tools : TOOLS
  end
