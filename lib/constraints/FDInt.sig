signature FDInt =
    sig
	exception OutOfRange of int
	type fdint

	val inf : fdint
	val sup : fdint

	val fdint : int -> fdint
    end
