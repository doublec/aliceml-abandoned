signature VECTOR =
    sig
	type vector = real * real * real
	type t = vector

	type row = real * real * real * real
	type matrix = row * row * row

	val add: vector * vector -> vector
	val sub: vector * vector -> vector
	val scale: real * vector -> vector
	val dotProd: vector * vector -> real
	val transformPoint: matrix * vector -> vector
	val transformVector: matrix * vector -> vector
	val length: vector -> real
	val toUnit: vector -> vector
    end
