structure Vector :> VECTOR =
    struct
	type vector = real * real * real
	type t = vector

	type row = real * real * real * real
	type matrix = row * row * row

	fun add ((ax, ay, az), (bx, by, bz)): vector =
	    (ax + bx, ay + by, az + bz)

	fun sub ((ax, ay, az), (bx, by, bz)): vector =
	    (ax - bx, ay - by, az - bz)

	fun scale (k, (x, y, z)): vector = (k * x, k * y, k * z)

	fun dotProd ((ax, ay, az), (bx, by, bz)): real =
	    ax * bx + ay * by + az * bz

	fun transformPoint (((a11, a12, a13, a14),
			     (a21, a22, a23, a24),
			     (a31, a32, a33, a34)), (b1, b2, b3)): vector =
	    (a11 * b1 + a12 * b2 + a13 * b3 + a14,
	     a21 * b1 + a22 * b2 + a23 * b3 + a24,
	     a31 * b1 + a32 * b2 + a33 * b3 + a34)

	fun transformVector (((a11, a12, a13, _),
			      (a21, a22, a23, _),
			      (a31, a32, a33, _)), (b1, b2, b3)): vector =
	    (a11 * b1 + a12 * b2 + a13 * b3,
	     a21 * b1 + a22 * b2 + a23 * b3,
	     a31 * b1 + a32 * b2 + a33 * b3)

	fun length (x, y, z): real = Math.sqrt (x * x + y * y + z * z)

	fun toUnit (x, y, z) =
	    let
		val k = Math.sqrt (x * x + y * y + z * z)
	    in
		(k * x, k * y, k * z)
	    end
    end
