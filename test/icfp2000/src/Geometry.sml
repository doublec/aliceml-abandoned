signature GEOMETRY =
    sig
	type angle = real (* radiant *)
	type vec   = real * real * real
	type point = vec
	type mat

	val addVec: vec * vec -> vec
	val subVec: vec * vec -> vec
	val mulScalVec: real * vec -> vec
	val mulVec: vec * vec -> real
	val mulMatVec: mat * vec -> vec
	val mulMatPoint: mat * point -> point
	val absVec: vec -> real
	val normalizeVec: vec -> vec

	val unitMat: mat
	val translationMat: vec -> mat
	val invTranslationMat: vec -> mat
	val scaleMat: vec -> mat
	val invScaleMat: vec -> mat
	val rotationXMat: angle -> mat
	val rotationYMat: angle -> mat
	val rotationZMat: angle -> mat
	val invRotationXMat: angle -> mat
	val invRotationYMat: angle -> mat
	val invRotationZMat: angle -> mat
	val mulMat: mat * mat -> mat
    end


structure Geometry :> GEOMETRY =
    struct
	open Real

	type angle = real (* radiant *)
	type vec   = real * real * real
	type row   = real * real * real * real
	type mat   = row * row * row
	type point = vec

	fun addVec ((ax, ay, az), (bx, by, bz)) =
	    (ax + bx, ay + by, az + bz)

	fun subVec ((ax, ay, az), (bx, by, bz)) =
	    (ax - bx, ay - by, az - bz)

	fun mulScalVec (k, (x, y, z)) = (k * x, k * y, k * z)

	fun mulVec ((ax, ay, az), (bx, by, bz)) =
	    ax * bx + ay * by + az * bz

	fun mulMatVec (((a11, a12, a13, _),
			(a21, a22, a23, _),
			(a31, a32, a33, _)), (b1, b2, b3)) =
	    (a11 * b1 + a12 * b2 + a13 * b3,
	     a21 * b1 + a22 * b2 + a23 * b3,
	     a31 * b1 + a32 * b2 + a33 * b3)

	fun mulMatPoint (((a11, a12, a13, a14),
			  (a21, a22, a23, a24),
			  (a31, a32, a33, a34)), (b1, b2, b3)) =
	    (a11 * b1 + a12 * b2 + a13 * b3 + a14,
	     a21 * b1 + a22 * b2 + a23 * b3 + a24,
	     a31 * b1 + a32 * b2 + a33 * b3 + a34)

	fun absVec (x, y, z) = Math.sqrt (x * x + y * y + z * z)

	fun normalizeVec (x, y, z) =
	    let
		val k = Math.sqrt (x * x + y * y + z * z)
	    in
		(k * x, k * y, k * z)
	    end
    end
