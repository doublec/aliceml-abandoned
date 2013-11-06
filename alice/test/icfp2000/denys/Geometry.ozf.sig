signature GEOMETRY_COMPONENT =
    sig
	structure Geometry:
	    sig
		type angle = real (* radiant *)
		type vec
		type point = vec
		type mat

		val vec: real * real * real -> vec
		val getX: vec -> real
		val getY: vec -> real
		val getZ: vec -> real

		val negVec: vec -> vec
		val rezVec: vec -> vec
		val addVec: vec * vec -> vec
		val subVec: vec * vec -> vec
		val mulScalVec: real * vec -> vec
		val mulVec: vec * vec -> real
		val mulMatVec: mat * vec -> vec
		val mulMatPoint: mat * point -> point
		val absVec: vec -> real
		val normalizeVec: vec -> vec

		val unitMat: mat
		val translationMat: real * real * real -> mat
		val scaleMat: real * real * real -> mat
		val uscaleMat: real -> mat
		val rotationXMat: angle -> mat
		val rotationYMat: angle -> mat
		val rotationZMat: angle -> mat
		val mulMat: mat * mat -> mat
	    end
    end
