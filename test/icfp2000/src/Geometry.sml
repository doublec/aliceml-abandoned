signature GEOMETRY =
    sig
	type angle = real (* radiant *)
	type vec   = real * real * real
	type point = vec
	type mat

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
	val translationMat: vec -> mat
	val scaleMat: vec -> mat
	val uscaleMat: real -> mat
	val rotationXMat: angle -> mat
	val rotationYMat: angle -> mat
	val rotationZMat: angle -> mat
	val mulMat: mat * mat -> mat
    end


structure Geometry :> GEOMETRY =
    struct
	open Real
	open Math

	type angle = real (* radiant *)
	type vec   = real * real * real
	type mat   = real * real * real * real
		   * real * real * real * real
		   * real * real * real * real
	type point = vec

	fun negVec (x1, x2, x3)                 = ( ~x1,    ~x2,    ~x3  )
	fun rezVec (x1, x2, x3)                 = (1.0/x1, 1.0/x2, 1.0/x3)
	fun mulScalVec (k, (x1, x2, x3))        = (  k*x1,   k*x2,   k*x3)
	fun addVec ((x1, x2, x3), (y1, y2, y3)) = ( x1+y1,  x2+y2,  x3+y3)
	fun subVec ((x1, x2, x3), (y1, y2, y3)) = ( x1-y1,  x2-y2,  x3-y3)
	fun mulVec ((x1, x2, x3), (y1, y2, y3)) =   x1*y1 + x2*y2 + x3*y3

	fun mulMatVec ((a11, a12, a13, _,
			a21, a22, a23, _,
			a31, a32, a33, _), (x1, x2, x3)) =
	    (a11*x1 + a12*x2 + a13*x3,
	     a21*x1 + a22*x2 + a23*x3,
	     a31*x1 + a32*x2 + a33*x3)

	fun mulMatPoint ((a11, a12, a13, a14,
			  a21, a22, a23, a24,
			  a31, a32, a33, a34), (x1, x2, x3)) =
	    (a11*x1 + a12*x2 + a13*x3 + a14,
	     a21*x1 + a22*x2 + a23*x3 + a24,
	     a31*x1 + a32*x2 + a33*x3 + a34)

	fun absVec (x1, x2, x3) = sqrt (x1*x1 + x2*x2 + x3*x3)

	fun normalizeVec (x1, x2, x3) =
	    let
		val k = 1.0 / sqrt (x1*x1 + x2*x2 + x3*x3)
	    in
		(k*x1, k*x2, k*x3)
	    end


	val unitMat =
	    (1.0, 0.0, 0.0, 0.0,
	     0.0, 1.0, 0.0, 0.0,
	     0.0, 0.0, 1.0, 0.0)

        fun translationMat (tx,ty,tz) =
	    (1.0, 0.0, 0.0, tx,
	     0.0, 1.0, 0.0, ty,
	     0.0, 0.0, 1.0, tz)

        fun scaleMat (sx,sy,sz) =
	    ( sx, 0.0, 0.0, 0.0,
	     0.0,  sy, 0.0, 0.0,
	     0.0, 0.0,  sz, 0.0)

	fun uscaleMat s = scaleMat(s,s,s)

	fun rotationXMat a =
	    let
		val sina = sin a
		val cosa = cos a
	    in
		(1.0,  0.0,  0.0,  0.0,
		 0.0, cosa, ~sina, 0.0,
		 0.0, sina,  cosa, 0.0)
	    end

	fun rotationYMat a =
	    let
		val sina = sin a
		val cosa = cos a
	    in
		(cosa, 0.0, sina, 0.0,
		 0.0,  1.0,  0.0, 0.0,
		~sina, 0.0, cosa, 0.0)
	    end

	fun rotationZMat a =
	    let
		val sina = sin a
		val cosa = cos a
	    in
		(cosa, ~sina, 0.0, 0.0,
		 sina,  cosa, 0.0, 0.0,
		  0.0,  0.0,  1.0, 0.0)
	    end

	fun mulMat ((a11, a12, a13, a14,
		     a21, a22, a23, a24,
		     a31, a32, a33, a34),
		    (b11, b12, b13, b14,
		     b21, b22, b23, b24,
		     b31, b32, b33, b34)) =
	    (a11*b11 + a12*b21 + a13*b31,
	     a11*b12 + a12*b22 + a13*b32,
	     a11*b13 + a12*b23 + a13*b33,
	     a11*b14 + a12*b24 + a13*b34 + a14,
	     a21*b11 + a22*b21 + a23*b31,
	     a21*b12 + a22*b22 + a23*b32,
	     a21*b13 + a22*b23 + a23*b33,
	     a21*b14 + a22*b24 + a23*b34 + a24,
	     a31*b11 + a32*b21 + a33*b31,
	     a31*b12 + a32*b22 + a33*b32,
	     a31*b13 + a32*b23 + a33*b33,
	     a31*b14 + a32*b24 + a33*b34 + a34)
    end
