structure Renderer :> RENDERER =
    struct
	open Geometry

	type color = Color.color

	datatype plane_face    = PlaneSurface
	datatype sphere_face   = SphereSurface
	datatype cube_face     =
	    CubeBottom | CubeTop | CubeFront | CubeBack | CubeLeft | CubeRight
	datatype cylinder_face = CylinderBottom | CylinderTop | CylinderSide
	datatype cone_face     = ConeBase | ConeSide

	type surface' =
	    point -> {color: color, diffuse: real, specular: real, phong: real}
	type 'face surface = 'face -> surface'

	datatype object =
	    Plane      of plane_face surface
	  | Sphere     of sphere_face surface
	  | Cube       of cube_face surface
	  | Cylinder   of cylinder_face surface
	  | Cone       of cone_face surface
	  | Union      of object * object
	  | Intersect  of object * object
	  | Difference of object * object
	  | Transform  of mat * mat * object

	datatype light =
	    Directional of color * vec
	  | Point       of color * point
	  | Spot        of color * point * point * angle * real

	type id = int
	type data = id * surface' * mat * (point -> vec)

	datatype object' =
	    Plane'      of mat * data
	  | Spheroid'   of mat * real * real * data
	  | Union'      of object' * object'
	  | Intersect'  of object' * object'
	  | Difference' of object' * object'

	exception Crash

	val epsilon = 1.0E~10

	val topMat = translationMat (0.0, 1.0, 0.0)
	val topMat' = translationMat (0.0, ~1.0, 0.0)
	val bottomMat = rotationXMat Math.pi
	val bottomMat' = bottomMat
	val frontMat = rotationXMat (~Math.pi / 2.0)
	val frontMat' = rotationXMat (Math.pi / 2.0)
	val backMat = mulMat (translationMat (0.0, 0.0, 1.0),
			      rotationXMat (Math.pi / 2.0))
	val backMat' = mulMat (rotationXMat (~Math.pi / 2.0),
			       translationMat (0.0, 0.0, ~1.0))
	val leftMat = rotationZMat (Math.pi / 2.0)
	val leftMat' = rotationZMat (~Math.pi / 2.0)
	val rightMat = mulMat (translationMat (1.0, 0.0, 0.0),
			       rotationZMat (~Math.pi / 2.0))
	val rightMat' = mulMat (rotationZMat (Math.pi / 2.0),
				translationMat (~1.0, 0.0, 0.0))

	local
	    val counter = ref 0
	in
	    fun newId () =
		let
		    val n = !counter
		in
		    counter := n + 1; n
		end
	end

	fun preprocess (Plane surface, o2w, w2o) =
	    let
		val normal = mulMatVec (o2w, (0.0, 1.0, 0.0))
	    in
		Plane' (w2o, (newId (), surface PlaneSurface, w2o,
			      fn _ => normal))
	    end
	  | preprocess (Sphere surface, o2w, w2o) =
	    let
		val center = mulMatPoint (o2w, (0.0, 0.0, 0.0))
	    in
		Spheroid' (w2o, 1.0, 1.0,
			   (newId (), surface SphereSurface, w2o,
			    fn v => subVec (v, center)))
	    end
	  | preprocess (Cube surface, o2w, w2o) =
	    let
		fun plane (o2w', w2o', face) =
		    let
			val normal =
			    mulMatVec (mulMat (o2w, o2w'), (0.0, 1.0, 0.0))
		    in
			Plane' (mulMat (w2o', w2o),
				(newId (), surface face, w2o, fn _ => normal))
		    end
		val top = plane (topMat, topMat', CubeTop)
		val bottom = plane (bottomMat, bottomMat', CubeBottom)
		val front = plane (frontMat, frontMat', CubeFront)
		val back = plane (backMat, backMat', CubeBack)
		val left = plane (leftMat, leftMat', CubeLeft)
		val right = plane (rightMat, rightMat', CubeRight)
	    in
		Intersect' (Intersect' (Intersect' (top, bottom),
					Intersect' (left, right)),
			    Intersect' (front, back))
	    end
	  | preprocess (Cylinder surface, o2w, w2o) =
	    let
		fun plane (o2w', w2o', face) =
		    let
			val normal =
			    mulMatVec (mulMat (o2w, o2w'), (0.0, 1.0, 0.0))
		    in
			Plane' (mulMat (w2o', w2o),
				(newId (), surface face, w2o, fn _ => normal))
		    end
		val top = plane (topMat, topMat', CylinderTop)
		val bottom = plane (bottomMat, bottomMat', CylinderBottom)
		fun normal v =
		    let
			val (v1, _, v3) = mulMatPoint (w2o, v)
		    in
			mulMatVec (o2w, (v1, 0.0, v3))
		    end
	    in
		Intersect' (Spheroid' (w2o, 0.0, 1.0,
				       (newId (), surface CylinderSide, w2o,
					normal)),
			    Intersect' (top, bottom))
	    end
	  | preprocess (Cone surface, o2w, w2o) =
	    let
		fun plane (o2w', w2o', face) =
		    let
			val normal =
			    mulMatVec (mulMat (o2w, o2w'), (0.0, 1.0, 0.0))
		    in
			Plane' (mulMat (w2o', w2o),
				(newId (), surface face, w2o, fn _ => normal))
		    end
		val top = plane (topMat, topMat', ConeBase)
		val bottom = plane (bottomMat, bottomMat', ConeSide)
		fun normal v =
		    let
			val (v1, _, v3) = mulMatPoint (w2o, v)
		    in
			mulMatVec (o2w, (v1, ~1.0, v3))
		    end
		val bound =
		    Transform (mulMat (translationMat (0.0, 0.5, 0.0),
				       uscaleMat 1.5),
			       mulMat (uscaleMat (2.0 / 3.0),
				       translationMat (0.0, ~0.5, 0.0)),
			       Sphere (fn _ => surface ConeSide))
	    in
		Intersect' (Intersect' (preprocess (bound, o2w, w2o),
					Spheroid' (w2o, ~1.0, 0.0,
						   (newId (), surface ConeSide,
						    w2o, normal))),
			    Intersect' (top, bottom))
	    end
	  | preprocess (Union (o1, o2), o2w, w2o) =
	    Union' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Intersect (o1, o2), o2w, w2o) =
	    Intersect' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Difference (o1, o2), o2w, w2o) =
	    Difference' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Transform (o2w', w2o', obj), o2w, w2o) =
	    preprocess (obj, mulMat (o2w, o2w'), mulMat (w2o', w2o))

	datatype intersection = Entry | Exit
	datatype which = A | B
	datatype wher = Outside | InA | InB | InAB

	fun merge (xs as (x as (l1, _, _))::xr, ys as (y as (l2, _, _))::yr) =
	    (case Real.compare (l1, l2) of
		 LESS => (x, A)::merge (xr, ys)
	       | EQUAL => (x, A)::(y, B)::merge (xr, yr)
	       | GREATER => (y, B)::merge (xs, yr))
	  | merge (nil, ys) = List.map (fn y => (y, B)) ys
	  | merge (xs, nil) = List.map (fn x => (x, A)) xs

	fun start ((_, _, Exit)::_, (_, _, Exit)::_) = InAB
	  | start ((_, _, Exit)::_, _) = InA
	  | start (_, (_, _, Exit)::_) = InB
	  | start (_, _) = Outside

	fun union (nil, ys) = ys
	  | union (xs, nil) = xs
	  | union (xs, ys) = union' (merge (xs, ys), start (xs, ys))
	and union' ((x as (_, _, Entry), A)::xr, Outside) = x::union' (xr, InA)
	  | union' ((x as (_, _, Entry), B)::xr, Outside) = x::union' (xr, InB)
	  | union' ((x as (_, _, Exit), A)::xr, InA) = x::union' (xr, Outside)
	  | union' ((x as (_, _, Entry), B)::xr, InA) = union' (xr, InAB)
	  | union' ((x as (_, _, Entry), A)::xr, InB) = union' (xr, InAB)
	  | union' ((x as (_, _, Exit), B)::xr, InB) = x::union' (xr, Outside)
	  | union' ((x as (_, _, Exit), A)::xr, InAB) = union' (xr, InB)
	  | union' ((x as (_, _, Exit), B)::xr, InAB) = union' (xr, InA)
	  | union' (nil, _) = nil
	  | union' (_, _) = raise Crash

	fun inter (_, nil) = nil
	  | inter (xs, ys) = inter' (merge (xs, ys), start (xs, ys))
	and inter' ((x as (_, _, Entry), A)::xr, Outside) = inter' (xr, InA)
	  | inter' ((x as (_, _, Entry), B)::xr, Outside) = inter' (xr, InB)
	  | inter' ((x as (_, _, Exit), A)::xr, InA) = inter' (xr, Outside)
	  | inter' ((x as (_, _, Entry), B)::xr, InA) = x::inter' (xr, InAB)
	  | inter' ((x as (_, _, Entry), A)::xr, InB) = x::inter' (xr, InAB)
	  | inter' ((x as (_, _, Exit), B)::xr, InB) = inter' (xr, Outside)
	  | inter' ((x as (_, _, Exit), A)::xr, InAB) = x::inter' (xr, InB)
	  | inter' ((x as (_, _, Exit), B)::xr, InAB) = x::inter' (xr, InA)
	  | inter' (nil, _) = nil
	  | inter' (_, _) = raise Crash

	fun diff (xs, nil) = xs
	  | diff (xs, ys) = diff' (merge (xs, ys), start (xs, ys))
	and diff' ((x as (k1, _, Entry), A)::xr, Outside) =
	    (case xr of
		 ((k2, _, Entry), B)::xrr =>
		     if k2 - k1 < 2.0 * epsilon then diff' (xrr, InAB)
		     else x::diff' (xr, InA)
	       | _ => x::diff' (xr, InA))
	  | diff' ((x as (_, _, Entry), B)::xr, Outside) = diff' (xr, InB)
	  | diff' ((x as (_, _, Exit), A)::xr, InA) = x::diff' (xr, Outside)
	  | diff' (((l, (id, surface, w2o, normal), Entry), B)::xr, InA) =
	    (l, (id, surface, w2o, negVec o normal), Exit)::diff' (xr, InAB)
	  | diff' ((x as (_, _, Entry), A)::xr, InB) = diff' (xr, InAB)
	  | diff' ((x as (_, _, Exit), B)::xr, InB) = diff' (xr, Outside)
	  | diff' ((x as (_, _, Exit), A)::xr, InAB) = diff' (xr, InB)
	  | diff' (((k1, (id, surface, w2o, normal), Exit), B)::xr, InAB) =
	    (case xr of
		 ((k2, _, Exit), A)::xrr =>
		     if k2 - k1 < 2.0 * epsilon then diff' (xrr, Outside)
		     else
			 (k1, (id, surface, w2o, negVec o normal), Entry)::
			 diff' (xr, InA)
	       | _ =>
		     (k1, (id, surface, w2o, negVec o normal), Entry)::
		     diff' (xr, InA))
	  | diff' (nil, _) = nil
	  | diff' (_, _) = raise Crash

	fun intersect' (Plane' (w2o, data), base, dir) =
	    let
		val (_, dy, _) = mulMatVec (w2o, dir)
	    in
		if Real.== (dy, 0.0) then nil
		else
		    let
			val (_, y, _) = mulMatPoint (w2o, base)
			val k = y / dy
		    in
			[(~k, data, if dy < 0.0 then Entry else Exit)]
		    end
	    end
	  | intersect' (Spheroid' (w2o, factor, dist, data), base, dir) =
	    let
		val x as (x1, x2, x3) = mulMatPoint (w2o, base)
		val x' = (x1, factor * x2, x3)
		val v as (v1, v2, v3) = mulMatVec (w2o, dir)
		val v' = (v1, factor * v2, v3)
		val divi = mulVec (v', v)
		val a = ~(mulVec (x', v)) / divi
		val arg = a * a - (mulVec (x', x) - dist) / divi
	    in
		if arg < 0.0 then nil
		else
		    let
			val b = Math.sqrt arg
			val k1 = a - b
			val k2 = a + b
		    in
			if k2 > ~epsilon then
			    if k1 > ~epsilon then
				[(k1, data, Entry), (k2, data, Exit)]
			    else
				[(k2, data, Exit)]
			else nil
		    end
	    end
	  | intersect' (Union' (obj1, obj2), base, dir) =
	    union (intersect' (obj1, base, dir), intersect' (obj2, base, dir))
	  | intersect' (Intersect' (obj1, obj2), base, dir) =
	    let
		val xs = intersect' (obj1, base, dir)
	    in
		if List.null xs then nil
		else inter (xs, intersect' (obj2, base, dir))
	    end
	  | intersect' (Difference' (obj1, obj2), base, dir) =
	    let
		val xs = intersect' (obj1, base, dir)
	    in
		if List.null xs then nil
		else diff (xs, intersect' (obj2, base, dir))
	    end

	fun dropPrefix (xs as (k, (id1, _, _, _), i)::xr) =
	    if k < ~epsilon then
		case xr of
		    (_, (id2, _, _, _), Exit)::_ =>
			if id1 = id2 then xs
			else dropPrefix xr
		  | _ => dropPrefix xr
	    else if i = Exit then dropPrefix xr
	    else xs
	  | dropPrefix nil = nil

	fun intersect (scene, base, dir) =
	    dropPrefix (intersect' (scene, base, dir))

	fun isShadowed ((k', _, Entry)::_, k: real) = k' < k
	  | isShadowed ((_, _, Exit)::_, _) = raise Crash
	  | isShadowed (nil, _) = false

	fun testLight (Directional (color, dir), scene, point) =
	    let
		val dir' = negVec dir
	    in
		if List.null (intersect (scene, point, dir')) then
		    SOME (color, dir')
		else NONE
	    end
	  | testLight (Point (color, pos), scene, point) =
	    let
		val dir = subVec (pos, point)
		val dist2 = mulVec (dir, dir)
	    in
		if dist2 < epsilon orelse
		    isShadowed (intersect (scene, point, dir), 1.0) then NONE
		else
		    let
			val attenuation = 100.0 / (99.0 + dist2)
		    in
			SOME (Color.scale (attenuation, color),
			      mulScalVec (1.0 / Math.sqrt dist2, dir))
		    end
	    end
	  | testLight (Spot (color, pos, at, cutoff, exp), scene, point) =
	    let
		val spotDir = subVec (at, pos)
		val unitSpotDir = normalizeVec spotDir
		val spotPointDir = subVec (point, pos)
		val dist = absVec spotPointDir
		val unitSpotPointDir = mulScalVec (1.0 / dist, spotPointDir)
		val orthoDist = mulVec (unitSpotDir, unitSpotPointDir)
	    in
		if Math.acos orthoDist > cutoff orelse
		    isShadowed (intersect (scene, point, negVec spotPointDir),
				1.0)
		then NONE
		else
		    let
			val attenuation1 = Math.pow (orthoDist, exp)
			val attenuation2 = 100.0 / (99.0 + dist * dist)
		    in
			SOME (Color.scale (attenuation1 * attenuation2, color),
			      negVec unitSpotPointDir)
		    end
	    end

	fun colorSum f (k, i) xs =
	    if Real.== (k, 0.0) then Color.black
	    else
		Color.scale
		(k, List.foldr (fn (x, sum) =>
				Color.add (sum, Color.nclamp (f x))) i xs)

	fun trace (base, dir, ambient, lights, scene, depth) =
	    case intersect (scene, base, dir) of
		(k, (_, surface, w2o, f), _)::_ =>
		    let
			val p =   (* intersection point *)
			    addVec (base, mulScalVec (k, dir))
			val n =   (* unit normal vector on surface *)
			    normalizeVec (f p)
			val {color = c, diffuse = kd,
			     specular = ks, phong = exp} =
			    surface (mulMatPoint (w2o, p))
			    handle Domein =>
				{color = Color.black, diffuse = 0.0,
				 specular = 0.0, phong = 1.0}
			val intensityDirList =
			    if Real.== (kd, 0.0) andalso Real.== (ks, 0.0)
			    then nil
			    else
				List.mapPartial
				(fn light => testLight (light, scene, p))
				lights
			val diffuseLighting =
			    colorSum
			    (fn (i, l) =>
			     Color.scale (mulVec (n, l), i))
			    (kd, ambient) intensityDirList
			val reflected =
			    if Real.== (ks, 0.0) orelse depth = 0 then
				Color.black
			    else
				trace (p,
				       addVec (dir,
					       mulScalVec (~2.0 *
							   mulVec (n, dir),
							   n)),
				       ambient, lights, scene, depth - 1)
			val d = normalizeVec dir
			val specularLighting =
			    colorSum
			    (fn (i, l) =>
			     let
				 val h = subVec (l, d)
				 val hlen = absVec h
				 val h0 =
				     if Real.== (hlen, 0.0) then n
				     else mulScalVec (1.0 / hlen, h)
				 val x = mulVec (n, h0)
			     in
				 if x <= 0.0 then Color.black
				 else Color.scale (Math.pow (x, exp), i)
			     end) (ks, reflected) intensityDirList
		    in
			Color.prod (Color.add (diffuseLighting,
					       specularLighting),
				    Color.nclamp c)
		    end
	      | nil => Color.black

	fun mkRender {ambient, lights, scene, vision, width, height, depth} =
	    let
		val scene' = preprocess (scene, unitMat, unitMat)
		val w = 2.0 * Math.tan (0.5 * vision)
		val delta = w / Real.fromInt width
		val h = delta * Real.fromInt height
		val top = (h + delta) / 2.0
		val left = ~(w + delta) / 2.0
		val base = (0.0, 0.0, ~1.0)
	    in
		fn (x, y) =>
		let
		    val dir = (left + delta * Real.fromInt x,
			       top - delta * Real.fromInt y,
			       1.0)
		    val c = trace (base, dir, ambient, lights, scene', depth)
		in
		    Color.nclamp (Color.clamp c)
		end
	    end
    end
