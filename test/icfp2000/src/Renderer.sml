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
	type 'face surface =
	    'face -> surface'

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

	datatype object' =
	    Plane'      of mat * vec * surface'
	  | Sphere'     of mat * point * surface'
	  | Cylinder'   of mat * mat * surface'
	  | Cone'       of mat * mat * surface'
	  | Union'      of object' * object'
	  | Intersect'  of object' * object'
	  | Difference' of object' * object'

	exception Error of string
	exception Crash

	fun preprocess (Plane surface, o2w, w2o) =
	    Plane' (w2o, mulMatVec (o2w, (0.0, 1.0, 0.0)),
		    surface PlaneSurface)
	  | preprocess (Sphere surface, o2w, w2o) =
	    Sphere' (w2o, mulMatPoint (o2w, (0.0, 0.0, 0.0)),
		     surface SphereSurface)
	  | preprocess (Cube surface, o2w, w2o) =
	    let
		fun plane (o2w', w2o', face) =
		    Plane' (mulMat (w2o', w2o),
			    mulMatVec (mulMat (o2w, o2w'), (0.0, 1.0, 0.0)),
			    surface face)
		val top = plane (translationMat (0.0, 1.0, 0.0),
				 translationMat (0.0, ~1.0, 0.0), CubeTop)
		val rotXpi = rotationXMat Math.pi
		val bottom = plane (rotXpi, rotXpi, CubeBottom)
		val rotXpi2 = rotationXMat (Math.pi / 2.0)
		val rotXmpi2 = rotationXMat (~Math.pi / 2.0)
		val front = plane (rotXmpi2, rotXpi2, CubeFront)
		val back = plane (mulMat (translationMat (0.0, 0.0, 1.0),
					  rotXpi2),
				  mulMat (translationMat (0.0, 0.0, ~1.0),
					  rotXmpi2), CubeBack)
		val rotZpi2 = rotationZMat (Math.pi / 2.0)
		val rotZmpi2 = rotationZMat (~Math.pi / 2.0)
		val left = plane (rotZpi2, rotZmpi2, CubeLeft)
		val right = plane (mulMat (translationMat (1.0, 0.0, 0.0),
					   rotZmpi2),
				   mulMat (translationMat (~1.0, 0.0, 0.0),
					   rotZpi2), CubeBack)
	    in
		Intersect' (Intersect' (Intersect' (top, bottom),
					Intersect' (left, right)),
			    Intersect' (front, back))
	    end
	  | preprocess (Cylinder surface, o2w, w2o) = raise Crash (*TODO*)
	  | preprocess (Cone surface, o2w, w2o) = raise Crash (*TODO*)
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

	fun debug xs = print ("[" ^ debug' xs ^ "]\n")   (*DEBUG*)
	and debug' ((k, _, Entry)::rest) =
	    "Entry " ^ Real.toString k ^
	    (case rest of nil => "" | _::_ => ", " ^ debug' rest)
	  | debug' ((k, _, Exit)::rest) =
	    "Exit " ^ Real.toString k ^
	    (case rest of nil => "" | _::_ => ", " ^ debug' rest)
	  | debug' nil = ""

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

	fun inter (nil, _) = nil
	  | inter (_, nil) = nil
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

	fun diff (nil, _) = nil
	  | diff (xs, nil) = xs
	  | diff (xs, ys) = diff' (merge (xs, ys), start (xs, ys))
	and diff' ((x as (_, _, Entry), A)::xr, Outside) = x::diff' (xr, InA)
	  | diff' ((x as (_, _, Entry), B)::xr, Outside) = diff' (xr, InB)
	  | diff' ((x as (_, _, Exit), A)::xr, InA) = x::diff' (xr, Outside)
	  | diff' (((l, s, Entry), B)::xr, InA) = (l, s, Exit)::diff' (xr, InAB)
	  | diff' ((x as (_, _, Entry), A)::xr, InB) = diff' (xr, InAB)
	  | diff' ((x as (_, _, Exit), B)::xr, InB) = diff' (xr, Outside)
	  | diff' ((x as (_, _, Exit), A)::xr, InAB) = diff' (xr, InB)
	  | diff' (((l, s, Exit), B)::xr, InAB) = (l, s, Entry)::diff' (xr, InA)
	  | diff' (nil, _) = nil
	  | diff' (_, _) = raise Crash

	fun intersect' (Plane' (w2o, normal, surface), base, dir) =
	    let
		val (_, dy, _) = mulMatVec (w2o, dir)
		val (_, y, _) = mulMatPoint (w2o, base)
	    in
		if Real.== (dy, 0.0) then nil
		else
		    let
			val k = y / dy
		    in
			if k > 0.0 then nil
			else if dy > 0.0 then
			    [(~k, (surface, fn _ => normal), Exit)]
			else
			    [(~k, (surface, fn _ => normal), Entry)]
		    end
	    end
	  | intersect' (Sphere' (w2o, center, surface), base, dir) =
	    let
		val x = mulMatPoint (w2o, base)
		val v = mulMatVec (w2o, dir)
		val v2 = mulVec (v, v)
		val a = ~(mulVec (x, v)) / v2
		val arg = a * a - (mulVec (x, x) - 1.0) / v2
	    in
		if arg < 0.0 then nil
		else
		    let
			val b = Math.sqrt arg
			val k1 = a - b
			val k2 = a + b
		    in
			if k1 > 0.0 andalso k2 > 0.0 then
			    let
				val x = (surface, fn v => subVec (v, center))
			    in
				[(k1, x, Entry), (k2, x, Exit)]
			    end
			else if k2 > 0.0 then
			    let
				val x = (surface, fn v => subVec (v, center))
			    in
				[(k2, x, Exit)]
			    end
			else nil
		    end
	    end
	  | intersect' (Union' (obj1, obj2), base, dir) =
	    union (intersect' (obj1, base, dir), intersect' (obj2, base, dir))
	  | intersect' (Intersect' (obj1, obj2), base, dir) =
	    inter (intersect' (obj1, base, dir), intersect' (obj2, base, dir))
	  | intersect' (Difference' (obj1, obj2), base, dir) =
	    diff (intersect' (obj1, base, dir), intersect' (obj2, base, dir))

	fun dropWhile f xs = dropWhile' (xs, f)
	and dropWhile' (xs as x::xr, f) =
	    if f x then dropWhile' (xr, f) else xs
	  | dropWhile' (nil, _) = nil

	fun intersect (scene, base, dir) =
	    dropWhile (fn (_, _, i) => i = Exit)
	    (intersect' (scene, base, dir))

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
	    in
		if isShadowed (intersect (scene, point, dir), 1.0) then NONE
		else
		    let
			val dist = absVec dir
			val attenuation = 100.0 / (99.0 + dist * dist)
		    in
			SOME (Color.scale (attenuation, color),
			      normalizeVec dir)
		    end
	    end
	  | testLight (Spot (color, pos, at, cutoff, exp), scene, point) =
	    let
		val spotDir = subVec (at, pos)
		val unitSpotDir = normalizeVec spotDir
		val spotPointDir = subVec (pos, point)
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
			val attenuation1 = Math.pow (~orthoDist, exp)
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
				Color.add (sum, Color.clamp (f x))) i xs)

	fun trace (base, dir, ambient, lights, scene, depth) =
	    case intersect (scene, base, dir) of
		(k, (surface, f), _)::_ =>
		    let
			val p =   (* intersection point *)
			    addVec (base, mulScalVec (k, dir))
			val n =   (* unit normal vector on surface *)
			    normalizeVec (f p)
			val {color = c, diffuse = kd,
			     specular = ks, phong = exp} = surface p
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
(*(TextIO.print ("n = " ^ Real.toString (#1 n) ^
", " ^ Real.toString (#2 n) ^ ", " ^ Real.toString (#3 n) ^ "\n");
TextIO.print ("l = " ^ Real.toString (#1 l) ^
", " ^ Real.toString (#2 l) ^ ", " ^ Real.toString (#3 l) ^ "\n");*)
			     Color.scale (mulVec (n, l), i))
(*)*)
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
			     Color.scale (Math.pow
					  (0.5 * mulVec (n, subVec (l, d)),
					   exp), i))
			    (ks, reflected) intensityDirList
(*val _ = TextIO.print ("diffuse = " ^ Real.toString (#red diffuseLighting) ^
", " ^ Real.toString (#green diffuseLighting) ^ ", " ^ Real.toString (#blue diffuseLighting) ^ "\n")
val _ = TextIO.print ("specular = " ^ Real.toString (#red specularLighting) ^
", " ^ Real.toString (#green specularLighting) ^ ", " ^ Real.toString (#blue specularLighting) ^ "\n")
val _ = TextIO.print ("|lights| = " ^ Int.toString (List.length intensityDirList) ^ "\n")*)
		    in
			Color.prod (Color.add (diffuseLighting,
					       specularLighting), c)
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
		    Color.clamp c
		end
	    end
    end
