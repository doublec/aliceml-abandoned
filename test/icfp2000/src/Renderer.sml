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

	type 'face surface =
	    'face -> point ->
	    {color: color, diffuse: real, specular: real, phong: real}

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
	    Plane'      of mat * mat * plane_face surface
	  | Sphere'     of mat * mat * sphere_face surface
	  | Union'      of object' * object'
	  | Intersect'  of object' * object'
	  | Difference' of object' * object'

	exception Crash

	fun preprocess (Plane surface, o2w, w2o) = Plane' (o2w, w2o, surface)
	  | preprocess (Sphere surface, o2w, w2o) = Sphere' (o2w, w2o, surface)
	  | preprocess (Cube surface, o2w, w2o) = raise Crash (*TODO*)
	  | preprocess (Cylinder surface, o2w, w2o) = raise Crash (*TODO*)
	  | preprocess (Cone surface, o2w, w2o) = raise Crash (*TODO*)
	  | preprocess (Union (o1, o2), o2w, w2o) =
	    Union' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Intersect (o1, o2), o2w, w2o) =
	    Intersect' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Difference (o1, o2), o2w, w2o) =
	    Difference' (preprocess (o1, o2w, w2o), preprocess (o2, o2w, w2o))
	  | preprocess (Transform (o2w', w2o', obj), o2w, w2o) =
	    preprocess (obj, mulMat (o2w', o2w), mulMat (w2o, w2o'))

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

	fun union ((x as (_, _, Entry), A)::xr, Outside) = x::union (xr, InA)
	  | union ((x as (_, _, Entry), B)::xr, Outside) = x::union (xr, InB)
	  | union ((x as (_, _, Exit), A)::xr, InA) = x::union (xr, Outside)
	  | union ((x as (_, _, Entry), B)::xr, InA) = union (xr, InAB)
	  | union ((x as (_, _, Entry), A)::xr, InB) = union (xr, InAB)
	  | union ((x as (_, _, Exit), B)::xr, InB) = x::union (xr, Outside)
	  | union ((x as (_, _, Exit), A)::xr, InAB) = union (xr, InB)
	  | union ((x as (_, _, Exit), B)::xr, InAB) = union (xr, InA)
	  | union (nil, _) = nil
	  | union (_, _) = raise Crash

	fun inter ((x as (_, _, Entry), A)::xr, Outside) = inter (xr, InA)
	  | inter ((x as (_, _, Entry), B)::xr, Outside) = inter (xr, InB)
	  | inter ((x as (_, _, Exit), A)::xr, InA) = inter (xr, Outside)
	  | inter ((x as (_, _, Entry), B)::xr, InA) = x::inter (xr, InAB)
	  | inter ((x as (_, _, Entry), A)::xr, InB) = x::inter (xr, InAB)
	  | inter ((x as (_, _, Exit), B)::xr, InB) = inter (xr, Outside)
	  | inter ((x as (_, _, Exit), A)::xr, InAB) = x::inter (xr, InB)
	  | inter ((x as (_, _, Exit), B)::xr, InAB) = x::inter (xr, InA)
	  | inter (nil, _) = nil
	  | inter (_, _) = raise Crash

	fun diff ((x as (_, _, Entry), A)::xr, Outside) = x::diff (xr, InA)
	  | diff ((x as (_, _, Entry), B)::xr, Outside) = diff (xr, InB)
	  | diff ((x as (_, _, Exit), A)::xr, InA) = x::diff (xr, Outside)
	  | diff (((l, s, Entry), B)::xr, InA) = (l, s, Exit)::diff (xr, InAB)
	  | diff ((x as (_, _, Entry), A)::xr, InB) = diff (xr, InAB)
	  | diff ((x as (_, _, Exit), B)::xr, InB) = diff (xr, Outside)
	  | diff ((x as (_, _, Exit), A)::xr, InAB) = diff (xr, InB)
	  | diff (((l, s, Exit), B)::xr, InAB) = (l, s, Entry)::diff (xr, InA)
	  | diff (nil, _) = nil
	  | diff (_, _) = raise Crash

	fun intersect (Plane' (o2w, w2o, surface), base, dir) =
	    let
		val (_, dy, _) = mulMatVec (w2o, dir)
	    in
		if dy >= 0.0 then nil
		else
		    let
			val (_, y, _) = mulMatPoint (w2o, base)
		    in
			[(y / ~dy,
			  (surface PlaneSurface,
			   fn _ => mulMatVec (o2w, (0.0, 1.0, 0.0))),
			  Entry)]
		    end
	    end
	  | intersect (Sphere' (o2w, w2o, surface), base, dir) =
	    let
		val base' = mulMatPoint (w2o, base)
		val dir' = mulMatVec (w2o, dir)
		val mtca = mulVec (dir', base')
	    in
		if mtca > 0.0 then nil
		else
		    let
			val d2 = mulVec (base', base') - mtca * mtca
		    in
			if d2 > 1.0 then nil
			else
			    let
				val tca = ~mtca
				val thc = Math.sqrt (1.0 - d2)
				val x = (surface SphereSurface,
					 fn v =>
					 mulMatVec (o2w, subVec (v, base')))
				val k = absVec dir'
			    in
				[((tca - thc) / k, x, Entry),
				 ((tca + thc) / k, x, Exit)]
			    end
		    end
	    end
(*
	  | intersect (Cylinder' (o2w, w2o, surface), base, dir) =
	    (* Possibilities:
	     *    2 intersections with bottom (dy = 0)
	     *    1 intersection with bottom
	     *         1 intersection with top - order!
	     *         1 intersection with side - order!
	     *         0 intersections with top/side (tangential)
	     *    0 intersections with bottom
	     *         2 intersections with top (dy = 0)
	     *         1 intersection with top
             *              1 intersection with side - order!
	     *              0 intersections with top/side
	     *         0 intersection with top
	     *              (runs along side: cannot happen)
	     *              2 intersections with side
	     *              1 intersections with side (tangential)
	     *              0 intersections with side
	     *)
	    let
		val base' as (x, y, z) = mulMatPoint (w2o, base)
		val dir' as (dx, dy, dz) = mulMatVec (w2o, dir)
	    in
		if Real.= (dy, 0.0) then
		    if y > 0.0 andalso y < 1.0 then
		    else if Real.= (y, 0.0) then
		    else if Real.= (y, 1.0) then
		    else nil
		else
		    let   (* test intersection with bottom *)
			val k = y / dy
			val bot_x = x + k * dx
			val bot_z = z + k * dz
		    in
			if bot_x * bot_x + bot_z * bot_z <= 1.0 then
			    k is intersection; test with top/side
			else   (* test intersection with top *)
			    let
				val k = (1.0 - y) / dy
				val top_x = x + k * dx
				val top_z = z + k * dz
			    in
				if top_x * top_x + top_z * top_z <= 1.0 then
				    k is intersection; test with side
				else   (* test intersection with top *)
			    end
		    end
	    end
*)
	  | intersect (Union' (obj1, obj2), base, dir) =
	    union (merge (intersect (obj1, base, dir),
			  intersect (obj2, base, dir)), Outside)
	  | intersect (Intersect' (obj1, obj2), base, dir) =
	    inter (merge (intersect (obj1, base, dir),
			  intersect (obj2, base, dir)), Outside)
	  | intersect (Difference' (obj1, obj2), base, dir) =
	    diff (merge (intersect (obj1, base, dir),
			 intersect (obj2, base, dir)), Outside)

	fun isShadowed ((k', _, Entry)::_, k: real) = k' < k
	  | isShadowed ((_, _, Exit)::_, _) = raise Crash
	  | isShadowed (nil, _) = false

	fun testLight (Directional (color, dir), scene, point) =
	    if List.null (intersect (scene, point, dir)) then SOME (color, dir)
	    else NONE
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
		val dir = subVec (pos, point)
		val spotDir = subVec (at, pos)
		val unitSpotDir = normalizeVec spotDir
		val phi = Math.acos (mulVec (spotDir, dir) / absVec dir)
	    in
		if phi > cutoff orelse
		    isShadowed (intersect (scene, point, dir), 1.0) then NONE
		else
		    let
			val dist = absVec dir
			val unitDir = mulScalVec (1.0 / dist, dir)
			val negUnitDir = negVec unitDir
			val attenuation1 =
			    Math.pow (mulVec (unitSpotDir, negUnitDir), exp)
			val attenuation2 = 100.0 / (99.0 + dist * dist)
		    in
			SOME (Color.scale (attenuation1 * attenuation2, color),
			      unitDir)
		    end
	    end

	fun colorSum f (k, i) xs =
	    if Real.== (k, 0.0) then Color.black
	    else
		Color.scale
		(k, List.foldr (fn (x, sum) => Color.add (sum, f x)) i xs)

	fun trace (base, dir, ambient, lights, scene, depth) =
	    case intersect (scene, base, dir) of
		(k, (surface, f), Entry)::_ =>
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
			    (fn (i, l) => Color.scale (mulVec (n, l), i))
			    (kd, ambient) intensityDirList
			val d = normalizeVec dir
			val reflected =
			    if Real.== (ks, 0.0) orelse depth = 0
			    then Color.black
			    else
				trace (p,
				       addVec (dir,
					       mulScalVec (2.0 *
							   mulVec (n, dir),
							   n)),
				       ambient, lights, scene, depth - 1)
			val specularLighting =
			    colorSum
			    (fn (i, l) =>
			     Color.scale (Math.pow
					  (0.5 * mulVec (n, subVec (l, d)),
					   exp), i))
			    (ks, reflected) intensityDirList
		    in
			Color.prod (Color.add (diffuseLighting,
					       specularLighting), c)
		    end
	      | (_, _, Exit)::_ => raise Crash
	      | nil => ambient

	fun mkRender {ambient, lights, scene, vision, width, height, depth} =
	    let
		val scene' = preprocess (scene, unitMat, unitMat)
		val w = 2.0 * Math.tan (0.5 * vision)
		val delta = w / Real.fromInt width
		val h = delta * Real.fromInt height
		val top = (h + delta) / 2.0
		val left = (w + delta) / 2.0
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
