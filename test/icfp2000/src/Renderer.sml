signature VECT =
    sig
	type t = real * real * real

	val add: t * t -> t
	val sub: t * t -> t
	val scale: real * t -> t
	val dotProd: t * t -> real
	val vecProd: t * t -> t
	val det: t * t * t -> real
    end

structure Vect :> VECT =
    struct
	type t = real * real * real

	fun add ((ax, ay, az), (bx, by, bz)): t = (ax + bx, ay + by, az + bz)

	fun sub ((ax, ay, az), (bx, by, bz)): t = (ax - bx, ay - by, az - bz)

	fun scale (k, (x, y, z)): t = (k * x, k * y, k * z)

	fun dotProd ((ax, ay, az), (bx, by, bz)): real =
	    ax * bx + ay * by + az * bz

	fun vecProd ((ax, ay, az), (bx, by, bz)): t =
	    (ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx)

	fun det ((ax, ay, az), (bx, by, bz), (cx, cy, cz)): real =
	    az * by * cz + bx * cy * az + cx * ay * bz -
	    ax * cy * bz - cx * by * az - bx * ay * cz
    end

structure Renderer (*:> RENDERER*) =
    struct
    type angle   = real
    type point   = real * real * real
    type vector  = real * real * real
    type stretch = vector * vector * vector
    type color   = {red : real, green : real, blue : real}

    datatype plane_face    = PlaneSurface
    datatype sphere_face   = SphereSurface
    datatype cube_face     = CubeBottom | CubeTop | CubeFront | CubeBack
			   | CubeLeft | CubeRight
    datatype cylinder_face = CylinderBottom | CylinderTop | CylinderSide
    datatype cone_face     = ConeBase | ConeSide

    type 'face surface =
	    'face -> point ->
	    { color :    color
	    , diffuse :  real
	    , specular : real
	    , phong :    real }

    datatype object =
	      Plane      of plane_face surface * vector * real
	    | Sphere     of sphere_face surface * point * real
	    | Ellipsoid  of sphere_face surface * point * stretch
	    | Cube       of cube_face surface * point * stretch     (* Tier 2 *)
	    | Cylinder   of cylinder_face surface * point * stretch (* Tier 2 *)
	    | Cone       of cone_face surface * point * stretch     (* Tier 2 *)
	    | Union      of object * object
	    | Intersect  of object * object                         (* Tier 3 *)
	    | Difference of object * object                         (* Tier 3 *)

    datatype light =
	      Directional of color * vector
	    | Point       of color * point                          (* Tier 2 *)
	    | Spot        of color * point * point * angle * real   (* Tier 3 *)

	exception Crash

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

	fun intersect (Plane (surface, normal, dist), base', dir') =
	    let
		val d = Vect.dotProd (normal, dir')
	    in
		if d >= 0.0 then nil
		else
		    [(~(Vect.dotProd (normal, base') + dist) / d,
		      (surface PlaneSurface, fn _ => normal), Entry)]
	    end
	  | intersect (Sphere (surface, base, r), base', dir') =
	    let
		val u = Vect.sub (base, base')
		val lambda = Vect.dotProd (dir', u)
		val e = Vect.dotProd (dir', dir')
		val d = lambda * lambda - e * (Vect.dotProd (u, u) - r * r)
	    in
		if d >= 0.0 then
		    let
			val lambda0 = lambda / e
			val x = (surface SphereSurface,
				 fn v => Vect.sub (v, base))
		    in
			if lambda0 - d >= 0.0 then
			    [(lambda0 - d, x, Entry), (lambda0 + d, x, Exit)]
			else if lambda0 + d >= 0.0 then
			    [(lambda0 + d, x, Entry), (lambda0 - d, x, Exit)]
			else nil
		    end
		else nil
	    end
	  | intersect (Union (obj1, obj2), base', dir') =
	    union (merge (intersect (obj1, base', dir'),
			  intersect (obj2, base', dir')), Outside)
	  | intersect (Intersect (obj1, obj2), base', dir') =
	    inter (merge (intersect (obj1, base', dir'),
			  intersect (obj2, base', dir')), Outside)
	  | intersect (Difference (obj1, obj2), base', dir') =
	    diff (merge (intersect (obj1, base', dir'),
			 intersect (obj2, base', dir')), Outside)

	fun render {ambient, lights, scene, vision, width, height, depth} =
	    let
		fun trace (base, dir) =
		    case intersect (scene, base, dir) of
			(lambda, (surface, f), Entry)::_ =>
			    let
				val p =   (* intersection point *)
				    Vect.add (base, Vect.scale (lambda, dir))
				val n = f p   (* normal vector on surface *)
				val {color = c, diffuse = kd,
				     specular = ks, phong = n} = surface p
			    in
				ambient (*UNF*)
			    end
		      | (_, _, Exit)::_ => raise Crash
		      | nil => ambient

		fun render' (x, y) =
		    {red = 0.0, green = 0.0, blue = 0.0}   (*UNFINISHED*)
	    in
		render'
	    end
    end
