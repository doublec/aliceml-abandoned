signature RENDERER =
sig
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
	      Plane      of plane_face surface * point * vector
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

    val render :
	    { ambient :   color
	    , lights :    light list
	    , scene :     object
	    , vision :    angle
	    , width :     int
	    , height :    int
	    , depth :     int
	    , outstream : BinIO.outstream } -> unit
end
