signature RENDERER =
sig
    type angle  = real (* radiant *)
    type point  = Vector.t
    type vector = Vector.t
    type matrix = Vector.matrix
    type color  = Color.t

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
	      Plane      of matrix * matrix * plane_face surface
	    | Sphere     of matrix * matrix * sphere_face surface
	    | Cube       of matrix * matrix * cube_face surface     (* Tier 2 *)
	    | Cylinder   of matrix * matrix * cylinder_face surface (* Tier 2 *)
	    | Cone       of matrix * matrix * cone_face surface     (* Tier 2 *)
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
	    , depth :     int } -> (int * int -> color)
end
