signature RENDERER =
sig
    type angle = Geometry.angle
    type point = Geometry.point
    type vec   = Geometry.vec
    type mat   = Geometry.mat
    type color = Color.color

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
	      Plane      of plane_face surface
	    | Sphere     of sphere_face surface
	    | Cube       of cube_face surface                     (* Tier 2 *)
	    | Cylinder   of cylinder_face surface                 (* Tier 2 *)
	    | Cone       of cone_face surface                     (* Tier 2 *)
	    | Union      of object * object
	    | Intersect  of object * object                       (* Tier 3 *)
	    | Difference of object * object                       (* Tier 3 *)
	    | Transform  of mat * mat * object

    datatype light =
	      Directional of color * vec
	    | Point       of color * point                        (* Tier 2 *)
	    | Spot        of color * point * point * angle * real (* Tier 3 *)

    val mkRender :
	    { ambient :   color
	    , lights :    light list
	    , scene :     object
	    , vision :    angle
	    , width :     int
	    , height :    int
	    , depth :     int
	    } -> int * int -> color
end
