signature MACHINE =
sig
    type id = string
    type operator

    datatype token =
	  Operator    of operator
	| Id          of id
	| Binder      of id
	| IntConst    of int
	| RealConst   of real
	| StringConst of string
	| Array       of token list
	| Function    of token list

    datatype program = Program of token list

    exception Error of string

    val operator : string -> operator option
    val run :      program -> unit
end


structure Machine :> MACHINE =
struct

  (* Operators *)

    datatype operator =
	  Apply
	| False | True | If
	| EqI | LessI | AddI | SubI | NegI | MulI | DivI | ModI
	| EqF | LessF | AddF | SubF | NegF | MulF | DivF
	| RealI | Floor | Frac | ClampF
	| Sqrt | Sin | Cos | Asin | Acos
	| Point | GetX | GetY | GetZ
	| Get | Length
	| Plane | Sphere | Cube | Cone | Cylinder
	| Union | Intersect | Difference
	| Translate | Scale | Uscale | RotateX | RotateY | RotateZ
	| Light | Pointlight | Spotlight
	| Render

    fun operator "acos"		= SOME Acos
      | operator "addf"		= SOME AddF
      | operator "addi"		= SOME AddI
      | operator "apply"	= SOME Apply
      | operator "asin"		= SOME Asin
      | operator "clampf"	= SOME ClampF
      | operator "cone"		= SOME Cone
      | operator "cos"		= SOME Cos
      | operator "cube"		= SOME Cube
      | operator "cylinder"	= SOME Cylinder
      | operator "difference"	= SOME Difference
      | operator "divf"		= SOME DivF
      | operator "divi"		= SOME DivI
      | operator "eqf"		= SOME EqF
      | operator "eqi"		= SOME EqI
      | operator "false"	= SOME False
      | operator "floor"	= SOME Floor
      | operator "frac"		= SOME Frac
      | operator "get"		= SOME Get
      | operator "getx"		= SOME GetX
      | operator "gety"		= SOME GetY
      | operator "getz"		= SOME GetZ
      | operator "if"		= SOME If
      | operator "intersect"	= SOME Intersect
      | operator "length"	= SOME Length
      | operator "lessf"	= SOME LessF
      | operator "lessi"	= SOME LessI
      | operator "light"	= SOME Light
      | operator "modi"		= SOME ModI
      | operator "mulf"		= SOME MulF
      | operator "muli"		= SOME MulI
      | operator "negf"		= SOME NegF
      | operator "negi"		= SOME NegI
      | operator "plane"	= SOME Plane
      | operator "point"	= SOME Point
      | operator "pointlight"	= SOME Pointlight
      | operator "real"		= SOME RealI
      | operator "reander"	= SOME Render
      | operator "rotatex"	= SOME RotateX
      | operator "rotatey"	= SOME RotateY
      | operator "rotatez"	= SOME RotateZ
      | operator "scale"	= SOME Scale
      | operator "sin"		= SOME Sin
      | operator "sphere"	= SOME Sphere
      | operator "spotlight"	= SOME Spotlight
      | operator "sqrt"		= SOME Sqrt
      | operator "subf"		= SOME SubF
      | operator "subi"		= SOME SubI
      | operator "translate"	= SOME Translate
      | operator "true"		= SOME True
      | operator "union"	= SOME Union
      | operator "uscale"	= SOME Uscale
      | operator  _		= NONE


  (* Programs *)

    type id = string

    datatype token =
	  Operator    of operator
	| Id          of id
	| Binder      of id
	| IntConst    of int
	| RealConst   of real
	| StringConst of string
	| Array       of token list
	| Function    of token list

    datatype program = Program of token list


  (* Values *)

    structure Env = BinaryMapFn(type ord_ky = id  val compare = String.compare)

    datatype value =
	  Bool    of bool
	| Int     of int
	| Real    of real
	| String  of string
	| Closure of env * token list
	| Vector  of value vector
	| Point   of real * real * real
	| Object  of Renderer.Object
	| Light   of Renderer.Light

    withtype env = value Env.map


  (* Helpers *)

    fun degToRad r = r * Math.pi / 180.0
    fun radToDeg r = r * 180.0 / Math.pi

    val unitMatrix = ( (1.0, 0.0, 0.0, 0.0)
		     , (0.0, 1.0, 0.0, 0.0)
		     , (0.0, 0.0, 1.0, 0.0)
		     )


  (* Evaluation *)

    exception Error of string

    fun typeError() = raise Error "stack underflow or type error"

    fun run(Program code) =
	ignore(eval(Env.empty, nil, code))
	handle Option.Option     => raise Error "unbound identifier"
	     | General.Subscript => raise Error "index out of bounds"
	     | IO.Io _           => raise Error "i/o error"

    and eval(env, stack, nil)     = stack
      | eval(env, stack, v::code) =
	case v of
	  IntConst i    => eval(env, Int i :: stack, code)
	| RealConst r   => eval(env, Real r :: stack, code)
	| StringConst s => eval(env, String s :: stack, code)
	| Binder x      => eval(Env.insert(env, x, hd stack), tl stack, code)
	| Id x          => eval(env, valOf(Env.find(env, x)) :: stack, code)
	| Function c    => eval(env, Closure(env, c) :: stack, code)
	| Array c       => let val vs = List.rev(eval(env, nil, c)) in
			    eval(env, Vector(Vector.fromList vs) :: stack, code)
			   end
	| Operator y    => let val stack' = evalOp(stack, y) in
			    eval(env, stack', code)
			   end

    and evalOp(Apply, Closure(env, code) :: stack) = eval(env, stack, code)
      | evalOp(False, stack)                       = Bool false :: stack
      | evalOp(True,  stack)                       = Bool true :: stack
      | evalOp(If, Closure(env2,code2)::Closure(env1,code1)::Bool b :: stack) =
	if b then eval(env1, stack, code1)
	     else eval(env2, stack, code2)

      | evalOp(EqI,   stack) = evalIntIntToBool (stack, op=)
      | evalOp(LessI, stack) = evalIntIntToBool (stack, Int.<)
      | evalOp(AddI,  stack) = evalIntIntToInt  (stack, Int.+)
      | evalOp(SubI,  stack) = evalIntIntToInt  (stack, Int.-)
      | evalOp(NegI,  stack) = evalIntToInt     (stack, Int.~)
      | evalOp(MulI,  stack) = evalIntIntToInt  (stack, Int.*)
      | evalOp(DivI,  stack) = evalIntIntToInt  (stack, Int.quot)
      | evalOp(ModI,  stack) = evalIntIntToInt  (stack, Int.rem)

      | evalOp(EqF,   stack) = evalRealRealToBool (stack, Real.==)
      | evalOp(LessF, stack) = evalRealRealToBool (stack, Real.<)
      | evalOp(AddF,  stack) = evalRealRealToReal (stack, Real.+)
      | evalOp(SubF,  stack) = evalRealRealToReal (stack, Real.-)
      | evalOp(NegF,  stack) = evalRealToReal     (stack, Real.~)
      | evalOp(MulF,  stack) = evalRealRealToReal (stack, Real.*)
      | evalOp(DivF,  stack) = evalRealRealToReal (stack, Real.quot)

      | evalOp(RealI, stack) = evalIntToReal  (stack, Real.fromInt)
      | evalOp(Floor, stack) = evalRealToInt  (stack, Real.floor)
      | evalOp(Frac,  stack) = evalRealToReal (stack, Real.realMod)
      | evalOp(ClampF,stack) = evalRealToReal (stack, fn r =>
						    if r < 0.0 then 0.0 else
						    if r > 1.0 then 1.0 else r)
      | evalOp(Sqrt,  stack) = evalRealToReal (stack, Math.sqrt)
      | evalOp(Sin,   stack) = evalRealToReal (stack, Math.sin o radToDeg)
      | evalOp(Cos,   stack) = evalRealToReal (stack, Math.cos o radToDeg)
      | evalOp(Asin,  stack) = evalRealToReal (stack, degToRad o Math.asin)
      | evalOp(Acos,  stack) = evalRealToReal (stack, degToRad o Math.acos)

      | evalOp(Point, Real(z)::Real(y)::Real(x)::stack) = Point(x,y,z) :: stack
      | evalOp(GetX, Point(x,y,z) :: stack) = Real x :: stack
      | evalOp(GetY, Point(x,y,z) :: stack) = Real y :: stack
      | evalOp(GetZ, Point(x,y,z) :: stack) = Real z :: stack

      | evalOp(Get,    Int i :: Array a :: stack) = Vector.sub(a, i) :: stack
      | evalOp(Length, Array a :: stack)          = Vector.length a :: stack

      | evalOp(Plane,    stack) = evalPrimObj(stack, Renderer.Plane, planeFace)
      | evalOp(Sphere,   stack) = evalPrimObj(stack, Renderer.Sphere,sphereFace)
      | evalOp(Cube,     stack) = evalPrimObj(stack, Renderer.Cube, cubeFace)
      | evalOp(Cone,     stack) = evalPrimObj(stack, Renderer.Cone, coneFace)
      | evalOp(Cylinder, stack) = evalPrimObj(stack, Renderer.Cylinder,
						     cylinderFace)
      | evalOp(Union,      stack) = evalConstrObj(stack, Renderer.Union)
      | evalOp(Intersect,  stack) = evalConstrObj(stack, Renderer.Intersect)
      | evalOp(Difference, stack) = evalConstrObj(stack, Renderer.Difference)

      | evalOp(Translate,  stack) = evalTransformVec(stack, translate)
      | evalOp(Scale,      stack) = evalTransformVec(stack, scale)
      | evalOp(Uscale,     stack) = evalTransformReal(stack, uscale)
      | evalOp(RotateX,    stack) = evalTransformAngle(stack, rotateX)
      | evalOp(RotateY,    stack) = evalTransformAngle(stack, rotateY)
      | evalOp(RotateZ,    stack) = evalTransformAngle(stack, rotateZ)

      | evalOp(Light,      stack) = evalLight(stack, Renderer.Directional)
      | evalOp(Light, Point(r,g,b) :: Point dir :: stack) =
	    Light(Renderer.Directional({red=r, green=g, blue=b}, dir)) :: stack
      | evalOp(Pointlight, stack) = evalLight(stack, Renderer.Point)
      | evalOp(Spotlight,  stack) = evalSpotlight(stack, Renderer.Spot)

      | evalOp(Render, String filename :: Int height :: Int width ::
		       Real vision :: Int depth :: Object scene ::
		       Array lights :: Point(r,g,b) :: stack) =
	let
	    val file   = BinIO.openOut filename
	    val header = "P6 # Helikopter\n" ^
			 Int.toString width ^" "^ Int.toString height ^ " 255\n"
	in
	    BinIO.output(Word8Vector.tabulate(String.size header,
		fn i => Word8.fromInt(Char.ord(String.sub(header, i)))));
	    Renderer.render
		{ ambient   = {red = r, green = g, blue = b}
		, lights    = Vector.toList lights
		, scene     = scene
		, vision    = degToRad vision
		, width     = width
		, height    = height
		, depth     = depth
		, outstream = file
		};
	    BinIO.closeOut file;
	    stack
	end

      | evalOp _ = typeError()

  (* Artihmetic Functions *)

    and evalIntToInt(Int i :: stack, f)   = Int(f i) :: stack
      | evalIntToInt _                    = typeError()
    and evalIntToReal(Int i :: stack, f)  = Real(f i) :: stack
      | evalIntToReal _                   = typeError()
    and evalRealToInt(Real r :: stack, f) = Int(f r) :: stack
      | evalRealToInt _                   = typeError()

    and evalIntIntToInt(Int i2 :: Int i1 :: stack, f)    = Int(f(i1,i2))::stack
      | evalIntIntToInt _                                = typeError()
    and evalIntIntToBool(Int i2 :: Int i1 :: stack, f)   = Bool(f(i1,i2))::stack
      | evalIntIntToBool _                               = typeError()
    and evalRealRealToReal(Real(r2)::Real(r1)::stack, f) = Real(f(r1,r2))::stack
      | evalRealRealToReal _                             = typeError()
    and evalRealRealToBool(Real(r2)::Real(r1)::stack, f) = Bool(f(r1,r2))::stack
      | evalRealRealToBool _                             = typeError()

  (* Primitive Objects *)

    and evalPrimObj(Closure(env, code) :: stack, RenderObj, face) =
	let
	    (* UNFINISHED: do optimizations *)
	    fun surface(i,u,v) = 
		case eval(env, [Real v, Real u, Int i], code) of
		  [Real n, Real ks, Real kd, Point(r,g,b)] =>
			{ color    = {red = r, green = g, blue = b}
			, diffuse  = kd
			, specular = ks
			, phong    = n
			}
		| _ => typeError()
	in
	    Object(RenderObj(unitMatrix, unitMatrix, surface o face)) :: stack
	end
      | evalPrimObj _ = typeError()

  (* Constructive Objects *)

    and evalConstrObj(Object obj2 :: Object obj1 :: stack, RenderConstr) =
	    Object(RenderConstr(obj1, obj2)) :: stack
      | evalConstrObj _ = typeError()

  (* Transformations *)

    and evalTransformReal(Real r :: Object obj :: stack, transform) =
	    Object(transform(obj, r)) :: stack
      | evalTransformReal _ = typeError()

    and evalTransformAngle(Real r :: Object obj :: stack, transform) =
	    Object(transform(obj, degToRad r)) :: stack
      | evalTransformAngle _ = typeError()

    and evalTransformVec(Real z :: Real y :: Real x :: Object obj :: stack,
			 transform) =
	    Object(transform(obj, x, y, z)) :: stack
      | evalTransformVec _ = typeError()

  (* Lights *)

    and evalLight(Point(r,g,b) :: Point pos :: stack, LightConstr) =
	    Light(LightConstr({red = r, green = g, blue = b}, pos)) :: stack
      | evalLight _ = typeError()

    and evalSpotlight(Real exp :: Real cutoff :: Point(r,g,b) :: Point at ::
		      Point pos :: stack, LightConstr) =
	    Light(LightConstr({red = r, green = g, blue = b}, pos, at,
			      degToRad cutoff, exp)) :: stack
      | evalSpotlight _ = typeError()

end
