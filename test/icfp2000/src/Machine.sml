signature MACHINE =
sig
    type id = string
    type operator

    datatype token =
	  Operator of operator
	| Id       of id
	| Binder   of id
	| Int      of int
	| Real     of real
	| String   of string
	| Array    of token list
	| Function of token list

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
      | operator "render"	= SOME Render
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


  (* Transformations *)

    fun transform(mat, matI, Renderer.Transform(mat', matI', obj)) =
	    Renderer.Transform(Geometry.mulMat(mat, mat'),
			       Geometry.mulMat(matI',matI), obj)
      | transform(mat, matI, obj) = Renderer.Transform(mat, matI, obj)

    fun translate(tx, ty, tz, obj) =
	transform(Geometry.translationMat(tx, ty, tz),
		  Geometry.translationMat(~tx, ~ty, ~tz), obj)
    fun scale(sx, sy, sz, obj) =
	transform(Geometry.scaleMat(sx, sy, sz),
		  Geometry.scaleMat(1.0/sx, 1.0/sy, 1.0/sz), obj)

    fun uscale(s, obj) = scale(s, s, s, obj)

    fun rotateX(angle, obj) = transform(Geometry.rotationXMat angle,
					Geometry.rotationXMat(~angle), obj)
    fun rotateY(angle, obj) = transform(Geometry.rotationYMat angle,
					Geometry.rotationYMat(~angle), obj)
    fun rotateZ(angle, obj) = transform(Geometry.rotationZMat angle,
					Geometry.rotationZMat(~angle), obj)

  (* Surface wrappers *)

    fun planeFace (Renderer.PlaneSurface) (u,v,_) = (0,u,v)

    fun sphereFace (Renderer.SphereSurface) (x,y,z) = (0,x,x) (*UNFINISHED*)

    fun cubeFace (Renderer.CubeFront)  (u,v,_) = (0,u,v)
      | cubeFace (Renderer.CubeBack)   (u,v,_) = (1,u,v)
      | cubeFace (Renderer.CubeLeft)   (_,u,v) = (2,u,v)
      | cubeFace (Renderer.CubeRight)  (_,u,v) = (3,u,v)
      | cubeFace (Renderer.CubeTop)    (u,_,v) = (4,u,v)
      | cubeFace (Renderer.CubeBottom) (u,_,v) = (5,u,v)

    fun halfp1 x = (x + 1.0)/2.0

    fun cylinderFace (Renderer.CylinderSide)   (x,y,z) = (0,x,x) (*UNFINISHED*)
      | cylinderFace (Renderer.CylinderTop)    (x,_,z) = (1, halfp1 x, halfp1 z)
      | cylinderFace (Renderer.CylinderBottom) (x,_,z) = (2, halfp1 x, halfp1 z)

    fun coneFace (Renderer.ConeSide) (x,y,z) = (0,x,x) (*UNFINISHED*)
      | coneFace (Renderer.ConeBase) (x,_,z) = (1, halfp1 x, halfp1 z)


  (* Programs *)

    type id = string

    datatype token =
	  Operator of operator
	| Id       of id
	| Binder   of id
	| Int      of int
	| Real     of real
	| String   of string
	| Array    of token list
	| Function of token list

    datatype program = Program of token list


  (* Values *)

    structure Env = BinaryMapFn(type ord_key = id  val compare = String.compare)

    datatype value =
	  BoolV    of bool
	| IntV     of int
	| RealV    of real
	| StringV  of string
	| ClosureV of env * token list
	| ArrayV   of value vector
	| PointV   of real * real * real
	| ObjectV  of Renderer.object
	| LightV   of Renderer.light

    withtype env = value Env.map


  (* Helpers *)

    open Geometry

    fun degToRad r = r * Math.pi / 180.0
    fun radToDeg r = r * 180.0 / Math.pi

    fun realToWord8 x = Word8.fromInt(Real.trunc(255.9 * x))
    fun colorToPPMColor {red, green, blue} =
	(realToWord8 red, realToWord8 green, realToWord8 blue)

    fun loop(n,f)    = loop'(0,n,f)
    and loop'(i,n,f) = if i = n then () else (f i; loop'(i+1, n, f))


  (* Evaluation *)

    exception Error of string

    fun typeError() = raise Error "stack underflow or type error"


    (* This has to be polymorphic, thus here... -- I WANT POLYREC in SML!! *)
    val evalFwd =
	ref(NONE : (env * value list * token list -> value list) option)
    fun evalPrimObj(ClosureV(env, code) :: stack, RenderObj, face) =
	let
	    fun surface(i,u,v) =
		case valOf(!evalFwd)(env, [RealV v, RealV u, IntV i], code) of
		  [RealV n, RealV ks, RealV kd, PointV rgb] =>
			{ color    = Color.color rgb
			, diffuse  = kd
			, specular = ks
			, phong    = n
			}
		| _ => typeError()

	    val f = case code of
		      (* constant function *)
		      [Binder _, Binder _, Binder _,
		       Real r, Real g, Real b, Operator Point,
		       Real kd, Real ks, Real n] =>
			  let val result =
				{ color    = Color.color(r,g,b)
				, diffuse  = kd
				, specular = ks
				, phong    = n
				}
			  in fn _ => fn _ => result end
		    | _ => fn x => surface o face x
	in
	    ObjectV(RenderObj f) :: stack
	end
      | evalPrimObj _ = typeError()


    fun run(Program code) =
	ignore(eval(Env.empty, nil, code))
	handle Option.Option     => raise Error "unbound identifier"
	     | General.Subscript => raise Error "index out of bounds"
	     | General.Div       => raise Error "division by zero"
	     | General.Overflow  => raise Error "overflow"
	     | IO.Io _           => raise Error "i/o error"

    and eval(env, stack, nil)     = stack
      | eval(env, stack, v::code) =
	case v of
	  Int i      => eval(env, IntV i :: stack, code)
	| Real r     => eval(env, RealV r :: stack, code)
	| String s   => eval(env, StringV s :: stack, code)
	| Binder x   => eval(Env.insert(env, x, hd stack), tl stack, code)
	| Id x       => eval(env, valOf(Env.find(env, x)) :: stack, code)
	| Function c => eval(env, ClosureV(env, c) :: stack, code)
	| Array c    => let val vs = List.rev(eval(env, nil, c)) in
			    eval(env, ArrayV(Vector.fromList vs) :: stack, code)
			end
	| Operator y => let val stack' = evalOp(y, stack) in
			    eval(env, stack', code)
			end

    and evalOp(Apply, ClosureV(env, code) :: stack) = eval(env, stack, code)
      | evalOp(False, stack)                        = BoolV false :: stack
      | evalOp(True,  stack)                        = BoolV true :: stack
      | evalOp(If, ClosureV(env2, code2) ::
		   ClosureV(env1, code1) :: BoolV b :: stack) =
	if b then eval(env1, stack, code1)
	     else eval(env2, stack, code2)

      | evalOp(EqI,   stack) = evalIntIntToBool (stack, op=)
      | evalOp(LessI, stack) = evalIntIntToBool (stack, Int.<)
      | evalOp(AddI,  stack) = evalIntIntToInt  (stack, Int.+)
      | evalOp(SubI,  stack) = evalIntIntToInt  (stack, Int.-)
      | evalOp(NegI,  stack) = evalIntToInt     (stack, Int.~)
      | evalOp(MulI,  stack) = evalIntIntToInt  (stack, Int.* )
      | evalOp(DivI,  stack) = evalIntIntToInt  (stack, Int.quot)
      | evalOp(ModI,  stack) = evalIntIntToInt  (stack, Int.rem)

      | evalOp(EqF,   stack) = evalRealRealToBool (stack, Real.==)
      | evalOp(LessF, stack) = evalRealRealToBool (stack, Real.<)
      | evalOp(AddF,  stack) = evalRealRealToReal (stack, Real.+)
      | evalOp(SubF,  stack) = evalRealRealToReal (stack, Real.-)
      | evalOp(NegF,  stack) = evalRealToReal     (stack, Real.~)
      | evalOp(MulF,  stack) = evalRealRealToReal (stack, Real.* )
      | evalOp(DivF,  stack) = evalRealRealToReal (stack, Real./)

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

      | evalOp(Point, RealV z :: RealV y :: RealV x :: stack) =
	    PointV(x,y,z) :: stack
      | evalOp(GetX, PointV(x,y,z) :: stack) = RealV x :: stack
      | evalOp(GetY, PointV(x,y,z) :: stack) = RealV y :: stack
      | evalOp(GetZ, PointV(x,y,z) :: stack) = RealV z :: stack

      | evalOp(Get,    IntV i :: ArrayV a :: stack) = Vector.sub(a, i) :: stack
      | evalOp(Length, ArrayV a :: stack) = IntV(Vector.length a) :: stack

      | evalOp(Plane,    stack) = evalPrimObj(stack, Renderer.Plane, planeFace)
      | evalOp(Sphere,   stack) = evalPrimObj(stack, Renderer.Sphere,sphereFace)
      | evalOp(Cube,     stack) = evalPrimObj(stack, Renderer.Cube, cubeFace)
      | evalOp(Cone,     stack) = evalPrimObj(stack, Renderer.Cone, coneFace)
      | evalOp(Cylinder, stack) = evalPrimObj(stack, Renderer.Cylinder,
						     cylinderFace)
      | evalOp(Union,      stack) = evalComposedObj(stack, Renderer.Union)
      | evalOp(Intersect,  stack) = evalComposedObj(stack, Renderer.Intersect)
      | evalOp(Difference, stack) = evalComposedObj(stack, Renderer.Difference)

      | evalOp(Translate,  stack) = evalTransformVec(stack, translate)
      | evalOp(Scale,      stack) = evalTransformVec(stack, scale)
      | evalOp(Uscale,     stack) = evalTransformReal(stack, uscale)
      | evalOp(RotateX,    stack) = evalTransformAngle(stack, rotateX)
      | evalOp(RotateY,    stack) = evalTransformAngle(stack, rotateY)
      | evalOp(RotateZ,    stack) = evalTransformAngle(stack, rotateZ)

      | evalOp(Light, PointV rgb :: PointV dir :: stack) =
	    LightV(Renderer.Directional(Color.color rgb, normalizeVec dir))
	    :: stack
      | evalOp(Pointight, PointV rgb :: PointV pos :: stack) =
	    LightV(Renderer.Point(Color.color rgb, pos)) :: stack
      | evalOp(Spotlight, RealV exp :: RealV cutoff :: PointV rgb ::
			  PointV at :: PointV pos :: stack) =
	    LightV(Renderer.Spot(Color.color rgb, pos,at, degToRad cutoff, exp))
	    :: stack

      | evalOp(Render, StringV filename :: IntV height :: IntV width ::
		       RealV vision :: IntV depth :: ObjectV scene ::
		       ArrayV lights :: PointV rgb :: stack) =
	let
	    val file   = PPMFile.openOut(filename, width, height)
	    val render = Renderer.mkRender
		{ ambient   = Color.color rgb
		, lights    = List.tabulate(Vector.length lights,
					    fn i => case Vector.sub(lights, i)
						      of LightV l => l
						       | _ => typeError())
		, scene     = scene
		, vision    = degToRad vision
		, width     = width
		, height    = height
		, depth     = depth
		}
	in
	    loop(height, fn y =>
		loop(width, fn x =>
		    PPMFile.output1(file, colorToPPMColor(render(x,y)))
		)
	    );
	    PPMFile.closeOut file;
	    stack
	end

      | evalOp _ = typeError()

  (* Artihmetic Functions *)

    and evalIntToInt(IntV i :: stack, f)		= IntV(f i) :: stack
      | evalIntToInt _					= typeError()
    and evalIntToReal(IntV i :: stack, f)		= RealV(f i) :: stack
      | evalIntToReal _					= typeError()
    and evalRealToInt(RealV r :: stack, f)		= IntV(f r) :: stack
      | evalRealToInt _					= typeError()
    and evalRealToReal(RealV r :: stack, f)		= RealV(f r) :: stack
      | evalRealToReal _				= typeError()
    and evalIntIntToInt(IntV n :: IntV m :: stack, f)	= IntV(f(m,n)) :: stack
      | evalIntIntToInt _				= typeError()
    and evalIntIntToBool(IntV n :: IntV m :: stack, f)	= BoolV(f(m,n)) :: stack
      | evalIntIntToBool _				= typeError()
    and evalRealRealToReal(RealV y :: RealV x :: stack, f)
							= RealV(f(x,y)) :: stack
      | evalRealRealToReal _				= typeError()
    and evalRealRealToBool(RealV y :: RealV x :: stack, f)
							= BoolV(f(x,y)) :: stack
      | evalRealRealToBool _				= typeError()

  (* Composed Objects *)

    and evalComposedObj(ObjectV obj2 :: ObjectV obj1 :: stack, RenderConstr) =
	    ObjectV(RenderConstr(obj1, obj2)) :: stack
      | evalComposedObj _ = typeError()

  (* Transformations *)

    and evalTransformReal(RealV r :: ObjectV obj :: stack, transform) =
	    ObjectV(transform(r, obj)) :: stack
      | evalTransformReal _ = typeError()

    and evalTransformAngle(RealV r :: ObjectV obj :: stack, transform) =
	    ObjectV(transform(degToRad r, obj)) :: stack
      | evalTransformAngle _ = typeError()

    and evalTransformVec(RealV z :: RealV y :: RealV x :: ObjectV obj :: stack,
			 transform) =
	    ObjectV(transform(x, y, z, obj)) :: stack
      | evalTransformVec _ = typeError()

  (* Tie the knot *)

    val _ = evalFwd := SOME eval
end
