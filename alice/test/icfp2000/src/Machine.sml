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

  (* Helpers *)

    open Geometry

    fun rez x = 1.0/x

    fun degToRad r = r * Math.pi / 180.0
    fun radToDeg r = r * 180.0 / Math.pi

    fun clamp(x : real) = if x < 0.0 then 0.0 else
			  if x > 1.0 then 1.0 else x

    fun transform(mat, matI, Renderer.Transform(mat', matI', obj)) =
	    Renderer.Transform(mulMat(mat, mat'), mulMat(matI',matI), obj)
      | transform(mat, matI, obj) =
	    Renderer.Transform(mat, matI, obj)

    fun realToWord8 x = Word8.fromInt(Real.trunc(255.9 * x))
    fun colorToPPMColor {red, green, blue} =
	(realToWord8 red, realToWord8 green, realToWord8 blue)

    fun loop(n,f)    = loop'(0,n,f)
    and loop'(i,n,f) = if i = n then () else (f i; loop'(i+1, n, f))


  (* Operators *)

    type color  = Color.color
    type object = Renderer.object

    datatype operator =
	  Apply | If
	| BoolOp            of bool
	| IntToIntOp        of int -> int
	| IntToRealOp       of int -> real
	| RealToIntOp       of real -> int
	| RealToRealOp      of real -> real
	| IntIntToBoolOp    of int * int -> bool
	| IntIntToIntOp     of int * int -> int
	| RealRealToBoolOp  of real * real -> bool
	| RealRealToRealOp  of real * real -> real
	| Point
	| PointToRealOp     of point -> real
	| Get | Length
	| PrimitiveObj      of (int * real * real ->
				{ color :    color
				, diffuse :  real
				, specular : real
				, phong :    real }) -> object
	| ComposedObj       of object * object -> object
	| RealTransform     of (real  -> mat) * (real  -> mat)
	| AngleTransform    of (angle -> mat) * (angle -> mat)
	| VecTransform      of (vec   -> mat) * (vec   -> mat)
	| DirectionalLight | PointLight | SpotLight
	| Render


  (* Surface wrappers *)

    open Math

    val doublepi    = 2.0*pi
    val rezdoublepi = 1.0/doublepi
    fun p1half x    = (x + 1.0)/2.0
    fun clamped surface (i,u,v) = surface(i, clamp u, clamp v)

    fun uFrom(a (* = sin(2*pi*u) *), b (* = cos(2*pi*u) *)) =
	(if a >= 0.0 then acos b else doublepi - acos b) * rezdoublepi

    fun mkPlane surface =
	Renderer.Plane
	    (fn Renderer.PlaneSurface => fn(u,_,v) => surface(0,u,v))

    fun mkSphere surface =
	Renderer.Sphere
	    (fn Renderer.SphereSurface => fn(x,y,z) =>
		let val absy = clamp(abs y) in
		    if Real.==(absy, 1.0) then
			surface(0, 0.0, 0.5*(y+1.0))
		    else
			let val c = 1.0 / sqrt(1.0 - absy*absy)
			in clamped surface (0, uFrom(c*x, c*z), 0.5*(y+1.0)) end
		end
	    )

    fun mkCube surface =
	Renderer.Cube
	    (fn Renderer.CubeFront  => (fn(u,v,_) => clamped surface (0,u,v))
	      | Renderer.CubeBack   => (fn(u,v,_) => clamped surface (1,u,v))
	      | Renderer.CubeLeft   => (fn(_,v,u) => clamped surface (2,u,v))
	      | Renderer.CubeRight  => (fn(_,v,u) => clamped surface (3,u,v))
	      | Renderer.CubeTop    => (fn(u,_,v) => clamped surface (4,u,v))
	      | Renderer.CubeBottom => (fn(u,_,v) => clamped surface (5,u,v))
	    )

    fun mkCylinder surface =
	Renderer.Cylinder
	    (fn Renderer.CylinderSide =>
		    (fn(x,y,z) => clamped surface (0, uFrom(x,z), y))
	      | Renderer.CylinderTop =>
		    (fn(x,_,z) => clamped surface (1, p1half x, p1half z))
	      | Renderer.CylinderBottom =>
		    (fn(x,_,z) => clamped surface (2, p1half x, p1half z))
	    )

    fun mkCone surface =
	Renderer.Cone
	    (fn Renderer.ConeSide =>
		    (fn(x,y,z) => if Real.==(y, 0.0)
				  then surface (0, 0.0, 0.0)
				  else clamped surface (0, uFrom(x/y, z/y), y))
	      | Renderer.ConeBase =>
		    (fn(x,_,z) => clamped surface (1, p1half x, p1half z))
	    )


  (* Operator table *)

    val translationMats	= (translationMat, translationMat o negVec)
    val scaleMats	= (scaleMat,       scaleMat o rezVec)
    val uscaleMats	= (uscaleMat,      uscaleMat o rez)
    val rotationXMats	= (rotationXMat,   rotationXMat o op~)
    val rotationYMats	= (rotationYMat,   rotationYMat o op~)
    val rotationZMats	= (rotationZMat,   rotationZMat o op~)

    fun operator "acos"		= SOME(RealToRealOp(radToDeg o Math.acos))
      | operator "addf"		= SOME(RealRealToRealOp Real.+)
      | operator "addi"		= SOME(IntIntToIntOp Int.+)
      | operator "apply"	= SOME(Apply)
      | operator "asin"		= SOME(RealToRealOp(radToDeg o Math.asin))
      | operator "clampf"	= SOME(RealToRealOp clamp)
      | operator "cone"		= SOME(PrimitiveObj mkCone)
      | operator "cos"		= SOME(RealToRealOp(Math.cos o degToRad))
      | operator "cube"		= SOME(PrimitiveObj mkCube)
      | operator "cylinder"	= SOME(PrimitiveObj mkCylinder)
      | operator "difference"	= SOME(ComposedObj Renderer.Difference)
      | operator "divf"		= SOME(RealRealToRealOp Real./)
      | operator "divi"		= SOME(IntIntToIntOp Int.quot)
      | operator "eqf"		= SOME(RealRealToBoolOp Real.==)
      | operator "eqi"		= SOME(IntIntToBoolOp op=)
      | operator "false"	= SOME(BoolOp false)
      | operator "floor"	= SOME(RealToIntOp Real.floor)
      | operator "frac"		= SOME(RealToRealOp Real.realMod)
      | operator "get"		= SOME Get
      | operator "getx"		= SOME(PointToRealOp #1)
      | operator "gety"		= SOME(PointToRealOp #2)
      | operator "getz"		= SOME(PointToRealOp #3)
      | operator "if"		= SOME If
      | operator "intersect"	= SOME(ComposedObj Renderer.Intersect)
      | operator "length"	= SOME Length
      | operator "lessf"	= SOME(RealRealToBoolOp Real.<)
      | operator "lessi"	= SOME(IntIntToBoolOp Int.<)
      | operator "light"	= SOME DirectionalLight
      | operator "modi"		= SOME(IntIntToIntOp Int.rem)
      | operator "mulf"		= SOME(RealRealToRealOp Real.* )
      | operator "muli"		= SOME(IntIntToIntOp Int.* )
      | operator "negf"		= SOME(RealToRealOp Real.~)
      | operator "negi"		= SOME(IntToIntOp Int.~)
      | operator "plane"	= SOME(PrimitiveObj mkPlane)
      | operator "point"	= SOME Point
      | operator "pointlight"	= SOME PointLight
      | operator "real"		= SOME(IntToRealOp Real.fromInt)
      | operator "render"	= SOME Render
      | operator "rotatex"	= SOME(AngleTransform rotationXMats)
      | operator "rotatey"	= SOME(AngleTransform rotationYMats)
      | operator "rotatez"	= SOME(AngleTransform rotationZMats)
      | operator "scale"	= SOME(VecTransform scaleMats)
      | operator "sin"		= SOME(RealToRealOp(Math.sin o degToRad))
      | operator "sphere"	= SOME(PrimitiveObj mkSphere)
      | operator "spotlight"	= SOME SpotLight
      | operator "sqrt"		= SOME(RealToRealOp Math.sqrt)
      | operator "subf"		= SOME(RealRealToRealOp Real.-)
      | operator "subi"		= SOME(IntIntToIntOp Int.-)
      | operator "translate"	= SOME(VecTransform translationMats)
      | operator "true"		= SOME(BoolOp true)
      | operator "union"	= SOME(ComposedObj Renderer.Union)
      | operator "uscale"	= SOME(RealTransform uscaleMats)
      | operator  _		= NONE


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


  (* Evaluation *)

    exception Error of string

    fun typeError() = raise Error "stack underflow or type error"

    fun run(Program code) =
	ignore(eval(Env.empty, nil, code))
	handle General.Subscript => raise Error "index out of bounds"
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
      | evalOp(If, ClosureV(env2, code2) ::
		   ClosureV(env1, code1) :: BoolV b :: stack) =
	    if b then eval(env1, stack, code1)
		 else eval(env2, stack, code2)

      | evalOp(BoolOp b, stack)                  = BoolV b :: stack
      | evalOp(IntToIntOp f, IntV i :: stack)    = IntV(f i) :: stack
      | evalOp(IntToRealOp f, IntV i :: stack)   = RealV(f i) :: stack
      | evalOp(RealToIntOp f, RealV r :: stack)  = IntV(f r) :: stack
      | evalOp(RealToRealOp f, RealV r :: stack) = RealV(f r) :: stack
      | evalOp(IntIntToIntOp f, IntV n :: IntV m :: stack) =
	    IntV(f(m,n)) :: stack
      | evalOp(IntIntToBoolOp f, IntV n :: IntV m :: stack) =
	    BoolV(f(m,n)) :: stack
      | evalOp(RealRealToRealOp f, RealV y :: RealV x :: stack) =
	    RealV(f(x,y)) :: stack
      | evalOp(RealRealToBoolOp f, RealV y :: RealV x :: stack) =
	    BoolV(f(x,y)) :: stack

      | evalOp(Point, RealV z :: RealV y :: RealV x :: stack) =
	    PointV(x,y,z) :: stack
      | evalOp(PointToRealOp f, PointV p :: stack) = RealV(f p) :: stack
      | evalOp(Get, IntV i :: ArrayV a :: stack) = Vector.sub(a, i) :: stack
      | evalOp(Length, ArrayV a :: stack) = IntV(Vector.length a) :: stack

      | evalOp(ComposedObj f, ObjectV obj2 :: ObjectV obj1 :: stack) =
	    ObjectV(f(obj1, obj2)) :: stack
      | evalOp(PrimitiveObj f, ClosureV(env, code) :: stack) =
	let
	    val surface =
		case code
		of (* constant function *)
		   [Binder _, Binder _, Binder _,
		    Real r, Real g, Real b, Operator Point,
		    Real kd, Real ks, Real n] =>
		    let val result =
			{ color    = Color.color(r,g,b)
			, diffuse  = kd
			, specular = ks
			, phong    = n
			}
		    in fn _ => result end
		| _ =>
		    fn(i,u,v) =>
			case eval(env, [RealV v, RealV u, IntV i], code)
			of [RealV n, RealV ks, RealV kd, PointV rgb] =>
				{ color    = Color.color rgb
				, diffuse  = kd
				, specular = ks
				, phong    = n
				}
			| _ => typeError()
	in
	    ObjectV(f surface) :: stack
	end

      | evalOp(RealTransform(f1,f2), RealV r :: ObjectV obj :: stack) =
	    ObjectV(transform(f1 r, f2 r, obj)) :: stack
      | evalOp(AngleTransform(f1,f2), RealV r :: ObjectV obj :: stack) =
	    ObjectV(transform(f1(degToRad r), f2(degToRad r), obj)) :: stack
      | evalOp(VecTransform(f1,f2),
	       RealV z :: RealV y :: RealV x :: ObjectV obj :: stack) =
	    ObjectV(transform(f1(x,y,z), f2(x,y,z), obj)) :: stack

      | evalOp(DirectionalLight, PointV rgb :: PointV dir :: stack) =
	    LightV(Renderer.Directional(Color.color rgb, normalizeVec dir))
		:: stack
      | evalOp(PointLight, PointV rgb :: PointV pos :: stack) =
	    LightV(Renderer.Point(Color.color rgb, pos)) :: stack
      | evalOp(SpotLight, RealV exp :: RealV cutoff :: PointV rgb ::
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

end
