(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure IL :> IL =
    struct
	type id = string

	type dottedname = id list

	(* Class Attributes *)

	type isPublic = bool

	type classVisibility = isPublic

	datatype classInheritance =
	    AbstractClass
	  | InterfaceClass
	  | SealedClass
	  | ValueClass
	  | RegularClass

	type classAttr = classVisibility * classInheritance

	(* Method Attributes *)

	datatype memberVisibility =
	    Private
	  | Assembly
	  | Public
	  | FamilyAndAssembly
	  | Family
	  | FamilyOrAssembly
	datatype methKind =
	    Static
	  | Instance
	  | Abstract
	  | Virtual
	  | Final

	type methAttr = memberVisibility * methKind

	(* Field Attributes *)

	type static = bool
	type initonly = bool

	type fieldAttr = memberVisibility * static * initonly

	(* Types *)

	datatype ty =
	    ClassTy of dottedname
	  | ValueClassTy of dottedname
	  | ArrayTy of ty
	  | CharTy
	  | VoidTy
	  | BoolTy
	  | Int32Ty
	  | Float64Ty

	(* Instructions *)

	type label = int

	type isInstance = bool

	datatype cond =
	    EQ
	  | GE
	  | GE_UN
	  | GT
	  | GT_UN
	  | LE
	  | LE_UN
	  | LT
	  | LT_UN
	  | NE_UN
	  | TRUE
	  | FALSE

	datatype instr =
	    Add
	  | AddOvf
	  | And
	  | B of cond * label
	  | Br of label
	  | Call of isInstance * dottedname * id * ty list * ty
	  | Callvirt of dottedname * id * ty list * ty
	  | Castclass of dottedname
	  | Ceq
	  | Cgt
	  | CgtUn
	  | Clt
	  | CltUn
	  | Comment of string
	  | Div
	  | DivUn
	  | Dup
	  | Isinst of dottedname
	  | Label of label
	  | Ldarg of int
	  | LdcI4 of int
	  | LdcR8 of string
	  | LdelemRef
	  | Ldfld of dottedname * id * ty
	  | Ldlen
	  | Ldloc of int
	  | Ldnull
	  | Ldsfld of dottedname * id * ty
	  | Ldstr of string
	  | Leave of label
	  | Newarr of ty
	  | Newobj of dottedname * ty list
	  | Mul
	  | Neg
	  | Not
	  | Or
	  | Pop
	  | Rem
	  | RemUn
	  | Ret
	  | Rethrow
	  | Shl
	  | Shr
	  | ShrUn
	  | Starg of int
	  | StelemRef
	  | Stfld of dottedname * id * ty
	  | Stloc of int
	  | Stsfld of dottedname * id * ty
	  | Sub
	  | SubOvf
	  | Switch of label list
	  | Tail
	  | Throw
	  | Try of label * label * dottedname * label * label
	  | Xor

	(* Top-Level Declarations *)

	type extends = dottedname
	type implements = dottedname list

	type locals = ty list * bool   (* initialize *)

	datatype classDecl =
	    Field of id * fieldAttr * ty
	  | Method of id * methAttr * ty list * ty * locals * instr list

	type isEntrypoint = bool

	datatype decl =
	    Class of
	    dottedname * classAttr * extends * implements * classDecl list
	  | GlobalMethod of
	    id * isPublic * ty list * ty * isEntrypoint * locals * instr list

	type program = decl list

	(* Output to File *)

	val output = TextIO.output
	val output1 = TextIO.output1

	val outputId = output

	(* Compute Stack Size *)

	structure Map =
	    MakeHashImpMap(type t = label
			   val equals = op=
			   fun hash label = label)

	val size = ref 0
	val maxSize = ref 0
	val map: int Map.t ref = ref (Map.new ())
	val returnSize = ref 0

	fun pop n =
	    if !size = ~1 then ()
	    else
		let
		    val i = !size - n
		in
		    if i < 0 then
			raise Crash.Crash ("stack underflow by " ^
					   Int.toString (~i))
		    else size := i
		end

	fun push n =
	    if !size = ~1 then ()
	    else
		let
		    val i = !size + n
		in
		    size := i;
		    if i > !maxSize then maxSize := i else ()
		end

	fun branch label =
	    if !size = ~1 then
		case Map.lookup (!map, label) of
		    SOME n => size := n
		  | NONE => ()
	    else
		case Map.lookup (!map, label) of
		    SOME n =>
			if !size = n then ()
			else
			    raise Crash.Crash ("inconsistent stack size " ^
					       "for label " ^
					       Int.toString label ^ ": " ^
					       Int.toString (!size) ^ " <> " ^
					       Int.toString n)
		  | NONE => Map.insertDisjoint (!map, label, !size)

	fun catch label = Map.insertDisjoint (!map, label, 1)

	fun invalidate () = size := ~1

	fun return () =
	    if !size = ~1 orelse !size = !returnSize then ()
	    else
		raise Crash.Crash ("non-empty stack on return: " ^
				   Int.toString (!size) ^ " <> " ^
				   Int.toString (!returnSize))

	fun eval (Add | AddOvf) = (pop 2; push 1)
	  | eval And = (pop 2; push 1)
	  | eval (B ((TRUE | FALSE), label)) = (pop 1; branch label)
	  | eval (B (_, label)) = (pop 2; branch label)
	  | eval (Br label) = (branch label; invalidate ())
	  | eval (Call (isInstance, _, _, tys, ty)) =
	    (pop ((if isInstance then 1 else 0) + List.length tys);
	     case ty of VoidTy => () | _ => push 1)
	  | eval (Callvirt (_, _, tys, ty)) =
	    (pop (List.length tys + 1);
	     case ty of VoidTy => () | _ => push 1)
	  | eval (Castclass _) = (pop 1; push 1)
	  | eval (Ceq | Cgt | CgtUn | Clt | CltUn) = (pop 2; push 1)
	  | eval (Comment _) = ()
	  | eval (Div | DivUn) = (pop 2; push 1)
	  | eval Dup = (pop 1; push 2)
	  | eval (Isinst _) = (pop 1; push 1)
	  | eval (Label label) = branch label
	  | eval (Ldarg _) = push 1
	  | eval (LdcI4 _) = push 1
	  | eval (LdcR8 _) = push 1
	  | eval LdelemRef = (pop 2; push 1)
	  | eval (Ldfld (_, _, _)) = (pop 1; push 1)
	  | eval Ldlen = (pop 1; push 1)
	  | eval (Ldloc _) = push 1
	  | eval Ldnull = push 1
	  | eval (Ldsfld (_, _, _)) = push 1
	  | eval (Ldstr _) = push 1
	  | eval (Leave label) = (branch label; invalidate ())
	  | eval (Newarr _) = (pop 1; push 1)
	  | eval (Newobj (_, tys)) = (pop (List.length tys); push 1)
	  | eval Mul = (pop 2; push 1)
	  | eval Neg = (pop 2; push 1)
	  | eval Not = (pop 2; push 1)
	  | eval Or = (pop 2; push 1)
	  | eval Pop = pop 1
	  | eval (Rem | RemUn) = (pop 2; push 1)
	  | eval Ret = (return (); invalidate ())
	  | eval Rethrow = invalidate ()
	  | eval (Shl | Shr | ShrUn) = (pop 2; push 1)
	  | eval (Starg _) = pop 2
	  | eval StelemRef = pop 3
	  | eval (Stfld (_, _, _)) = pop 2
	  | eval (Stloc _) = pop 1
	  | eval (Stsfld (_, _, _)) = pop 1
	  | eval (Sub | SubOvf) = (pop 2; push 1)
	  | eval (Switch labels) = (pop 1; List.app branch labels)
	  | eval Tail = ()
	  | eval Throw = (pop 1; invalidate ())
	  | eval (Try (tryLabel, _, _, catchLabel, _)) =
	    (branch tryLabel; catch catchLabel)
	  | eval Xor = (pop 2; push 1)

	fun outputMaxStack (q, instrs, ty) =
	    (size := 0; maxSize := 0; map := Map.new ();
	     returnSize := (case ty of VoidTy => 0 | _ => 1);
	     List.app eval instrs;
	     output (q, ".maxstack "); output (q, Int.toString (!maxSize));
	     output (q, "\n"))
	    handle Crash.Crash s => output (q, "//--** " ^ s ^ "\n.maxstack 1024\n")

	(* Output IL Syntax *)

	fun outputDottedname (q, [id]) = outputId (q, id)
	  | outputDottedname (q, id::idr) =
	    (outputId (q, id); output1 (q, #"."); outputDottedname (q, idr))
	  | outputDottedname (_, nil) = raise Crash.Crash "IL.outputDottedname"

	fun outputClassAttr (q, (isPublic, inheritance)) =
	    (if isPublic then output (q, "public ")
	     else output (q, "private ");
	     case inheritance of
		 AbstractClass => output (q, "abstract ")
	       | InterfaceClass => output (q, "interface ")
	       | SealedClass => output (q, "sealed ")
	       | ValueClass => output (q, "value ")
	       | RegularClass => ())

	fun outputMemberVisibility (q, Private) = output (q, "private ")
	  | outputMemberVisibility (q, Assembly) = output (q, "assembly ")
	  | outputMemberVisibility (q, Public) = output (q, "public ")
	  | outputMemberVisibility (q, FamilyAndAssembly) =
	    output (q, "famandassem ")
	  | outputMemberVisibility (q, Family) = output (q, "family ")
	  | outputMemberVisibility (q, FamilyOrAssembly) =
	    output (q, "famorassem ")

	fun outputMethKind (q, Static) = output (q, "static ")
	  | outputMethKind (q, Instance) = ()
	  | outputMethKind (q, Abstract) = output (q, "abstract virtual ")
	  | outputMethKind (q, Virtual) = output (q, "virtual ")
	  | outputMethKind (q, Final) = output (q, "final virtual ")

	fun outputMethAttr (q, (vis, kind)) =
	    (outputMemberVisibility (q, vis);
	     outputMethKind (q, kind))

	fun outputFieldAttr (q, (vis, static, initonly)) =
	    (outputMemberVisibility (q, vis);
	     if static then output (q, "static ") else ();
	     if initonly then output (q, "initonly ") else ())

	fun outputTy (q, ClassTy dottedname) =
	    (output (q, "class "); outputDottedname (q, dottedname))
	  | outputTy (q, ValueClassTy dottedname) =
	    (output (q, "value class "); outputDottedname (q, dottedname))
	  | outputTy (q, ArrayTy ty) = (outputTy (q, ty); output (q, "[]"))
	  | outputTy (q, CharTy) = output (q, "wchar")
	  | outputTy (q, VoidTy) = output (q, "void")
	  | outputTy (q, BoolTy) = output (q, "bool")
	  | outputTy (q, Int32Ty) = output (q, "int32")
	  | outputTy (q, Float64Ty) = output (q, "float64")

	fun outputTys (q, [ty]) = outputTy (q, ty)
	  | outputTys (q, ty::tyr) =
	    (outputTy (q, ty); output (q, ", "); outputTys (q, tyr))
	  | outputTys (q, nil) = ()

	fun outputLabel (q, i) = output (q, "L" ^ Int.toString i)

	fun outputLabels (q, [label]) = outputLabel (q, label)
	  | outputLabels (q, label::labelr) =
	    (outputLabel (q, label); output (q, ", ");
	     outputLabels (q, labelr))
	  | outputLabels (q, nil) = ()

	fun intToString i =
	    if i < 0 then "-" ^ Int.toString (~i)
	    else Int.toString i

	local
	    fun toOct i = String.str (Char.chr (i mod 8 + Char.ord #"0"))

	    fun charToCString #"\\" = "\\\\"
	      | charToCString #"\"" = "\\\""
	      | charToCString #"\a" = "\\a"
	      | charToCString #"\b" = "\\b"
	      | charToCString #"\t" = "\\t"
	      | charToCString #"\n" = "\\n"
	      | charToCString #"\v" = "\\v"
	      | charToCString #"\f" = "\\f"
	      | charToCString #"\r" = "\\r"
	      | charToCString c =
		let
		    val i = Char.ord c
		in
		    if i < 32 then "\\0" ^ toOct (i div 8) ^ toOct i
		    else String.str c
		end
	in
	    fun stringToCString s =
		List.foldr (fn (c, rest) => charToCString c ^ rest) ""
		(explode s)
	end

	fun outputInstr (q, Add) = output (q, "add")
	  | outputInstr (q, AddOvf) = output (q, "add.ovf")
	  | outputInstr (q, And) = output (q, "and")
	  | outputInstr (q, B (EQ, label)) =   (*--** short form? *)
	    (output (q, "beq "); outputLabel (q, label))
	  | outputInstr (q, B (GE, label)) =   (*--** short form? *)
	    (output (q, "bge "); outputLabel (q, label))
	  | outputInstr (q, B (GE_UN, label)) =   (*--** short form? *)
	    (output (q, "bge.un "); outputLabel (q, label))
	  | outputInstr (q, B (GT, label)) =   (*--** short form? *)
	    (output (q, "bgt "); outputLabel (q, label))
	  | outputInstr (q, B (GT_UN, label)) =   (*--** short form? *)
	    (output (q, "bgt.un "); outputLabel (q, label))
	  | outputInstr (q, B (LE, label)) =   (*--** short form? *)
	    (output (q, "ble "); outputLabel (q, label))
	  | outputInstr (q, B (LE_UN, label)) =   (*--** short form? *)
	    (output (q, "ble.un "); outputLabel (q, label))
	  | outputInstr (q, B (LT, label)) =   (*--** short form? *)
	    (output (q, "blt "); outputLabel (q, label))
	  | outputInstr (q, B (LT_UN, label)) =   (*--** short form? *)
	    (output (q, "blt.un "); outputLabel (q, label))
	  | outputInstr (q, B (NE_UN, label)) =   (*--** short form? *)
	    (output (q, "bne.un "); outputLabel (q, label))
	  | outputInstr (q, B (TRUE, label)) =   (*--** short form? *)
	    (output (q, "brtrue "); outputLabel (q, label))
	  | outputInstr (q, B (FALSE, label)) =   (*--** short form? *)
	    (output (q, "brfalse "); outputLabel (q, label))
	  | outputInstr (q, Br label) =   (*--** short form? *)
	    (output (q, "br "); outputLabel (q, label))
	  | outputInstr (q, Call (isInstance, dottedname, id, tys, ty)) =
	    (output (q, "call ");
	     if isInstance then output (q, "instance ") else ();
	     outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id); output1 (q, #"(");
	     outputTys (q, tys); output1 (q, #")"))
	  | outputInstr (q, Callvirt (dottedname, id, tys, ty)) =
	    (output (q, "callvirt instance ");
	     outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id); output1 (q, #"(");
	     outputTys (q, tys); output1 (q, #")"))
	  | outputInstr (q, Castclass dottedname) =
	    (output (q, "castclass "); outputDottedname (q, dottedname))
	  | outputInstr (q, Ceq) = output (q, "ceq")
	  | outputInstr (q, Cgt) = output (q, "cgt")
	  | outputInstr (q, CgtUn) = output (q, "cgt.un")
	  | outputInstr (q, Clt) = output (q, "clt")
	  | outputInstr (q, CltUn) = output (q, "clt.un")
	  | outputInstr (q, Comment s) = output (q, "// " ^ s)
	  | outputInstr (q, Div) = output (q, "div")
	  | outputInstr (q, DivUn) = output (q, "div.un")
	  | outputInstr (q, Dup) = output (q, "dup")
	  | outputInstr (q, Isinst dottedname) =
	    (output (q, "isinst "); outputDottedname (q, dottedname))
	  | outputInstr (q, Label label) =
	    (outputLabel (q, label); output1 (q, #":"))
	  | outputInstr (q, Ldarg i) =
	    (output (q, "ldarg");
	     if i < 4 then (output1 (q, #"."); output (q, Int.toString i))
	     else if i < 256 then
		 (output (q, ".s "); output (q, Int.toString i))
	     else (output1 (q, #" "); output (q, Int.toString i)))
	  | outputInstr (q, LdcI4 i) =
	    (output (q, "ldc.i4");
	     if i >= 0 andalso i <= 8 then
		 (output1 (q, #"."); output (q, Int.toString i))
	     else if i = ~1 then output (q, ".M1")
	     else if i >= ~128 andalso i <= 127 then
		 (output (q, ".s "); output (q, intToString i))
	     else (output1 (q, #" "); output (q, intToString i)))
	  | outputInstr (q, LdcR8 r) =
	    (output (q, "ldc.r8 "); output (q, r))
	  | outputInstr (q, LdelemRef) = output (q, "ldelem.ref")
	  | outputInstr (q, Ldfld (dottedname, id, ty)) =
	    (output (q, "ldfld "); outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id))
	  | outputInstr (q, Ldlen) = output (q, "ldlen")
	  | outputInstr (q, Ldloc i) =
	    (output (q, "ldloc");
	     if i < 4 then (output1 (q, #"."); output (q, Int.toString i))
	     else if i < 256 then
		 (output (q, ".s "); output (q, Int.toString i))
	     else (output1 (q, #" "); output (q, Int.toString i)))
	  | outputInstr (q, Ldnull) = output (q, "ldnull")
	  | outputInstr (q, Ldsfld (dottedname, id, ty)) =
	    (output (q, "ldsfld "); outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id))
	  | outputInstr (q, Ldstr s) =
	    (output (q, "ldstr \""); output (q, stringToCString s);
	     output1 (q, #"\""))
	  | outputInstr (q, Leave label) =   (*--** short form? *)
	    (output (q, "leave "); outputLabel (q, label))
	  | outputInstr (q, Newarr ty) =
	    (output (q, "newarr "); outputTy (q, ty))
	  | outputInstr (q, Newobj (dottedname, tys)) =
	    (output (q, "newobj instance void ");
	     outputDottedname (q, dottedname);
	     output (q, "::.ctor("); outputTys (q, tys); output1 (q, #")"))
	  | outputInstr (q, Mul) = output (q, "mul")
	  | outputInstr (q, Neg) = output (q, "neg")
	  | outputInstr (q, Not) = output (q, "not")
	  | outputInstr (q, Or) = output (q, "or")
	  | outputInstr (q, Pop) = output (q, "pop")
	  | outputInstr (q, Rem) = output (q, "rem")
	  | outputInstr (q, RemUn) = output (q, "rem.un")
	  | outputInstr (q, Ret) = output (q, "ret")
	  | outputInstr (q, Rethrow) = output (q, "rethrow")
	  | outputInstr (q, Shl) = output (q, "shl")
	  | outputInstr (q, Shr) = output (q, "shr")
	  | outputInstr (q, ShrUn) = output (q, "shr.un")
	  | outputInstr (q, Starg i) =
	    (output (q, "starg");
	     if i < 4 then (output1 (q, #"."); output (q, Int.toString i))
	     else if i < 256 then
		 (output (q, ".s "); output (q, Int.toString i))
	     else (output1 (q, #" "); output (q, Int.toString i)))
	  | outputInstr (q, StelemRef) = output (q, "stelem.ref")
	  | outputInstr (q, Stfld (dottedname, id, ty)) =
	    (output (q, "stfld "); outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id))
	  | outputInstr (q, Stloc i) =
	    (output (q, "stloc");
	     if i < 4 then (output1 (q, #"."); output (q, Int.toString i))
	     else if i < 256 then
		 (output (q, ".s "); output (q, Int.toString i))
	     else (output1 (q, #" "); output (q, Int.toString i)))
	  | outputInstr (q, Stsfld (dottedname, id, ty)) =
	    (output (q, "stsfld "); outputTy (q, ty); output1 (q, #" ");
	     outputDottedname (q, dottedname); output (q, "::");
	     outputId (q, id))
	  | outputInstr (q, Sub) = output (q, "sub")
	  | outputInstr (q, SubOvf) = output (q, "sub.ovf")
	  | outputInstr (q, Switch labels) =
	    (output (q, "switch("); outputLabels (q, labels);
	     output1 (q, #")"))
	  | outputInstr (q, Tail) = output (q, "tail.")
	  | outputInstr (q, Throw) = output (q, "throw")
	  | outputInstr (q, Try (label1, label2, dottedname, label3, label4)) =
	    (output (q, ".try "); outputLabel (q, label1);
	     output (q, " to "); outputLabel (q, label2);
	     output (q, " catch "); outputDottedname (q, dottedname);
	     output (q, " handler "); outputLabel (q, label3);
	     output (q, " to "); outputLabel (q, label4))
	  | outputInstr (q, Xor) = output (q, "xor")

	fun outputInstrs (q, (instr as Try (_, _, _, _, _))::instrr, trys) =
	    outputInstrs (q, instrr, trys @ [instr])
	  | outputInstrs (q, instr::instrr, trys) =
	    (output (q, "  "); outputInstr (q, instr); output1 (q, #"\n");
	     outputInstrs (q, instrr, trys))
	  | outputInstrs (q, nil, instr::instrr) =
	    (output (q, "  "); outputInstr (q, instr); output1 (q, #"\n");
	     outputInstrs (q, nil, instrr))
	  | outputInstrs (_, nil, nil) = ()

	local
	    fun outputLocals' (q, ty::tyr) =
		(output (q, ", "); outputTy (q, ty); outputLocals' (q, tyr))
	      | outputLocals' (q, nil) = ()
	in
	    fun outputLocals (q, (ty1::tyr, zeroinit)) =
		(output (q, ".locals("); outputTy (q, ty1);
		 outputLocals' (q, tyr); output (q, ")\n");
		 if zeroinit then output (q, ".zeroinit\n") else ())
	      | outputLocals (_, (nil, _)) = ()
	end

	fun outputClassDecl (q, Field (id, attr, ty)) =
	    (output (q, ".field "); outputFieldAttr (q, attr);
	     outputTy (q, ty); output1 (q, #" "); outputId (q, id);
	     output1 (q, #"\n"))
	  | outputClassDecl (q, Method (id, attr, tys, ty, locals, instrs)) =
	    (output (q, ".method "); outputMethAttr (q, attr);
	     outputTy (q, ty); output1 (q, #" "); outputId (q, id);
	     output1 (q, #"("); outputTys (q, tys); output (q, ") {\n");
	     outputMaxStack (q, instrs, ty);
	     outputLocals (q, locals); outputInstrs (q, instrs, nil);
	     output (q, "}\n"))

	fun outputClassDecls (q, decl::declr) =
	    (outputClassDecl (q, decl); outputClassDecls (q, declr))
	  | outputClassDecls (_, nil) = ()

	fun splitNamespace [id] = (nil, id)
	  | splitNamespace (id::idr) =
	    let
		val (namespace, id') = splitNamespace idr
	    in
		(id::namespace, id')
	    end
	  | splitNamespace nil = raise Crash.Crash "IL.splitNamespace"

	local
	    fun outputDottednames (q, dottedname::dottednames) =
		(output (q, ", "); outputDottedname (q, dottedname);
		 outputDottednames (q, dottednames))
	      | outputDottednames (_, nil) = ()
	in
	    fun outputImplements (q, dottedname::dottednames) =
		(output (q, "implements "); outputDottedname (q, dottedname);
		 outputDottednames (q, dottednames))
	      | outputImplements (_, nil) = ()
	end

	fun outputDecl (q, Class (name, attr, super, interfaces, members)) =
	    let
		val (namespace, id) = splitNamespace name
	    in
		case namespace of
		    nil => ()
		  | _::_ =>
			(output (q, ".namespace ");
			 outputDottedname (q, namespace);
			 output (q, " {\n"));
		output (q, ".class "); outputClassAttr (q, attr);
		outputId (q, id); output (q, " extends ");
		outputDottedname (q, super);
		outputImplements (q, interfaces);
		output (q, " {\n"); outputClassDecls (q, members);
		output (q, "}\n");
		case namespace of
		    nil => ()
		  | _::_ => output (q, "}\n")
	    end
	  | outputDecl (q, GlobalMethod (id, isPublic, tys, ty, isEntrypoint,
					 locals, instrs)) =
	    (output (q, ".method ");
	     output (q, if isPublic then "public " else "private ");
	     outputTy (q, ty); output1 (q, #" "); outputId (q, id);
	     output1 (q, #"("); outputTys (q, tys); output (q, ") {\n");
	     if isEntrypoint then output (q, ".entrypoint\n") else ();
	     outputMaxStack (q, instrs, ty);
	     outputLocals (q, locals); outputInstrs (q, instrs, nil);
	     output (q, "}\n"))

	fun outputProgram (q, [decl]) = outputDecl (q, decl)
	  | outputProgram (q, decl::declr) =
	    (outputDecl (q, decl); output1 (q, #"\n");
	     outputProgram (q, declr))
	  | outputProgram (_, nil) = ()
    end
