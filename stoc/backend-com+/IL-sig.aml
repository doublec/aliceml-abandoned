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

signature IL =
    sig
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
	  | Box of dottedname
	  | Br of label
	  | Call of isInstance * dottedname * id * ty list * ty
	  | Callvirt of dottedname * id * ty list * ty
	  | Castclass of ty
	  | Ceq
	  | Cgt
	  | CgtUn
	  | Clt
	  | CltUn
	  | Comment of string
	  | Div
	  | DivUn
	  | Dup
	  | Isinst of ty
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
	  | Unbox of dottedname
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
	type t = program

	val outputProgram: TextIO.outstream * program -> unit
    end
