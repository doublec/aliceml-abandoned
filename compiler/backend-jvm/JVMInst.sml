(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure JVMInst =
    struct
	type label = int
	type classname = string
	type fieldname = string
	type methodname = string
	type stamp = Stamp.t
	datatype arg =
	    Arraysig
	  | Boolsig
	  | Charsig
	  | Classsig of classname
	  | Floatsig
	  | Intsig
	  | Voidsig

	datatype jvmBaseType =
	    JVMInt of LargeInt.int
	  | JVMFloat of real
	  | JVMString of string
	  | JVMWord of LargeWord.word
	  | JVMChar of char;

	datatype classAccess =
	    CPublic | CFinal | CSuper | CAbstract | CInterface

	datatype fieldAccess =
	    FPublic | FPrivate | FProtected | FStatic | FFinal
	  | FVolatile | FTransient

	datatype methodAccess =
	    MPublic | MPrivate | MProtected | MStatic | MFinal
	  | MSynchronized | MNative | MAbstract

	datatype instr =
	    Astore of stamp
	  | Aastore
	  | Aaload
	  | Aconst_null
	  | Aload of stamp
	  | Anewarray of classname
	  | Areturn
	  | Arraylength
	  | Athrow
	  | Bipush of int
	  | Catch of classname * label * label * label
	  | Call of classname * methodname * instr list * instr list
	  | Checkcast of classname
	  | Comment of string
	  | Dup
	  | Fcmpl
	  | Fconst of int
	  | Get of instr list
	  | Getfield of fieldname * arg list
	    (* arg list specifies the type. May be an Array *)
	  | Getstatic of fieldname * arg list
	    (* arg list specifies the type. May be an Array *)
	  | Goto of label
	  | Iadd
	  | Iconst of int
	  | Ifacmpeq of label
	  | Ifacmpne of label
	  | Ifeq  of label
	  | Ificmpeq of label
	  | Ificmpge of label
	  | Ificmpgt of label
	  | Ificmple of label
	  | Ificmplt of label
	  | Ificmpne of label
	  | Ifne of label
	  | Ifnull of label
	  | Iload of stamp
	  | Instanceof of classname
	  | Invokeinterface of classname * methodname * (arg list * arg list)
	  | Invokespecial of classname * methodname * (arg list * arg list)
	  | Invokestatic of classname * methodname * (arg list * arg list)
	  | Invokevirtual of classname * methodname * (arg list * arg list)
	  | Ireturn
	  | Istore of stamp
	  | Isub
	  | Label of label
	  | Line of int
	  | Lcmp
	  | Ldc of jvmBaseType
	  | Lookupswitch of (LargeInt.int list * label list) * label
	  | Multi of instr list
	  | New of classname
	  | Nop
	  | Pop
	  | Putfield of fieldname * arg list
	    (* ARG list specifies the type. May be an Array *)
	  | Putstatic of fieldname * arg list
	    (* ARG list specifies the type. May be an Array *)
	  | Return
	  | Sipush of int
	  | Swap
	  | Tableswitch of LargeInt.int * (label list) * label
	  | Var of int * string * arg list * label * label

	and class =
	    Class of classAccess list * classname * classname * classname list * field list * method list
	    (* class, superclass, implemented interfaces *)

	and field =
	    (* arg list specifies the type. May be an Array *)
	    Field of fieldAccess list * fieldname * arg list

	and method =
	    Method of methodAccess list * methodname * (arg list * arg list) * instr list
    end
