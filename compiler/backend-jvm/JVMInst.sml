structure JVMInst =
    struct
	type label = string
	and classname = string
	and fieldname = string
	and methodname = string;
	datatype ARG =
	    Arraysig
	  | Boolsig
	  | Classsig of classname
	  | Floatsig
	  | Intsig
	  | Voidsig

	datatype LABEL =
	    StringLabel of string
	  | IntLabel of int
	datatype JVMBASETYPE = (* fuer ldc, double und long kann dml nicht *)
	    JVMInt of int
	  | JVMFloat of real
	  | JVMString of string
	datatype
	    INSTRUCTION =
	    Astore of int
	  | Aastore
	  | Aaload
	  | Aconst_null
	  | Aload of int
	  | Anewarray of classname
	  | Areturn
	  | Arraylength
	  | Athrow
	  | Bipush of int
	  | Catch of classname * label * label * label
	  | Checkcast of classname
	  | Comment of string
	  | Dup
	  | Fconst of int
	  | Getfield of fieldname * classname
	  | Getstatic of fieldname * classname
	  | Goto of label
	  | Iadd
	  | Iconst of int
	  | Ifacmpeq of label
	  | Ifacmpne of label
	  | Ifeq  of label
	  | Ificmplt of label
	  | Ifneq of label
	  | Ifnull of label
	  | Iinc of int * int
	  | Iload of int
	  | Instanceof of classname
	  | Invokeinterface of classname * methodname * (ARG list * ARG)
	  | Invokespecial of classname * methodname * (ARG list * ARG)
	  | Invokestatic of classname * methodname * (ARG list * ARG)
	  | Invokevirtual of classname * methodname * (ARG list * ARG)
	  | Ireturn
	  | Istore of int
	  | Label of label
	  | Ldc of JVMBASETYPE
	  | New of classname
	  | Pop
	  | Putfield of classname * fieldname
	  | Putstatic of classname * fieldname
	  | Return
	  | Sipush of int
	  | Swap
	  | Tableswitch of int * (label list) * label
	and CLASS =
	    Class of CLASSACCESS list * classname * classname * FIELD list * METHOD list
	(* klasse, oberklasse *)
	and
	    FIELD =
	    Field of FIELDACCESS list * fieldname * FIELDTYPE
	and
	    METHOD =
	    Method of METHODACCESS list * methodname * (ARG list * ARG) * LIMITS * INSTRUCTION list
	and
	    FIELDTYPE =
	    Classtype of classname
	  | Sonstwas of int
	and
	    CLASSACCESS =
	    CPublic | CFinal | CSuper | CAbstract | CInterface
	and
	    FIELDACCESS =
	    FPublic | FPrivate | FProtected | FStatic | FFinal | FVolatile | FTransient
	and
	    METHODACCESS =
	    MPublic | MPrivate | MProtected | MStatic | MFinal | MSynchronized | MNative | MAbstract
	and
	    LIMITS =
	    Limits of int * int (* locals, stack *)
    end
