structure JVMInst =
    struct
	type label = string
	and classname = string
	and fieldname = string
	and methodsig = string
	and methodname = string;
 
	datatype INSTRUCTION =
	    AStore of int
	  | Aastore
	  | Aconst of int
	  | Aload of int
	  | Anewarray of classname
	  | AtCodeFloat of real
	  | AtCodeInt of int   (* iconst_?, bipush, sipush, ldc :*)
	  | AtCodeString of string
	  | Athrow
	  | Checkcast of classname
	  | Dup
	  | Getfield of classname * fieldname
	  | Getstatic of classname * fieldname
	  | Goto of label
	  | Ifacmp of label
	  | Ifeq  of label
	  | Ifneq of label
	  | Ifnull of label
	  | Instanceof of classname
	  | Invokespecial of classname * methodname * methodsig
	  | Invokevirtual of classname * methodname * methodsig
	  | Ldc of label
	  | New of classname
	  | Pop
	  | Putfield of classname * fieldname
	  | Putstatic of classname * fieldname
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
	    Method of METHODACCESS list * methodname * methodsig * LIMITS * INSTRUCTION list * HANDLER list
	and
	    HANDLER =
	    Catch of classname * label * label * label
	and
	    FIELDTYPE =
	    Classtype of classname
	  | Sonstwas of int
	and
	    CLASSACCESS =
	    CPublic | CFinal | CSuper | CAbstract | CInterface
	and
	    FIELDACCESS =
	    FPublic | FPrivat | FProtected | FStatic | FFinal | FVolatile | FTransient
	and
	    METHODACCESS =
	    MPublic | MPrivat | MProtected | MStatic | MFinal | MSynchronized | FNative | FAbstract
	and
	    LIMITS =
	    Limits of int * int
    end
