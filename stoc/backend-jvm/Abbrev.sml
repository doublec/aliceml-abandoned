structure Abbrev  =
    struct
	val CBind          = ("de/uni_sb/ps/dml/runtime/Constants/dmlbind",
			      "de/uni_sb/ps/dml/runtime/Name", 0)
	val CCons          = ("de/uni_sb/ps/dml/runtime/Constants/cons",
			      "de/uni-sb/ps/dml/runtime/Constructor", 0)
	val CFalse         = ("de/uni_sb/ps/dml/runtime/Constants/dmlfalse",
			      "de/uni_sb/ps/dml/runtime/Name", 0)
	val CMatch         = ("de/uni_sb/ps/dml/runtime/Constants/dmlmatch",
			      "de/uni_sb/ps/dml/runtime/Name", 0)
	val CNil          = ("de/uni_sb/ps/dml/runtime/Constants/dmlnil",
			     "de/uni_sb/ps/dml/runtime/Name", 0)
	val CPickle        = ("de/uni_sb/ps/dml/runtime/General/pickle",
			      "de/uni_sb/ps/dml/runtime/DMLValue", 0)
	val CTrue          = ("de/uni_sb/ps/dml/runtime/Constants/dmltrue",
			      "de/uni_sb/ps/dml/runtime/Name", 0)
	val CUnit          = ("de/uni_sb/ps/dml/runtime/Constants/dmlunit",
			      "de/uni_sb/ps/dml/runtime/Name", 0)
	val CName          = "de/uni_sb/ps/dml/runtime/Name"
	val CConstants     = "de/uni_sb/ps/dml/runtime/Constants"
	val CConstructor   = "de/uni_sb/ps/dml/runtime/Constructor"
	val CConVal        = "de/uni_sb/ps/dml/runtime/DMLConVal"
	val CExWrap        = "de/uni_sb/ps/dml/runtime/ExceptionWrapper"
	val CFcnClosure    = "de/uni_sb/ps/dml/runtime/Function"
	val CFuture        = "de/uni_sb/ps/dml/runtime/Future"
	val CInt           = "de/uni_sb/ps/dml/runtime/Int"
	val CInternalError = "de/uni_sb/ps/dml/runtime/DMLInternalError"
	val CLabel         = "de/uni_sb/ps/dml/runtime/Label"
	val CLVal          = "de/uni_sb/ps/dml/runtime/LVar"
	val CReal          = "de/uni_sb/ps/dml/runtime/Real"
	val CRecord        = "de/uni_sb/ps/dml/runtime/Record"
	val CRecordArity   = "de/uni_sb/ps/dml/runtime/RecordArity"
	val CReference     = "de/uni_sb/ps/dml/runtime/DMLReference"
	val CSCon          = "de/uni_sb/ps/dml/runtime/SCon"
	val CStr           = "de/uni_sb/ps/dml/runtime/String"
	val CDMLThread     = "de/uni_sb/ps/dml/runtime/Thread"
	val CTuple         = "de/uni_sb/ps/dml/runtime/Tuple"
	val CVal           = "de/uni_sb/ps/dml/runtime/DMLValue"
	val CHashtable     = "java/util/Hashtable"
	val CObj           = "java/lang/Object"
	val COut           = ("java/lang/System/out","java/io/PrintStream", 0)
	val CPrintStream   = "java/io/PrintStream"
	val CString        = "java/lang/String"
	val CThread        = "java/lang/Thread"
	val CVector        = "java/util/Vector"
	val CPlus          = ("de/uni_sb/ps/dml/runtime/Int/plus",
			      "de/uni_sb/ps/dml/runtime/DMLValue", 0)
	val CBuiltin       = "de/uni_sb/ps/dml/runtime/Builtin"
	val CDMLTuple      = "de/uni_sb/ps/dml/runtime/DMLTuple"

	(* Ab hier nur Unfug! *)
	val CEquals        = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/Equals",0 )
	val CNot           = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/Not",0)
	val CDeref         = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/Deref",0)
	val CRef           = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/Ref",0)
	val CAssign        = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/Assign",0)
	val CIntFromString = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/IntFromString",0)
	val CBoolFromString = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/BoolFromString",0)
	val CRealFromString = ("de/uni_sb/ps/dml/runtime/Builtin","de/uni_sb/ps/dml/runtime/RealFromString",0)
	val CSel            = "de/uni_sb/ps/dml/runtime/Builtin"
    end
