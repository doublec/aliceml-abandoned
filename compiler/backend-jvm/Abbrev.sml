structure Abbrev  =
    struct
	open JVMInst

	val CEquals       = ("de/uni_sb/ps/dml/runtime/General/equals",
			     [Classsig "de/uni_sb/ps/dml/runtime/DMLValue"])
	val CAssign       = ("de/uni_sb/ps/dml/runtime/General/assign",
			     [Classsig "de/uni_sb/ps/dml/runtime/DMLValue"])
	val CRef          = ("de/uni_sb/ps/dml/runtime/Constants/reference",
			      [Classsig "de/uni_sb/ps/dml/runtime/Constructor"])
	val CBind          = ("de/uni_sb/ps/dml/runtime/General/bind",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CCons          = ("de/uni_sb/ps/dml/runtime/List/cons",
			      [Classsig "de/uni_sb/ps/dml/runtime/Constructor"])
	val CFalse         = ("de/uni_sb/ps/dml/runtime/Constants/dmlfalse",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CMatch         = ("de/uni_sb/ps/dml/runtime/Constants/dmlmatch",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CNil          = ("de/uni_sb/ps/dml/runtime/List/nil",
			     [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CPickle        = ("de/uni_sb/ps/dml/runtime/General/pickle",
			      [Classsig "de/uni_sb/ps/dml/runtime/DMLValue"])
	val CTrue          = ("de/uni_sb/ps/dml/runtime/Constants/dmltrue",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CUnit          = ("de/uni_sb/ps/dml/runtime/Constants/dmlunit",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val CName          = "de/uni_sb/ps/dml/runtime/Name"
	val CConstants     = "de/uni_sb/ps/dml/runtime/Constants"
	val CConstructor   = "de/uni_sb/ps/dml/runtime/Constructor"
	val CConVal        = "de/uni_sb/ps/dml/runtime/DMLConVal"
	val CExWrap        = "de/uni_sb/ps/dml/runtime/ExceptionWrapper"
	val CFcnClosure    = "de/uni_sb/ps/dml/runtime/Function"
	val CFuture        = "de/uni_sb/ps/dml/runtime/Future"
	val CInt           = "de/uni_sb/ps/dml/runtime/Int"
	val CWord          = "de/uni_sb/ps/dml/runtime/Word"
	val CChar          = "de/uni_sb/ps/dml/runtime/Char"
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
	val COut           = ("java/lang/System/out",
			      [Classsig "java/io/PrintStream"])
	val CPrintStream   = "java/io/PrintStream"
	val CString        = "java/lang/String"
	val CThread        = "java/lang/Thread"
	val CVector        = "java/util/Vector"
	val CPlus          = ("de/uni_sb/ps/dml/runtime/Int/plus",
			      [Classsig "de/uni_sb/ps/dml/runtime/DMLValue"])
	val CBuiltin       = "de/uni_sb/ps/dml/runtime/Builtin"
	val CBuilt         = ("de/uni_sb/ps/dml/runtime/General/bi",
			      [Classsig "de/uni_sb/ps/dml/runtime/DMLValue"])
	val CDMLTuple      = "de/uni_sb/ps/dml/runtime/DMLTuple"

	val CSelString     = "de/uni_sb/ps/dml/runtime/General$SelFunString"
	val CSelInt     = "de/uni_sb/ps/dml/runtime/General$SelFunInt"
    end
