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

structure StockWerk =
    struct
	val Prebound         = ["StockWerk", "Prebound"]
	val Komponist        = ["StockWerk", "Komponist"]
	val ExceptionWrapper = ["StockWerk", "ExceptionWrapper"]
	val RecordArity      = ["StockWerk", "RecordArity"]
	val StockWert        = ["StockWerk", "StockWert"]
	val Word             = ["StockWerk", "Word"]
	val Int              = ["StockWerk", "Int"]
	val Char             = ["StockWerk", "Char"]
	val String           = ["StockWerk", "String"]
	val Real             = ["StockWerk", "Real"]
	val NongenerativeCon = ["StockWerk", "NongenerativeCon"]
	val GenerativeCon    = ["StockWerk", "GenerativeCon"]
	val ConVal           = ["StockWerk", "ConVal"]
	val RefCon           = ["StockWerk", "RefCon"]
	val Ref              = ["StockWerk", "Ref"]
	val Tuple2           = ["StockWerk", "Tuple2"]
	val Tuple3           = ["StockWerk", "Tuple3"]
	val Tuple4           = ["StockWerk", "Tuple4"]
	val Tuple            = ["StockWerk", "Tuple"]
	val Record           = ["StockWerk", "Record"]
	val Vector           = ["StockWerk", "Vector"]
	val Transient        = ["StockWerk", "Transient"]
	val Promise          = ["StockWerk", "Promise"]
	val Future           = ["StockWerk", "Future"]
	val ByNeed           = ["StockWerk", "ByNeed"]
	val IntSelector      = ["StockWerk", "IntSelector"]
	val StringSelector   = ["StockWerk", "StringSelector"]
	val Procedure        = ["StockWerk", "Procedure"]
	val Procedure0       = ["StockWerk", "Procedure0"]
	val Procedure2       = ["StockWerk", "Procedure2"]
	val Procedure3       = ["StockWerk", "Procedure3"]
	val Procedure4       = ["StockWerk", "Procedure4"]

	val PreboundTy         = IL.ClassTy Prebound
	val KomponistTy        = IL.ClassTy Komponist
	val ExceptionWrapperTy = IL.ClassTy ExceptionWrapper
	val RecordArityTy      = IL.ClassTy RecordArity
	val StockWertTy        = IL.ClassTy StockWert
	val WordTy             = IL.ClassTy Word
	val IntTy              = IL.ClassTy Int
	val CharTy             = IL.ClassTy Char
	val StringTy           = IL.ClassTy String
	val RealTy             = IL.ClassTy Real
	val NongenerativeConTy = IL.ClassTy NongenerativeCon
	val GenerativeConTy    = IL.ClassTy GenerativeCon
	val ConValTy           = IL.ClassTy ConVal
	val RefConTy           = IL.ClassTy RefCon
	val RefTy              = IL.ClassTy Ref
	val Tuple2Ty           = IL.ClassTy Tuple2
	val Tuple3Ty           = IL.ClassTy Tuple3
	val Tuple4Ty           = IL.ClassTy Tuple4
	val TupleTy            = IL.ClassTy Tuple
	val RecordTy           = IL.ClassTy Record
	val VectorTy           = IL.ClassTy Vector
	val TransientTy        = IL.ClassTy Transient
	val PromiseTy          = IL.ClassTy Promise
	val FutureTy           = IL.ClassTy Future
	val ByNeedTy           = IL.ClassTy ByNeed
	val IntSelectorTy      = IL.ClassTy IntSelector
	val StringSelectorTy   = IL.ClassTy StringSelector
	val ProcedureTy        = IL.ClassTy Procedure
	val Procedure0Ty       = IL.ClassTy Procedure0
	val Procedure2Ty       = IL.ClassTy Procedure2
	val Procedure3Ty       = IL.ClassTy Procedure3
	val Procedure4Ty       = IL.ClassTy Procedure4
    end
