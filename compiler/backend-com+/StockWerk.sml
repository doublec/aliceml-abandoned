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
	val ExceptionWrapper = ["StockWerk", "ExceptionWrapper"]
	val RecordArity      = ["StockWerk", "RecordArity"]
	val StockWert        = ["StockWerk", "StockWert"]
	val Word             = ["StockWerk", "Word"]
	val Int              = ["StockWerk", "Int"]
	val Char             = ["StockWerk", "Char"]
	val String           = ["StockWerk", "String"]
	val Real             = ["StockWerk", "Real"]
	val Name             = ["StockWerk", "Name"]
	val NamedName        = ["StockWerk", "NamedName"]
	val Constructor      = ["StockWerk", "Constructor"]
	val NamedConstructor = ["StockWerk", "NamedConstructor"]
	val ConVal           = ["StockWerk", "ConVal"]
	val RefConstructor   = ["StockWerk", "RefConstructor"]
	val Ref              = ["StockWerk", "Ref"]
	val Tuple2           = ["StockWerk", "Tuple2"]
	val Tuple            = ["StockWerk", "Tuple"]
	val Record           = ["StockWerk", "Record"]
	val Vector           = ["StockWerk", "Vector"]
	val Transient        = ["StockWerk", "Transient"]
	val Promise          = ["StockWerk", "Promise"]
	val Future           = ["StockWerk", "Future"]
	val ByNeed           = ["StockWerk", "ByNeed"]
	val Selector         = ["StockWerk", "Selector"]
	val Procedure        = ["StockWerk", "Procedure"]
	val Procedure0       = ["StockWerk", "Procedure0"]
	val Procedure2       = ["StockWerk", "Procedure2"]

	val PreboundTy         = IL.ClassTy Prebound
	val ExceptionWrapperTy = IL.ClassTy ExceptionWrapper
	val RecordArityTy      = IL.ClassTy RecordArity
	val StockWertTy        = IL.ClassTy StockWert
	val WordTy             = IL.ClassTy Word
	val IntTy              = IL.ClassTy Int
	val CharTy             = IL.ClassTy Char
	val StringTy           = IL.ClassTy String
	val RealTy             = IL.ClassTy Real
	val NameTy             = IL.ClassTy Name
	val NamedNameTy        = IL.ClassTy NamedName
	val ConstructorTy      = IL.ClassTy Constructor
	val NamedConstructorTy = IL.ClassTy NamedConstructor
	val ConValTy           = IL.ClassTy ConVal
	val RefConstructorTy   = IL.ClassTy RefConstructor
	val RefTy              = IL.ClassTy Ref
	val Tuple2Ty           = IL.ClassTy Tuple2
	val TupleTy            = IL.ClassTy Tuple
	val RecordTy           = IL.ClassTy Record
	val VectorTy           = IL.ClassTy Vector
	val TransientTy        = IL.ClassTy Transient
	val PromiseTy          = IL.ClassTy Promise
	val FutureTy           = IL.ClassTy Future
	val ByNeedTy           = IL.ClassTy ByNeed
	val SelectorTy         = IL.ClassTy Selector
	val ProcedureTy        = IL.ClassTy Procedure
	val Procedure0Ty       = IL.ClassTy Procedure0
	val Procedure2Ty       = IL.ClassTy Procedure2
    end
