//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 1999
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

//
// Design of Serialization:
// -- StockWert implements ISerializable with the default serialization
//    behaviour
// -- replicable data structures (literals, tuples, records, vectors, names,
//    constructors including the reference constructor, and constructed
//    values except references) inherit the default serialization mechanism
// -- the abstract class Global represents entities that have a Guid.
//    Global overrides GetObjectData to generate a Guid upon serialization,
//    then fall back to StockWert::GetObjectData.  Global implements
//    IObjectReference, replacing itself upon deserialization by the unique
//    representation of the entity if it was already present in the system
// -- NongenericConstructor and GenericConstructor extend Global,
//    the first using a string, the second a Guid as identifier
// -- Transient overrides GetObjectData to request itself, then to fall back
//    to StockWert::GetObjectData.  Transient implements IObjectReference to
//    remove the indirection upon deserialization
// -- Thread and Ref implement ISerializable::GetObjectData to throw a
//    Pickle exception
//

//
// Design Limitations:
// -- Tuple/Record need an additional array object and pointer to it.
//    In C, these would be allocated as part of the tuple object.
//

.namespace StockWerk {
  .class sealed public Prebound extends System.Object {
    .field static public initonly class StockWerk.StockWert ref
    .field static public initonly class StockWerk.StockWert unit
    // Nongeneric Constructors
    .field static public initonly class StockWerk.StockWert EQUAL
    .field static public initonly class StockWerk.StockWert GREATER
    .field static public initonly class StockWerk.StockWert LESS
    .field static public initonly class StockWerk.StockWert cons
    .field static public initonly class StockWerk.StockWert false
    .field static public initonly class StockWerk.StockWert nil
    .field static public initonly class StockWerk.StockWert true
    // Exception Constructors
    .field static public initonly class StockWerk.StockWert Assembler$UnknownMember
    .field static public initonly class StockWerk.StockWert Assembler$UnknownType
    .field static public initonly class StockWerk.StockWert Component$Component
    .field static public initonly class StockWerk.StockWert General$Bind
    .field static public initonly class StockWerk.StockWert General$Chr
    .field static public initonly class StockWerk.StockWert General$Div
    .field static public initonly class StockWerk.StockWert General$Domain
    .field static public initonly class StockWerk.StockWert General$Fail
    .field static public initonly class StockWerk.StockWert General$Match
    .field static public initonly class StockWerk.StockWert General$Overflow
    .field static public initonly class StockWerk.StockWert General$Size
    .field static public initonly class StockWerk.StockWert General$Span
    .field static public initonly class StockWerk.StockWert General$Subscript
    .field static public initonly class StockWerk.StockWert General$TypeError
    .field static public initonly class StockWerk.StockWert List$Empty
    .field static public initonly class StockWerk.StockWert Option$Option
    .field static public initonly class StockWerk.StockWert Pickle$Pickle
    .field static public initonly class StockWerk.StockWert Transient$ByNeed
    .field static public initonly class StockWerk.StockWert Transient$Fulfill
    .field static public initonly class StockWerk.StockWert Transient$Future
    .field static public initonly class StockWerk.StockWert Transient$Promise
    // Builtins
    .field static public initonly class StockWerk.StockWert show
    .field static public initonly class StockWerk.StockWert eq
    .field static public initonly class StockWerk.StockWert ne
    .field static public initonly class StockWerk.StockWert cast
    .field static public initonly class StockWerk.StockWert Application$exit
    .field static public initonly class StockWerk.StockWert Application$getArgs
    .field static public initonly class StockWerk.StockWert Array$array
    .field static public initonly class StockWerk.StockWert Array$fromList
    .field static public initonly class StockWerk.StockWert Array$length
    .field static public initonly class StockWerk.StockWert Array$sub
    .field static public initonly class StockWerk.StockWert Array$update
    .field static public initonly class StockWerk.StockWert Assembler$beginCatchBlock
    .field static public initonly class StockWerk.StockWert Assembler$beginExceptionBlock
    .field static public initonly class StockWerk.StockWert Assembler$constructorDeclareLocal
    .field static public initonly class StockWerk.StockWert Assembler$constructorGetILGenerator
    .field static public initonly class StockWerk.StockWert Assembler$createLabel
    .field static public initonly class StockWerk.StockWert Assembler$createType
    .field static public initonly class StockWerk.StockWert Assembler$defineConstructor
    .field static public initonly class StockWerk.StockWert Assembler$defineField
    .field static public initonly class StockWerk.StockWert Assembler$defineMethod
    .field static public initonly class StockWerk.StockWert Assembler$defineStaticMethod
    .field static public initonly class StockWerk.StockWert Assembler$defineType
    .field static public initonly class StockWerk.StockWert Assembler$endExceptionBlock
    .field static public initonly class StockWerk.StockWert Assembler$emit
    .field static public initonly class StockWerk.StockWert Assembler$emitConstructor
    .field static public initonly class StockWerk.StockWert Assembler$emitField
    .field static public initonly class StockWerk.StockWert Assembler$emitFloat32
    .field static public initonly class StockWerk.StockWert Assembler$emitInt8
    .field static public initonly class StockWerk.StockWert Assembler$emitInt16
    .field static public initonly class StockWerk.StockWert Assembler$emitInt32
    .field static public initonly class StockWerk.StockWert Assembler$emitLabel
    .field static public initonly class StockWerk.StockWert Assembler$emitMethod
    .field static public initonly class StockWerk.StockWert Assembler$emitString
    .field static public initonly class StockWerk.StockWert Assembler$emitType
    .field static public initonly class StockWerk.StockWert Assembler$emitUnsignedInt8
    .field static public initonly class StockWerk.StockWert Assembler$getConstructor
    .field static public initonly class StockWerk.StockWert Assembler$getField
    .field static public initonly class StockWerk.StockWert Assembler$getMethod
    .field static public initonly class StockWerk.StockWert Assembler$getType
    .field static public initonly class StockWerk.StockWert Assembler$invoke
    .field static public initonly class StockWerk.StockWert Assembler$markLabel
    .field static public initonly class StockWerk.StockWert Assembler$methodDeclareLocal
    .field static public initonly class StockWerk.StockWert Assembler$methodGetILGenerator
    .field static public initonly class StockWerk.StockWert Char$chr
    .field static public initonly class StockWerk.StockWert Char$le
    .field static public initonly class StockWerk.StockWert Char$isDigit
    .field static public initonly class StockWerk.StockWert Char$isHexDigit
    .field static public initonly class StockWerk.StockWert Char$isSpace
    .field static public initonly class StockWerk.StockWert Char$ord
    .field static public initonly class StockWerk.StockWert Char$toCString
    .field static public initonly class StockWerk.StockWert General$assign
    .field static public initonly class StockWerk.StockWert General$exchange
    .field static public initonly class StockWerk.StockWert Int$compare
    .field static public initonly class StockWerk.StockWert Int$div
    .field static public initonly class StockWerk.StockWert Int$ge
    .field static public initonly class StockWerk.StockWert Int$gt
    .field static public initonly class StockWerk.StockWert Int$le
    .field static public initonly class StockWerk.StockWert Int$lt
    .field static public initonly class StockWerk.StockWert Int$minus
    .field static public initonly class StockWerk.StockWert Int$mod
    .field static public initonly class StockWerk.StockWert Int$plus
    .field static public initonly class StockWerk.StockWert Int$times
    .field static public initonly class StockWerk.StockWert Int$toString
    .field static public initonly class StockWerk.StockWert Int$uminus
    .field static public initonly class StockWerk.StockWert Pickle$load
    .field static public initonly class StockWerk.StockWert Pickle$save
    .field static public initonly class StockWerk.StockWert String$compare
    .field static public initonly class StockWerk.StockWert String$conc
    .field static public initonly class StockWerk.StockWert String$explode
    .field static public initonly class StockWerk.StockWert String$size
    .field static public initonly class StockWerk.StockWert String$str
    .field static public initonly class StockWerk.StockWert String$sub
    .field static public initonly class StockWerk.StockWert String$substring
    .field static public initonly class StockWerk.StockWert TextIO$closeIn
    .field static public initonly class StockWerk.StockWert TextIO$closeOut
    .field static public initonly class StockWerk.StockWert TextIO$inputAll
    .field static public initonly class StockWerk.StockWert TextIO$openIn
    .field static public initonly class StockWerk.StockWert TextIO$openOut
    .field static public initonly class StockWerk.StockWert TextIO$output
    .field static public initonly class StockWerk.StockWert TextIO$output1
    .field static public initonly class StockWerk.StockWert TextIO$print
    .field static public initonly class StockWerk.StockWert TextIO$stdErr
    .field static public initonly class StockWerk.StockWert TextIO$stdIn
    .field static public initonly class StockWerk.StockWert TextIO$stdOut
    .field static public initonly class StockWerk.StockWert Thread$current
    .field static public initonly class StockWerk.StockWert Thread$resume
    .field static public initonly class StockWerk.StockWert Thread$sleep
    .field static public initonly class StockWerk.StockWert Thread$spawn
    .field static public initonly class StockWerk.StockWert Thread$suspend
    .field static public initonly class StockWerk.StockWert Transient$await
    .field static public initonly class StockWerk.StockWert Transient$byNeed
    .field static public initonly class StockWerk.StockWert Transient$fulfill
    .field static public initonly class StockWerk.StockWert Transient$future
    .field static public initonly class StockWerk.StockWert Transient$promise
    .field static public initonly class StockWerk.StockWert Vector$fromList
    .field static public initonly class StockWerk.StockWert Vector$sub
    .field static public initonly class StockWerk.StockWert Word$andb
    .field static public initonly class StockWerk.StockWert Word$asr
    .field static public initonly class StockWerk.StockWert Word$fromInt2
    .field static public initonly class StockWerk.StockWert Word$lsr
    .field static public initonly class StockWerk.StockWert Word$minus
    .field static public initonly class StockWerk.StockWert Word$mod
    .field static public initonly class StockWerk.StockWert Word$notb
    .field static public initonly class StockWerk.StockWert Word$orb
    .field static public initonly class StockWerk.StockWert Word$plus
    .field static public initonly class StockWerk.StockWert Word$shl
    .field static public initonly class StockWerk.StockWert Word$times
    .field static public initonly class StockWerk.StockWert Word$toInt
    .field static public initonly class StockWerk.StockWert Word$toIntX
    .field static public initonly class StockWerk.StockWert Word$toString
    .field static public initonly class StockWerk.StockWert Word$xorb
    .method public static void .cctor() {
      newobj instance void StockWerk.RefConstructor::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::ref
      ldstr "()"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::unit
      // Names, Constructors, and Exceptions
      ldstr "EQUAL"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::EQUAL
      ldstr "GREATER"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::GREATER
      ldstr "LESS"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::LESS
      ldstr "::"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::cons
      ldstr "false"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::false
      ldstr "nil"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::nil
      ldstr "true"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::true
      ldstr "Assembler.UnknownMember"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownMember
      ldstr "Assembler.UnknownType"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownType
      ldstr "Component.Component"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Component$Component
      ldstr "General.Bind"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Bind
      ldstr "General.Char"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Chr
      ldstr "General.Div"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Div
      ldstr "General.Domain"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Domain
      ldstr "General.Fail"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Fail
      ldstr "General.Match"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      ldstr "General.Overflow"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
      ldstr "General.Size"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Size
      ldstr "General.Span"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Span
      ldstr "General.Subscript"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
      ldstr "General.TypeError"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      ldstr "List.Empty"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::List$Empty
      ldstr "Option.Option"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Option$Option
      ldstr "Pickle.Pickle"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$Pickle
      ldstr "Transient.ByNeed"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$ByNeed
      ldstr "Transient.Fulfill"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Fulfill
      ldstr "Transient.Future"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Future
      ldstr "Transient.Promise"
      call class StockWerk.NongenericConstructor StockWerk.NongenericConstructor::Make(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Promise
      // Builtins
      newobj instance void StockWerk.Builtins.show::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::show
      newobj instance void StockWerk.Builtins.eq::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::eq
      newobj instance void StockWerk.Builtins.ne::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::ne
      newobj instance void StockWerk.Builtins.Application$exit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Application$exit
      newobj instance void StockWerk.Builtins.Application$getArgs::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Application$getArgs
      newobj instance void StockWerk.Builtins.Array$array::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$array
      newobj instance void StockWerk.Builtins.Array$fromList::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$fromList
      newobj instance void StockWerk.Builtins.Array$length::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$length
      newobj instance void StockWerk.Builtins.Array$sub::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$sub
      newobj instance void StockWerk.Builtins.Array$update::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$update
      newobj instance void StockWerk.Builtins.Assembler$beginCatchBlock::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$beginCatchBlock
      newobj instance void StockWerk.Builtins.Assembler$beginExceptionBlock::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$beginExceptionBlock
      newobj instance void StockWerk.Builtins.Assembler$constructorDeclareLocal::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$constructorDeclareLocal
      newobj instance void StockWerk.Builtins.Assembler$constructorGetILGenerator::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$constructorGetILGenerator
      newobj instance void StockWerk.Builtins.Assembler$createLabel::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$createLabel
      newobj instance void StockWerk.Builtins.Assembler$createType::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$createType
      newobj instance void StockWerk.Builtins.Assembler$defineConstructor::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$defineConstructor
      newobj instance void StockWerk.Builtins.Assembler$defineField::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$defineField
      newobj instance void StockWerk.Builtins.Assembler$defineMethod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$defineMethod
      newobj instance void StockWerk.Builtins.Assembler$defineStaticMethod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$defineStaticMethod
      newobj instance void StockWerk.Builtins.Assembler$defineType::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$defineType
      newobj instance void StockWerk.Builtins.Assembler$endExceptionBlock::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$endExceptionBlock
      newobj instance void StockWerk.Builtins.Assembler$emit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emit
      newobj instance void StockWerk.Builtins.Assembler$emitConstructor::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitConstructor
      newobj instance void StockWerk.Builtins.Assembler$emitField::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitField
      newobj instance void StockWerk.Builtins.Assembler$emitFloat32::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitFloat32
      newobj instance void StockWerk.Builtins.Assembler$emitInt8::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitInt8
      newobj instance void StockWerk.Builtins.Assembler$emitInt16::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitInt16
      newobj instance void StockWerk.Builtins.Assembler$emitInt32::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitInt32
      newobj instance void StockWerk.Builtins.Assembler$emitLabel::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitLabel
      newobj instance void StockWerk.Builtins.Assembler$emitMethod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitMethod
      newobj instance void StockWerk.Builtins.Assembler$emitString::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitString
      newobj instance void StockWerk.Builtins.Assembler$emitType::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitType
      newobj instance void StockWerk.Builtins.Assembler$emitUnsignedInt8::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$emitUnsignedInt8
      newobj instance void StockWerk.Builtins.Assembler$getConstructor::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$getConstructor
      newobj instance void StockWerk.Builtins.Assembler$getField::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$getField
      newobj instance void StockWerk.Builtins.Assembler$getMethod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$getMethod
      newobj instance void StockWerk.Builtins.Assembler$getType::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$getType
      newobj instance void StockWerk.Builtins.Assembler$invoke::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$invoke
      newobj instance void StockWerk.Builtins.Assembler$markLabel::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$markLabel
      newobj instance void StockWerk.Builtins.Assembler$methodDeclareLocal::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$methodDeclareLocal
      newobj instance void StockWerk.Builtins.Assembler$methodGetILGenerator::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$methodGetILGenerator
      newobj instance void StockWerk.Builtins.Char$isDigit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isDigit
      newobj instance void StockWerk.Builtins.Char$isHexDigit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isHexDigit
      newobj instance void StockWerk.Builtins.Char$isSpace::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isSpace
      newobj instance void StockWerk.Builtins.Char$le::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$le
      newobj instance void StockWerk.Builtins.Char$chr::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$chr
      newobj instance void StockWerk.Builtins.Char$ord::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$ord
      newobj instance void StockWerk.Builtins.Char$toCString::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$toCString
      newobj instance void StockWerk.Builtins.General$assign::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$assign
      newobj instance void StockWerk.Builtins.General$exchange::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$exchange
      newobj instance void StockWerk.Builtins.Int$compare::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$compare
      newobj instance void StockWerk.Builtins.Int$div::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$div
      newobj instance void StockWerk.Builtins.Int$ge::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$ge
      newobj instance void StockWerk.Builtins.Int$gt::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$gt
      newobj instance void StockWerk.Builtins.Int$le::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$le
      newobj instance void StockWerk.Builtins.Int$lt::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$lt
      newobj instance void StockWerk.Builtins.Int$minus::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$minus
      newobj instance void StockWerk.Builtins.Int$mod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$mod
      newobj instance void StockWerk.Builtins.Int$plus::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$plus
      newobj instance void StockWerk.Builtins.Int$times::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$times
      newobj instance void StockWerk.Builtins.Int$toString::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$toString
      newobj instance void StockWerk.Builtins.Int$uminus::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$uminus
      newobj instance void StockWerk.Builtins.Pickle$load::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$load
      newobj instance void StockWerk.Builtins.Pickle$save::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$save
      newobj instance void StockWerk.Builtins.String$compare::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$compare
      newobj instance void StockWerk.Builtins.String$conc::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$conc
      newobj instance void StockWerk.Builtins.String$explode::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$explode
      newobj instance void StockWerk.Builtins.String$size::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$size
      newobj instance void StockWerk.Builtins.String$str::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$str
      newobj instance void StockWerk.Builtins.String$sub::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$sub
      newobj instance void StockWerk.Builtins.String$substring::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::String$substring
      newobj instance void StockWerk.Builtins.TextIO$closeIn::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$closeIn
      newobj instance void StockWerk.Builtins.TextIO$closeOut::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$closeOut
      newobj instance void StockWerk.Builtins.TextIO$inputAll::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$inputAll
      newobj instance void StockWerk.Builtins.TextIO$openIn::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$openIn
      newobj instance void StockWerk.Builtins.TextIO$openOut::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$openOut
      newobj instance void StockWerk.Builtins.TextIO$output::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$output
      newobj instance void StockWerk.Builtins.TextIO$output1::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$output1
      newobj instance void StockWerk.Builtins.TextIO$print::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$print
      ldsfld class System.IO.TextWriter System.Console::Error
      castclass System.IO.StreamWriter
      callvirt instance class System.IO.Stream System.IO.StreamWriter::GetStream()
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$stdErr
      ldsfld class System.IO.TextReader System.Console::In
      castclass System.IO.StreamReader
      callvirt instance class System.IO.Stream System.IO.StreamReader::GetStream()
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$stdIn
      ldsfld class System.IO.TextWriter System.Console::Out
      castclass System.IO.StreamWriter
      callvirt instance class System.IO.Stream System.IO.StreamWriter::GetStream()
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      stsfld class StockWerk.StockWert StockWerk.Prebound::TextIO$stdOut
      newobj instance void StockWerk.Builtins.Thread$current::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Thread$current
      newobj instance void StockWerk.Builtins.Thread$resume::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Thread$resume
      newobj instance void StockWerk.Builtins.Thread$sleep::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Thread$sleep
      newobj instance void StockWerk.Builtins.Thread$spawn::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Thread$spawn
      newobj instance void StockWerk.Builtins.Thread$suspend::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Thread$suspend
      newobj instance void StockWerk.Builtins.Transient$await::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$await
      newobj instance void StockWerk.Builtins.Transient$byNeed::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$byNeed
      newobj instance void StockWerk.Builtins.Transient$fulfill::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$fulfill
      newobj instance void StockWerk.Builtins.Transient$future::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$future
      newobj instance void StockWerk.Builtins.Transient$promise::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Transient$promise
      newobj instance void StockWerk.Builtins.Vector$fromList::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Vector$fromList
      newobj instance void StockWerk.Builtins.Vector$sub::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Vector$sub
      newobj instance void StockWerk.Builtins.Word$andb::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$andb
      newobj instance void StockWerk.Builtins.Word$asr::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$asr
      newobj instance void StockWerk.Builtins.Word$fromInt2::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$fromInt2
      newobj instance void StockWerk.Builtins.Word$lsr::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$lsr
      newobj instance void StockWerk.Builtins.Word$minus::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$minus
      newobj instance void StockWerk.Builtins.Word$mod::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$mod
      newobj instance void StockWerk.Builtins.Word$notb::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$notb
      newobj instance void StockWerk.Builtins.Word$orb::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$orb
      newobj instance void StockWerk.Builtins.Word$plus::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$plus
      newobj instance void StockWerk.Builtins.Word$shl::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$shl
      newobj instance void StockWerk.Builtins.Word$times::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$times
      newobj instance void StockWerk.Builtins.Word$toInt::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$toInt
      newobj instance void StockWerk.Builtins.Word$toIntX::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$toIntX
      newobj instance void StockWerk.Builtins.Word$toString::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$toString
      newobj instance void StockWerk.Builtins.Word$xorb::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Word$xorb
      ret
    }
  }

  .class sealed Util extends System.Object {
    .method public static int32 TestList(class StockWerk.StockWert&) {
      .locals(int32, class StockWerk.ConVal, class StockWerk.Tuple2, class StockWerk.StockWert, class StockWerk.StockWert)
      ldc.i4.0
      stloc.0
      ldnull
      stloc.s 2
      ldarg.0
      ldind.ref
    loop:
      dup
      isinst StockWerk.ConVal
      brfalse.s testNil
      castclass StockWerk.ConVal
      stloc.1
      ldloc.1
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      dup
      isinst StockWerk.Tuple2
      brtrue.s isTuple
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      stloc.3
      ldloc.1
      ldloc.3
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      ldloc.3
    isTuple:
      castclass StockWerk.Tuple2
      stloc.2
      ldloc.0
      ldc.i4.1
      add
      stloc.0
      ldloc.2
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      br.s loop
    testNil:
      dup
      ldsfld class StockWerk.StockWert StockWerk.Prebound::nil
      beq.s isNil
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      stloc.3
      ldloc.2
      brnull.s head
      ldloc.2
      ldloc.3
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      ldloc.3
      br.s loop
    head:
      ldarg.0
      ldloc.3
      stind.ref
      ldloc.3
      br.s loop
    isNil:
      pop
      ldloc.0
      ret
    }
  }

  .class sealed public ExceptionWrapper extends System.SystemException {
    .field public class StockWerk.StockWert Value
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      callvirt instance class System.String System.Object::ToString()
      call instance void System.SystemException::.ctor(class System.String)
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
      ret
    }
    .method public void .ctor(class StockWerk.StockWert, int32) {
      ldarg.0
      ldarg.1
      callvirt instance class System.String System.Object::ToString()
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldstr " thrown at line "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.2
      call instance class System.StringBuilder System.StringBuilder::Append(int32)
      call instance class System.String System.StringBuilder::ToString()
      call instance void System.SystemException::.ctor(class System.String)
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
      ret
    }
  }

  .class public sealed Komponist extends System.Object {
    .field private class System.Collections.Hashtable Table
    .method public void .ctor() {
      ldarg.0
      call instance void System.Object::.ctor()
      ldarg.0
      newobj instance void System.Collections.Hashtable::.ctor()
      stfld class System.Collections.Hashtable StockWerk.Komponist::Table
      ret
    }
    .method public class StockWerk.StockWert Import(class System.String) {
      .locals(class StockWerk.StockWert)
      ldarg.0
      ldfld class System.Collections.Hashtable StockWerk.Komponist::Table
      ldarg.1
      call instance class System.Object System.Collections.Hashtable::GetItem(class System.Object)
      dup
      brnull.s load
      castclass StockWerk.StockWert
      ret
    load:
      pop
      ldarg.0
      ldfld class System.Collections.Hashtable StockWerk.Komponist::Table
      ldarg.1
      ldarg.0
      ldarg.1
      newobj instance void StockWerk.Builtins.Internal.Component$load::.ctor(class StockWerk.Komponist, class System.String)
      newobj instance void StockWerk.ByNeed::.ctor(class StockWerk.StockWert)
      dup
      stloc.0
      call instance void System.Collections.Hashtable::Add(class System.Object, class System.Object)
      ldloc.0
      ret
    }
  }

  .class sealed public RecordArity extends System.Object implements System.Serialization.IObjectReference {
    .field private static class System.Collections.Hashtable ArityTable
    .field private class System.String[] Labels   //--** store as hashtable?
    .method public static void .cctor() {
      newobj instance void System.Collections.Hashtable::.ctor()
      stsfld class System.Collections.Hashtable StockWerk.RecordArity::ArityTable
      ret
    }
    .method private void .ctor(class System.String[]) {
      ldarg.0
      call instance void System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String[] StockWerk.RecordArity::Labels
      ret
    }
    .method static public class StockWerk.RecordArity MakeRecordArity(class System.String[]) {
      ldarg.0
      newobj instance void StockWerk.RecordArity::.ctor(class System.String[])
      call instance class System.Object StockWerk.RecordArity::GetRealObject()
      castclass StockWerk.RecordArity
      ret
    }
    .method virtual public int32 GetHashCode() {
      .locals(int32, int32, class System.String[])
      ldc.i4.0
      stloc.0
      ldc.i4.0
      stloc.1
      ldarg.0
      ldfld class System.String[] StockWerk.RecordArity::Labels
      stloc.2
    loop:
      ldloc.0
      ldloc.2
      ldlen
      bge.s return
      ldloc.2
      ldloc.0
      ldelem.ref
      callvirt instance int32 System.Object::GetHashCode()
      ldloc.1
      xor
      stloc.1
      ldloc.0
      ldc.i4.1
      add
      stloc.0
      br.s loop
    return:
      ldloc.1
      ret
    }
    .method virtual bool Equals(class System.Object) {
      .locals(class System.String[], class System.String[], int32)
      ldarg.1
      isinst StockWerk.RecordArity
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.RecordArity
      ldfld class System.String[] StockWerk.RecordArity::Labels
      dup
      stloc.0
      ldlen
      ldarg.0
      ldfld class System.String[] StockWerk.RecordArity::Labels
      dup
      stloc.1
      ldlen
      bne.un.s retFalse
      ldc.i4.0
      stloc.2
    loop:
      ldloc.2
      ldloc.0
      ldlen
      bge.s retTrue
      ldloc.0
      ldloc.2
      ldelem.ref
      ldloc.1
      ldloc.2
      ldelem.ref
      call bool System.String::Equals(class System.String, class System.String)
      brfalse.s retFalse
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    retFalse:
      ldc.i4.0
      ret
    retTrue:
      ldc.i4.1
      ret
    }
    .method instance class System.Object GetRealObject() {
      ldsfld class System.Collections.Hashtable StockWerk.RecordArity::ArityTable
      ldarg.0
      call instance class System.Object System.Collections.Hashtable::GetItem(class System.Object)
      dup
      brnull.s enter
      ret
    enter:
      pop
      ldsfld class System.Collections.Hashtable StockWerk.RecordArity::ArityTable
      ldarg.0
      ldarg.0
      call instance void System.Collections.Hashtable::Add(class System.Object, class System.Object)
      ldarg.0
      ret
    }
    .method virtual public class System.Object GetRealObject(value class System.Serialization.StreamingContext) {
      ldarg.0
      tail. call instance class System.Object StockWerk.RecordArity::GetRealObject()
      ret
    }
    .method public int32 Select(class System.String) {
      .locals(class System.String[], int32, int32)
      ldarg.0
      ldfld class System.String[] StockWerk.RecordArity::Labels
      dup
      stloc.0
      ldlen
      stloc.2
      ldc.i4.0
      stloc.1
    loop:
      ldloc.1
      ldloc.2
      bge.s notFound
      ldloc.0
      ldloc.1
      ldelem.ref
      ldarg.1
      call bool System.String::Equals(class System.String, class System.String)
      brtrue.s return
      ldloc.1
      ldc.i4.1
      add
      stloc.1
      br.s loop
    return:
      ldloc.1
      ret
    notFound:
      ldc.i4.M1
      ret
    }
  }

  //
  // Value Representation
  //

  .class abstract public StockWert extends System.Object implements System.Serialization.ISerializable {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void System.Object::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.0
      ldarg.1
      bne.un.s testTransient
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldc.i4.0
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Future() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public void Fulfill(class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Fulfill
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Await() {
      ldarg.0
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      .locals(class System.Reflection.MemberInfo[], class System.String[], int32)
      ldarg.0
      call instance class System.Type System.Object::GetType()
      ldarg.1
      call class System.Reflection.MemberInfo[] System.Serialization.FormatterServices::GetSerializableMembers(class System.Type, value class System.Serialization.StreamingContext)
      stloc.0
      ldloc.0
      ldlen
      dup
      stloc.2
      newarr class System.String
      stloc.1
    loop:
      ldloc.2
      ldc.i4.0
      beq.s done
      ldloc.2
      ldc.i4.1
      sub
      stloc.2
      ldloc.1
      ldloc.2
      ldloc.0
      ldloc.2
      ldelem.ref
      callvirt instance class System.String System.Reflection.MemberInfo::GetName()
      stelem.ref
      br.s loop
    done:
      ldarg.0
      call instance class System.Type System.Object::GetType()
      ldloc.1
      ldarg.0
      ldloc.0
      call value class System.Variant[] System.Serialization.FormatterServices::GetObjectData(class System.Object, class System.Reflection.MemberInfo[])
      newobj instance void System.Serialization.SerializationInfo::.ctor(class System.Type, class System.String[], value class System.Variant[] data)
      ret
    }
    .method virtual public void SetObjectData(class System.Serialization.SerializationInfo, value class System.Serialization.StreamingContext, value class System.Version) {
      .locals(class System.Serialization.SerializationInfoEnumerator, class System.Type)
      ldarg.1
      callvirt instance class System.Serialization.SerializationInfoEnumerator System.Serialization.SerializationInfo::GetEnumerator()
      stloc.0
      ldarg.0
      call instance class System.Type System.Object::GetType()
      stloc.1
    loop:
      ldloc.0
      call instance bool System.Serialization.SerializationInfoEnumerator::MoveNext()
      brfalse.s return
      ldloc.1
      ldloc.0
      call instance class System.String System.Serialization.SerializationInfoEnumerator::GetName()
      //--** ldc.i4.1
      call instance class System.Reflection.FieldInfo System.Type::GetField(class System.String/*, bool*/)
      ldarg.0
      ldloc.0
      call instance value class System.Variant System.Serialization.SerializationInfoEnumerator::GetValue()
      call instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
      br.s loop
    return:
      ret
    }
  }

  .class abstract public Global extends StockWerk.StockWert implements System.Serialization.IObjectReference {
    .field static private initonly class System.Collections.Hashtable GuidTable
    .field public class System.Object Guid   //--** use private instead of public
    .method public static void .cctor() {
      newobj instance void System.Collections.Hashtable::.ctor()
      stsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ret
    }
    .method public void .ctor() {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldnull
      stfld class System.Object StockWerk.Global::Guid
      ret
    }
    .method public static void Enter(class System.Object, class StockWerk.StockWert) {
      ldsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ldarg.0
      ldarg.1
      newobj instance void System.WeakReference::.ctor(class System.Object)
      tail. call instance void System.Collections.Hashtable::Add(class System.Object, class System.Object)
      ret
    }
    .method public static class System.Object Globalize(class StockWerk.StockWert) {
      .locals(value class System.Guid)
      call value class System.Guid System.Guid::NewGuid()
      stloc.0
      ldloca.s 0
      box System.Guid
      ldarg.0
      call void StockWerk.Global::Enter(class System.Object, class StockWerk.StockWert)
      ldarg.0
      ret
    }
    .method public static class StockWerk.StockWert Localize(class System.Object, class StockWerk.StockWert) {
      ldsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ldarg.0
      call instance class System.Object System.Collections.Hashtable::GetItem(class System.Object)
      dup
      brnull.s enter
      castclass System.WeakReference
      call instance class System.Object System.WeakReference::GetTarget()
      dup
      brnull.s enter
      castclass StockWerk.StockWert
      ret
    enter:
      pop
      ldarg.0
      ldarg.1
      call void StockWerk.Global::Enter(class System.Object, class StockWerk.StockWert)
      ldarg.1
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldarg.0
      ldfld class System.Object StockWerk.Global::Guid
      brtrue.s serialize
      ldarg.0
      dup
      call class System.Object StockWerk.Global::Globalize(class StockWerk.StockWert)
      stfld class System.Object StockWerk.Global::Guid
    serialize:
      ldarg.0
      ldarga.s 1
      ldobj System.Serialization.StreamingContext
      tail. call instance class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(value class System.Serialization.StreamingContext)
      ret
    }
    .method virtual public class System.Object GetRealObject(value class System.Serialization.StreamingContext) {
      ldarg.0
      ldfld class System.Object StockWerk.Global::Guid
      ldarg.0
      call class StockWerk.StockWert StockWerk.Global::Localize(class System.Object, class StockWerk.StockWert)
      ret
    }
  }

  //
  // Literals
  //

  .class sealed public Word extends StockWerk.StockWert {
    .field public int32 Value
    .method public void .ctor(int32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld int32 StockWerk.Word::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.0
      ldfld int32 StockWerk.Word::Value
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class sealed public Int extends StockWerk.StockWert {
    .field public int32 Value
    .method public void .ctor(int32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld int32 StockWerk.Int::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.0
      ldfld int32 StockWerk.Int::Value
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld int32 StockWerk.Int::Value
      tail. call class System.String System.Int32::ToString(int32)
      ret
    }
  }

  .class sealed public Char extends StockWerk.StockWert {
    .field public wchar Value
    .method public void .ctor(wchar) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld wchar StockWerk.Char::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      ldarg.0
      ldfld wchar StockWerk.Char::Value
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class sealed public String extends StockWerk.StockWert {
    .field public class System.String Value
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.String::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldarg.0
      ldfld class System.String StockWerk.String::Value
      tail. call bool System.String::Equals(class System.String, class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class System.String ToString() {
      //--** escape characters
      ldstr "\""
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.0
      ldfld class System.String StockWerk.String::Value
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldstr "\""
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class sealed public Real extends StockWerk.StockWert {
    .field public float32 Value
    .method public void .ctor(float32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld float32 StockWerk.Real::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Real
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Real
      ldfld float32 StockWerk.Real::Value
      ldarg.0
      ldfld float32 StockWerk.Real::Value
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld float32 StockWerk.Real::Value
      call class System.String System.Single::ToString(float32)
      dup
      ldc.i4.s 46   // '.'
      callvirt instance int32 System.String::IndexOf(wchar)
      ldc.i4.M1
      bne.un.s return
      dup
      ldc.i4.s 101   // 'e'
      callvirt instance int32 System.String::IndexOf(wchar)
      ldc.i4.M1
      bne.un.s return
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldstr ".0"
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      tail. call instance class System.String System.StringBuilder::ToString()
    return:
      ret
    }
  }

  //
  // Names, Constructors, and Constructed Values
  //

  .class sealed public NongenericConstructor extends StockWerk.Global {
    .method private void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.Global::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.Object StockWerk.Global::Guid
      ret
    }
    .method public static class StockWerk.NongenericConstructor Make(class System.String) {
      ldarg.0
      ldarg.0
      newobj instance void StockWerk.NongenericConstructor::.ctor(class System.String)
      call class StockWerk.StockWert StockWerk.Global::Localize(class System.Object, class StockWerk.StockWert)
      castclass StockWerk.NongenericConstructor
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.StockWert)
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      ret
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld class System.Object StockWerk.Global::Guid
      castclass System.String
      ret
    }
  }

  .class sealed public GenericConstructor extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Global::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.StockWert)
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      ret
    }
    .method virtual public class System.String ToString() {
      ldstr "<C>"   //--** include hash value or Guid
      ret
    }
  }

  .class sealed public ConVal extends StockWerk.StockWert {
    .field public class StockWerk.StockWert Con
    .field public class StockWerk.StockWert Val
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ConVal::Con
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.ConVal
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ConVal
      dup
      ldfld class StockWerk.StockWert StockWerk.ConVal::Con
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ConVal::Con
      bne.un.s retFalse
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    retFalse:
      ldc.i4.0
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ConVal::Con
      callvirt instance class System.String System.Object::ToString()
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldstr " "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class sealed public RefConstructor extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      newobj instance void StockWerk.Ref::.ctor(class StockWerk.StockWert)
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.RefConstructor
      brfalse.s testTransient
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class sealed public Ref extends StockWerk.StockWert {
    .field private class StockWerk.StockWert Value
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Access() synchronized {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method public void Assign(class StockWerk.StockWert) synchronized {
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method public class StockWerk.StockWert Exchange(class StockWerk.StockWert) synchronized {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() synchronized {
      ldstr "ref "
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  //
  // Tuples and Records
  //

  .class public Tuple2 extends StockWerk.StockWert {
    .field public class StockWerk.StockWert Value1
    .field public class StockWerk.StockWert Value2
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.Tuple2)
    retry:
      ldarg.1
      isinst StockWerk.Tuple2
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple2
      stloc.0
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.1
      ldstr "1"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test2
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      ret
    test2:
      ldarg.1
      ldstr "2"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s raise
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() {
      ldstr "("
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ")"
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class public Tuple3 extends StockWerk.StockWert {
    .field public class StockWerk.StockWert Value1
    .field public class StockWerk.StockWert Value2
    .field public class StockWerk.StockWert Value3
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.Tuple3)
    retry:
      ldarg.1
      isinst StockWerk.Tuple3
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple3
      stloc.0
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.1
      ldstr "1"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test2
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      ret
    test2:
      ldarg.1
      ldstr "2"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test3
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      ret
    test3:
      ldarg.1
      ldstr "3"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s raise
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() {
      ldstr "("
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ")"
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class public Tuple4 extends StockWerk.StockWert {
    .field public class StockWerk.StockWert Value1
    .field public class StockWerk.StockWert Value2
    .field public class StockWerk.StockWert Value3
    .field public class StockWerk.StockWert Value4
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.Tuple4)
    retry:
      ldarg.1
      isinst StockWerk.Tuple4
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple4
      stloc.0
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.1
      ldstr "1"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test2
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      ret
    test2:
      ldarg.1
      ldstr "2"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test3
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      ret
    test3:
      ldarg.1
      ldstr "3"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s test4
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      ret
    test4:
      ldarg.1
      ldstr "4"
      call bool System.String::Equals(class System.String, class System.String)
      bne.un.s raise
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() {
      ldstr "("
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldstr ")"
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      tail. call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class public Tuple extends StockWerk.StockWert {
    .field public class StockWerk.StockWert[] Values
    .method public void .ctor(class StockWerk.StockWert[]) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.StockWert[], class StockWerk.StockWert[], int32)
    retry:
      ldarg.1
      isinst StockWerk.Tuple
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      dup
      stloc.1
      ldlen
      ldarg.0
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      dup
      stloc.0
      ldlen
      bne.un.s retFalse
      ldc.i4.0
      stloc.2
    loop:
      ldloc.2
      ldloc.0
      ldlen
      bge.s retTrue
      ldloc.0
      ldloc.2
      ldelem.ref
      ldloc.1
      ldloc.2
      ldelem.ref
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    retTrue:
      ldc.i4.1
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      //--** convert string to int and find whether it's within bounds
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() synchronized {
      .locals(class StockWerk.StockWert[], int32, int32)
      ldarg.0
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      stloc.0
      ldloc.0
      ldlen
      stloc.1
      ldc.i4.1
      stloc.2
      ldstr "("
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldloc.0
      ldc.i4.0
      ldelem.ref
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
    loop:
      ldloc.1
      ldloc.2
      beq.s return
      ldstr ", "
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      ldloc.0
      ldloc.2
      ldelem.ref
      call instance class System.StringBuilder System.StringBuilder::Append(class System.Object)
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    return:
      ldstr ")"
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      call instance class System.String System.StringBuilder::ToString()
      ret
    }
  }

  .class sealed public Record extends StockWerk.StockWert {
    .field public class StockWerk.RecordArity Arity
    .field public class StockWerk.StockWert[] Values
    .method public void .ctor(class StockWerk.RecordArity, class StockWerk.StockWert[]) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.RecordArity StockWerk.Record::Arity
      ldarg.0
      ldarg.2
      stfld class StockWerk.StockWert[] StockWerk.Record::Values
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.StockWert[], class StockWerk.StockWert[], int32)
    retry:
      ldarg.1
      isinst StockWerk.Record
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Record
      dup
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      stloc.1
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      ldarg.0
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      bne.un.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      stloc.0
      ldc.i4.0
      stloc.2
    loop:
      ldloc.2
      ldloc.0
      ldlen
      bge.s retTrue
      ldloc.0
      ldloc.2
      ldelem.ref
      ldloc.1
      ldloc.2
      ldelem.ref
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    retTrue:
      ldc.i4.1
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.0
      ldfld class StockWerk.StockWert[] StockWerk.Record::Values
      ldarg.0
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      ldarg.1
      call instance int32 StockWerk.RecordArity::Select(class System.String)
      dup
      ldc.i4.M1
      beq.s raise
      ldelem.ref
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  //
  // Transients
  //

  .class public abstract Transient extends StockWerk.StockWert implements System.Serialization.IObjectReference {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Select(class System.String)
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ldarg.1
      tail. call instance class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(value class System.Serialization.StreamingContext)
      ret
    }
    .method virtual public class System.Object GetRealObject(value class System.Serialization.StreamingContext) {
      ldarg.0
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    }
  }

  .class sealed public Promise extends StockWerk.Transient {
    .field private class StockWerk.StockWert Ref   // null until bound
    .method public void .ctor() {
      ldarg.0
      call instance void StockWerk.Transient::.ctor()
      ldarg.0
      ldnull
      stfld class StockWerk.StockWert StockWerk.Promise::Ref
      ret
    }
    .method public class StockWerk.StockWert FutureInternal() {
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brtrue.s forward
      ldarg.0
      call instance void System.Object::Wait()
    forward:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Future()
      ret
    }
    .method public void FulfillInternal(class StockWerk.StockWert) {
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brtrue.s forward
      ldarg.0
      call instance void System.Object::Wait()
    forward:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      ldarg.1
      tail. callvirt instance void StockWerk.StockWert::Fulfill(class StockWerk.StockWert)
      ret
    }
    .method public class StockWerk.StockWert AwaitInternal() {
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brtrue.s forward
      ldarg.0
      call instance void System.Object::Wait()
    forward:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brnull.s raise
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool Equals(class System.Object) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brnull.s raise
      ldarg.1
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brnull.s testEqual
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
      ret
    testEqual:
      pop
      ldarg.0
      ldarg.1
      ceq
      ret
    }
    .method virtual public class StockWerk.StockWert Future() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brtrue.s raise
      ldarg.0
      newobj instance void StockWerk.Future::.ctor(class StockWerk.Promise)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public void Fulfill(class StockWerk.StockWert) synchronized {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brtrue.s raise
      ldarg.1
      ldarg.0
      callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
      brtrue.s raise
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Promise::Ref
      ldarg.0
      call instance void System.Object::NotifyAll()
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Fulfill
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Await() synchronized {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brnull.s raise
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() synchronized {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brtrue.s bound
      pop
      ldstr "<transient>"
      ret
    bound:
      tail. callvirt instance class System.String System.Object::ToString()
      ret
    }
  }

  .class sealed public Future extends StockWerk.Transient {
    .field private class StockWerk.Promise Prom
    .method public void .ctor(class StockWerk.Promise) {
      ldarg.0
      call instance void StockWerk.Transient::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.Promise StockWerk.Future::Prom
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      call instance class StockWerk.StockWert StockWerk.Promise::AwaitInternal()
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      call instance class StockWerk.StockWert StockWerk.Promise::AwaitInternal()
      ldarg.1
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
      ret
    }
    .method virtual public class StockWerk.StockWert Future() {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      tail. call instance class StockWerk.StockWert StockWerk.Promise::FutureInternal()
      ret
    }
    .method virtual public void Fulfill(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      ldarg.1
      tail. call instance void StockWerk.Promise::FulfillInternal(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Await() {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      tail. call instance class StockWerk.StockWert StockWerk.Promise::AwaitInternal()
      ret
    }
    .method virtual public class System.String ToString() synchronized {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      tail. callvirt instance class System.String System.Object::ToString()
      ret
    }
  }

  .class sealed public ByNeed extends StockWerk.Transient {
    .field private int32 State
    .field private class StockWerk.StockWert Ref
    // State = 0: delaying - Ref holds pointer to procedure
    //         1: failed - Ref holds pointer to exception
    //         2: bound - Ref holds pointer to value
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.Transient::.ctor()
      ldarg.0
      ldc.i4.0
      stfld int32 StockWerk.ByNeed::State
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ByNeed::Ref
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Await()
      ldarg.1
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Await()
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldarg.1
      ceq
      ret
    }
    .method virtual public class StockWerk.StockWert Future() {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Await()
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Future()
      ret
    }
    .method virtual public void Fulfill(class StockWerk.StockWert) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Await()
      ldarg.1
      tail. callvirt instance void StockWerk.StockWert::Fulfill()
      ret
    }
    .method virtual public class StockWerk.StockWert Await() {
      .locals(class StockWerk.StockWert)
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
      ldarg.0
      ldfld int32 StockWerk.ByNeed::State
      switch(apply, raise, deref)
    apply:
      .try {
	ldarg.0
	ldfld class StockWerk.StockWert StockWerk.ByNeed::Ref
	ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
	callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
	stloc.0
	ldloc.0
	ldarg.0
	callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
	brtrue.s raiseFulfill
	ldarg.0
	ldc.i4.2
	stfld int32 StockWerk.ByNeed::State
	ldarg.0
	ldloc.0
	stfld class StockWerk.StockWert StockWerk.ByNeed::Ref
	leave.s return
      raiseFulfill:
	ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$Fulfill
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      } catch StockWerk.ExceptionWrapper {
	ldfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
	stloc.0
	ldsfld class StockWerk.StockWert StockWerk.Prebound::Transient$ByNeed
	newobj instance void StockWerk.ConVal::.ctor(class StockWerk.StockWert)
	dup
	ldloc.0
	stfld class StockWerk.StockWert StockWerk.ConVal::Val
	stloc.0
	ldarg.0
	ldc.i4.1
	stfld int32 StockWerk.ByNeed::State
	ldarg.0
	ldloc.0
	stfld class StockWerk.StockWert StockWerk.ByNeed::Ref
	ldarg.0
	call void System.CriticalSection::Exit(class System.Object)
	ldloc.0
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldloc.0
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    raise:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ByNeed::Ref
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    deref:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ByNeed::Ref
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    }
    .method virtual public class System.String ToString() synchronized {
      ldarg.0
      ldfld int32 StockWerk.ByNeed::State
      switch(unbound, failed, bound)
    unbound:
      ldstr "<byNeed>"
      ret
    failed:
      ldstr "<byNeed:failed>"
      ret
    bound:
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ByNeed::Ref
      tail. callvirt instance class System.String System.Object::ToString()
      ret
    }
  }

  //
  // Procedures
  //

  .class public sealed Selector extends StockWerk.StockWert {
    .field private class System.String Label
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.Selector::Label
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      ldarg.0
      ldfld class System.String StockWerk.Selector::Label
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Select(class System.String)
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Selector
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Selector
      ldfld class System.String StockWerk.Selector::Label
      ldarg.0
      ldfld class System.String StockWerk.Selector::Label
      tail. call bool System.String::Equals(class System.String, class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class public abstract Procedure extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Global::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      ldarg.0
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.Tuple2::.ctor()
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      ldarg.2
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.Tuple3::.ctor()
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      dup
      ldarg.2
      stfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      dup
      ldarg.3
      stfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.Tuple4::.ctor()
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      dup
      ldarg.2
      stfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      dup
      ldarg.3
      stfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      dup
      ldarg.s 4
      stfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
  }

  .class public abstract Procedure0 extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      bne.un.s testTransient
      ldarg.0
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply0()
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class public abstract Procedure2 extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class StockWerk.Tuple2)
    retry:
      ldarg.1
      isinst StockWerk.Tuple2
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple2
      stloc.0
      ldarg.0
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply2(class StockWerk.StockWert, class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class public abstract Procedure3 extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class StockWerk.Tuple3)
    retry:
      ldarg.1
      isinst StockWerk.Tuple3
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple3
      stloc.0
      ldarg.0
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple3::Value3
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class public abstract Procedure4 extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class StockWerk.Tuple4)
    retry:
      ldarg.1
      isinst StockWerk.Tuple4
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple4
      stloc.0
      ldarg.0
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value1
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value3
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple4::Value4
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  //
  // Thread
  //

  .class sealed ThreadStart extends System.Object {
    .field private class StockWerk.StockWert Proc
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ret
    }
    .method void Body() {
      //--** catch and print exceptions?
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ldarg.0
      ldnull
      stfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      pop
      ret
    }
  }

  .class public sealed Thread extends StockWerk.StockWert {
    .field public class System.Thread Value
    .method public void .ctor() {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      call class System.Thread System.Thread::GetCurrentThread()
      stfld class System.Thread StockWerk.Thread::Value
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.Thread
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Thread
      ldfld class System.Thread StockWerk.Thread::Value
      ldarg.0
      ldfld class System.Thread StockWerk.Thread::Value
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  //
  // Arrays and Vectors
  //

  .class public sealed Array extends StockWerk.StockWert {
    .field public class StockWerk.StockWert[] Values
    .method assembly void .ctor(int32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      newarr class StockWerk.StockWert
      stfld class StockWerk.StockWert[] StockWerk.Array::Values
      ret
    }
    .method public void .ctor(int32, class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      newarr class StockWerk.StockWert
    loop:
      ldarg.1
      ldc.i4.1
      sub
      starg.s 1
      ldarg.1
      ldc.i4.M1
      ble.s return
      dup
      ldarg.1
      ldarg.2
      stelem.ref
      br.s loop
    return:
      stfld class StockWerk.StockWert[] StockWerk.Array::Values
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed public Vector extends StockWerk.StockWert {
    .field public class StockWerk.StockWert[] Values
    .method public void .ctor(class StockWerk.StockWert[]) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert[] StockWerk.Vector::Values
      ret
    }
    .method virtual public bool Equals(class System.Object) {
      .locals(class StockWerk.StockWert[], class StockWerk.StockWert[], int32)
    retry:
      ldarg.1
      isinst StockWerk.Vector
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Vector
      ldfld class StockWerk.StockWert[] StockWerk.Vector::Values
      dup
      stloc.1
      ldlen
      ldarg.0
      ldfld class StockWerk.StockWert[] StockWerk.Vector::Values
      dup
      stloc.0
      ldlen
      bne.un.s retFalse
      ldc.i4.0
      stloc.2
    loop:
      ldloc.2
      ldloc.0
      ldlen
      bge.s retTrue
      ldloc.0
      ldloc.2
      ldelem.ref
      ldloc.1
      ldloc.2
      ldelem.ref
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    retTrue:
      ldc.i4.1
      ret
    }
  }

  //
  // Foreign Object
  //

  .class sealed ForeignObject extends StockWerk.StockWert {
    .field public class System.Object Object
    .method public void .ctor(class System.Object) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.Object StockWerk.ForeignObject::Object
      ret
    }
    .method virtual public bool Equals(class System.Object) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      ldarg.0
      ldfld class System.Object StockWerk.ForeignObject::Object
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle$Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld class System.Object StockWerk.ForeignObject::Object
      tail. callvirt instance class System.String System.Object::ToString()
      ret
    }
  }
}

//
// Builtins
//

.namespace StockWerk.Builtins {
  .class sealed show extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      call void System.Console::WriteLine(class System.Object)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    }
  }

  .class sealed eq extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.1
      ldarg.2
      callvirt instance bool System.Object::Equals(class System.Object)
      brfalse.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    }
  }

  .class sealed ne extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.1
      ldarg.2
      callvirt instance bool System.Object::Equals(class System.Object)
      brtrue.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    }
  }

  .class sealed cast extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      ret
    }
  }

  .class sealed Application$exit extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      ldarg.1
      ldfld int32 StockWerk.Int::Value
      call void System.Runtime::Exit(int32)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Application$getArgs extends StockWerk.Procedure0 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure0::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      .locals(class System.String[], int32, int32)
      call class System.String[] System.Runtime::GetCommandLineArgs()
      stloc.0
      ldloc.0
      ldlen
      ldc.i4.1
      sub
      stloc.1
      ldc.i4.0
      stloc.2
      ldloc.1
      ldnull
      newobj instance void StockWerk.Array::.ctor(int32, class StockWerk.StockWert)
      dup
      ldfld class StockWerk.StockWert[] StockWerk.Array::Values
    loop:
      ldloc.1
      ldloc.2
      beq.s return
      dup
      ldloc.2
      ldloc.0
      ldloc.2
      ldc.i4.1
      add
      ldelem.ref
      newobj instance void StockWerk.String::.ctor(class System.String)
      stelem.ref
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    return:
      pop
      ret
    }
  }

  .class sealed Array$array extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      newobj instance void StockWerk.Array::.ctor(int32, class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Array$fromList extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(int32, int32, class StockWerk.StockWert)
      ldarga.s 1
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.0
      ldloc.0
      newobj instance void StockWerk.Array::.ctor(int32)
      dup
      ldfld class StockWerk.StockWert[] StockWerk.Array::Values
      ldc.i4.0
      stloc.1
      ldarg.1
      stloc.2
    loop:
      ldloc.0
      ldloc.1
      beq.s done
      dup
      ldloc.1
      ldloc.2
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.2
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      stelem.ref
      ldloc.1
      ldc.i4.1
      add
      stloc.1
      br.s loop
    done:
      pop
      ret
    }
  }

  .class sealed Array$length extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Array
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Array
      ldfld class StockWerk.StockWert[] StockWerk.Array::Values
      ldlen
      newobj instance void StockWerk.Int::.ctor(int32)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Array$sub extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Array
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Array
	ldfld class StockWerk.StockWert[] StockWerk.Array::Values
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldelem.ref
	leave.s return
      } catch System.IndexOutOfRangeException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Array$update extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Array
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Array
	ldfld class StockWerk.StockWert[] StockWerk.Array::Values
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldarg.3
	stelem.ref
	leave.s return
      } catch System.IndexOutOfRangeException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$beginCatchBlock extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.ForeignObject
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      callvirt instance void System.Reflection.Emit.ILGenerator::BeginCatchBlock(class System.Type)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$beginExceptionBlock extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      callvirt instance value class System.Reflection.Emit.Label System.Reflection.Emit.ILGenerator::BeginExceptionBlock()
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$constructorDeclareLocal extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.ForeignObject
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ConstructorBuilder
      ldarg.2
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance value class System.Reflection.Emit.LocalToken System.Reflection.Emit.ConstructorBuilder::DeclareLocal(class System.Type)
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$constructorGetILGenerator extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ConstructorBuilder
      call instance class System.Reflection.Emit.ILGenerator System.Reflection.Emit.ConstructorBuilder::GetILGenerator()
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$createLabel extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      callvirt instance value class System.Reflection.Emit.Label System.Reflection.Emit.ILGenerator::CreateLabel()
      box System.Reflection.Emit.Label
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$createType extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.TypeBuilder
      call instance class System.Type System.Reflection.Emit.TypeBuilder::CreateType()
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$defineConstructor extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Reflection.DescriptorInfo, int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
      ldarga.s 2
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
      ldloc.1
      ldc.i4.1   // public
      newobj instance void System.Reflection.DescriptorInfo::.ctor(int32, value class System.Reflection.MethodAttributes)
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.2
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      isinst StockWerk.ForeignObject
      brtrue.s isForeignObject
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
    isForeignObject:
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance void System.Reflection.DescriptorInfo::SetArgument(int32, class System.Type)
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.TypeBuilder
      ldc.i4.1   // public
      ldloc.0
      call instance class System.Reflection.Emit.ConstructorBuilder System.Reflection.Emit.TypeBuilder::DefineConstructor(value class System.Reflection.MethodAttributes, class System.Reflection.DescriptorInfo)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$defineField extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.TypeBuilder
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      ldc.i4.1   // public
      call instance class System.Reflection.Emit.FieldBuilder System.Reflection.Emit.TypeBuilder::DefineField(class System.String, class System.Type, value class System.Reflection.FieldAttributes)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$defineMethod extends StockWerk.Procedure4 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure4::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Reflection.DescriptorInfo, int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse testTransient2
      ldarga.s 3
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
    retry4:
      ldarg.s 4
      isinst StockWerk.ForeignObject
      brfalse testTransient4
      ldloc.1
      ldc.i4.1   // public
      newobj instance void System.Reflection.DescriptorInfo::.ctor(int32, value class System.Reflection.MethodAttributes)
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.3
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      isinst StockWerk.ForeignObject
      brtrue.s isForeignObject
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
    isForeignObject:
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance void System.Reflection.DescriptorInfo::SetArgument(int32, class System.Type)
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldloc.0
      ldarg.s 4
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance void System.Reflection.DescriptorInfo::SetReturnType(class System.Type)
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.TypeBuilder
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldloc.0
      call instance class System.Reflection.Emit.MethodBuilder System.Reflection.Emit.TypeBuilder::DefineMethod(class System.String, class System.Reflection.DescriptorInfo)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br retry2
    testTransient4:
      ldarg.s 4
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.s 4
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 4
      br retry4
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$defineStaticMethod extends StockWerk.Procedure4 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure4::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Reflection.DescriptorInfo, int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse testTransient2
      ldarga.s 3
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
    retry4:
      ldarg.s 4
      isinst StockWerk.ForeignObject
      brfalse testTransient4
      ldloc.1
      ldc.i4.s 0x11   // public static
      newobj instance void System.Reflection.DescriptorInfo::.ctor(int32, value class System.Reflection.MethodAttributes)
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.3
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      isinst StockWerk.ForeignObject
      brtrue.s isForeignObject
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
    isForeignObject:
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance void System.Reflection.DescriptorInfo::SetArgument(int32, class System.Type)
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldloc.0
      ldarg.s 4
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance void System.Reflection.DescriptorInfo::SetReturnType(class System.Type)
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.TypeBuilder
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldloc.0
      call instance class System.Reflection.Emit.MethodBuilder System.Reflection.Emit.TypeBuilder::DefineMethod(class System.String, class System.Reflection.DescriptorInfo)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br retry2
    testTransient4:
      ldarg.s 4
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.s 4
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 4
      br retry4
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$defineType extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Type[], int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.String
      brfalse testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse testTransient3
      ldnull
      call class System.Reflection.Emit.ModuleBuilder System.Reflection.Emit.ModuleBuilder::GetDynamicModule(class System.String)
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldc.i4 0x101   // public sealed class
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      callvirt instance class System.Reflection.Emit.TypeBuilder System.Reflection.Emit.ModuleBuilder::DefineType(class System.String namespace, class System.String name, value class System.TypeAttributes attr, class System.Type parent)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$endExceptionBlock extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      callvirt instance void System.Reflection.Emit.ILGenerator::EndExceptionBlock()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emit extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitConstructor extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.ConstructorInfo
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, class System.Reflection.ConstructorInfo)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitField extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.FieldInfo
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, class System.Reflection.FieldInfo)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitFloat32 extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Real
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.Real
      ldfld float32 StockWerk.Real::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, float32)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitInt8 extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Int
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, int8)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitInt16 extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Int
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, int16)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitInt32 extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Int
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, int32)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitLabel extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      unbox System.Reflection.Emit.Label
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, value class System.Reflection.Emit.Label)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitMethod extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.MethodInfo
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, class System.Reflection.MethodInfo)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitString extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.String
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, class System.String)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitType extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.ForeignObject
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, class System.Type)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$emitUnsignedInt8 extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Int
      brfalse.s testTransient3
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      callvirt instance void System.Reflection.Emit.ILGenerator::Emit(int16, unsigned int8)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$getConstructor extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Type[], int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
      ldarga.s 2
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
      ldloc.1
      newarr class System.Type
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.2
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      isinst StockWerk.ForeignObject
      brtrue.s isForeignObject
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
    isForeignObject:
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      stelem.ref
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      ldloc.0
      call instance class System.Reflection.ConstructorInfo System.Type::GetConstructor(class System.Type[] types)
      dup
      brnull.s unknownMember
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    unknownMember:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownMember
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$getField extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call instance class System.Reflection.FieldInfo System.Type::GetField(class System.String name)
      dup
      brnull.s unknownMember
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    unknownMember:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownMember
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$getMethod extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.Type[], int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse testTransient1
      ldarga.s 3
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
      ldloc.1
      newarr class System.Type
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.3
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      isinst StockWerk.ForeignObject
      brtrue.s isForeignObject
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
    isForeignObject:
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      stelem.ref
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldloc.0
      call instance class System.Reflection.MethodInfo System.Type::GetMethod(class System.String, class System.Type[] types)
      dup
      brnull.s unknownMember
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    unknownMember:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownMember
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$markLabel extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.ForeignObject
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.ILGenerator
      ldarg.2
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      unbox System.Reflection.Emit.Label
      callvirt instance void System.Reflection.Emit.ILGenerator::MarkLabel(value class System.Reflection.Emit.Label)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$getType extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call class System.Type System.Type::GetType(class System.String)
      dup
      brnull.s unknownType
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    unknownType:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Assembler$UnknownType
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$invoke extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(value class System.Variant[], int32, int32, class StockWerk.StockWert)
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse testTransient1
      ldarga.s 2
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.1
      ldloc.1
      newarr value class System.Variant
      stloc.0
      ldc.i4.0
      stloc.2
      ldarg.2
      stloc.3
    loop:
      ldloc.1
      ldloc.2
      beq.s done
      ldloc.0
      ldloc.2
      ldloc.3
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.3
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      newobj instance void System.Variant::.ctor(class System.Object)
      stelem.ref
      ldloc.2
      ldc.i4.1
      add
      stloc.2
      br.s loop
    done:
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.MethodInfo
      ldnull
      ldloc.0
      call instance value class System.Variant System.Reflection.MethodInfo::Invoke(class System.Object, value class System.Variant[])
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$methodDeclareLocal extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.ForeignObject
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.MethodBuilder
      ldarg.2
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Type
      call instance value class System.Reflection.Emit.LocalToken System.Reflection.Emit.MethodBuilder::DeclareLocal(class System.Type)
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Assembler$methodGetILGenerator extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.Reflection.Emit.MethodBuilder
      call instance class System.Reflection.Emit.ILGenerator System.Reflection.Emit.MethodBuilder::GetILGenerator()
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$chr extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      .try {
	ldarg.1
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	call wchar System.Convert::ToChar(int32)
	newobj instance void StockWerk.Char::.ctor(wchar)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Chr
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$le extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Char
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld wchar StockWerk.Char::Value
      bgt.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$isDigit extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      call bool System.Globalization.CharacterInfo::IsDecimalDigit(wchar)
      brfalse.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$isHexDigit extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      call bool System.Globalization.CharacterInfo::IsHexDigit(wchar)
      brfalse.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$isSpace extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      call bool System.Globalization.CharacterInfo::IsWhiteSpace(wchar)
      brfalse.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$ord extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      newobj instance void StockWerk.Int::.ctor(int32)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Char$toCString extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(wchar)
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      stloc.0
      ldloc.0
      switch(octal,octal,octal,octal,octal,octal,octal,esc_a,esc_b,esc_t,esc_n,esc_v,esc_f)
      ldloc.0
      call bool System.Char::IsPrintable(wchar)
      brfalse.s octal
      ldloc.0
      ldc.i4.s 34   // "
      beq.s escape
      ldloc.0
      ldc.i4.s 39   // '
      beq.s escape
      ldloc.0
      ldc.i4.s 63   // ?
      beq.s escape
      ldloc.0
      ldc.i4.s 92   // \
      beq.s escape
      ldloc.0
      newobj instance void System.String::.ctor(wchar)
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    octal:
      ldstr "\\"
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldloc.0
      conv.i8
      ldc.i4.s 8
      call class System.String System.Radix::ToString(int64, int32)
      ldc.i4.1
      call instance class System.String System.String::Substring(int32)
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      call instance class System.String System.StringBuilder::ToString()
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    escape:
      ldstr "\\"
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldloc.0
      call instance class System.StringBuilder System.StringBuilder::Append(wchar)
      call instance class System.String System.StringBuilder::ToString()
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br retry
    esc_a:
      ldstr "\\a"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    esc_b:
      ldstr "\\b"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    esc_t:
      ldstr "\\t"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    esc_n:
      ldstr "\\n"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    esc_v:
      ldstr "\\v"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    esc_f:
      ldstr "\\f"
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed General$assign extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Ref
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Ref
      ldarg.2
      call instance void StockWerk.Ref::Assign(class StockWerk.StockWert)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed General$exchange extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Ref
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Ref
      ldarg.2
      call instance class StockWerk.StockWert StockWerk.Ref::Exchange(class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$compare extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(int32, int32)
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      stloc.0
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      stloc.1
      ldloc.0
      ldloc.1
      beq.s equal
      ldloc.0
      ldloc.1
      bgt.s greater
      ldsfld class StockWerk.StockWert StockWerk.Prebound::LESS
      ret
    equal:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::EQUAL
      ret
    greater:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::GREATER
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$div extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      div
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$ge extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      blt.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$gt extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ble.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$le extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      bgt.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$lt extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      bge.s retFalse
      ldsfld class StockWerk.StockWert StockWerk.Prebound::true
      ret
    retFalse:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::false
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$minus extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	sub.ovf
	newobj instance void StockWerk.Int::.ctor(int32)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$mod extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      rem
      newobj instance void StockWerk.Int::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$plus extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	add.ovf
	newobj instance void StockWerk.Int::.ctor(int32)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$times extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	mul.ovf
	newobj instance void StockWerk.Int::.ctor(int32)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$toString extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      ldarg.1
      callvirt instance class System.String System.Object::ToString()
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$uminus extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      .try {
	ldc.i4.0
	ldarg.1
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	sub.ovf
	newobj instance void StockWerk.Int::.ctor(int32)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Pickle$load extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      //--** this should take a URL instead of a path name
      .locals(class System.IO.FileStream)
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      ldfld class System.String StockWerk.String::Value
      ldc.i4.3   // System.IO.FileMode::Open
      ldc.i4.1   // System.IO.FileAccess::Read
      newobj instance void System.IO.FileStream::.ctor(class System.String, value class System.IO.FileMode, value class System.IO.FileAccess)
      dup
      stloc.0
      newobj instance void System.Serialization.PseudoMLFormatter::.ctor(class System.IO.Stream)
      callvirt instance class System.Object System.Serialization.IFormatter::Deserialize()
      ldloc.0
      callvirt instance void System.IO.Stream::Close()
      castclass StockWerk.StockWert
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Pickle$save extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      .locals(class System.IO.FileStream)
    retry:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.2
      ldfld class System.String StockWerk.String::Value
      ldc.i4.2   // System.IO.FileMode::Create
      ldc.i4.2   // System.IO.FileAccess::Write
      newobj instance void System.IO.FileStream::.ctor(class System.String, value class System.IO.FileMode, value class System.IO.FileAccess)
      dup
      stloc.0
      newobj instance void System.Serialization.PseudoMLFormatter::.ctor(class System.IO.Stream)
      ldarg.1
      callvirt instance void System.Serialization.IFormatter::Serialize(class System.Object)
      ldloc.0
      callvirt instance void System.IO.Stream::Close()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$compare extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call int32 System.String::CompareOrdinal(class System.String, class System.String)
      dup
      brzero.s equal
      ldc.i4.0
      bgt.s greater
      ldsfld class StockWerk.StockWert StockWerk.Prebound::LESS
      ret
    equal:
      pop
      ldsfld class StockWerk.StockWert StockWerk.Prebound::EQUAL
      ret
    greater:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::GREATER
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$conc extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      newobj instance void System.StringBuilder::.ctor(class System.String)
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call instance class System.StringBuilder System.StringBuilder::Append(class System.String)
      call instance class System.String System.StringBuilder::ToString()
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$explode extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class System.String, int32, class StockWerk.StockWert)
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      dup
      stloc.0
      call instance int32 System.String::GetLength()
      stloc.1
      ldsfld class StockWerk.StockWert StockWerk.Prebound::nil
      stloc.2
    loop:
      ldloc.1
      ldc.i4.0
      beq.s return
      ldsfld class StockWerk.StockWert StockWerk.Prebound::cons
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.StockWert)
      dup
      newobj instance void StockWerk.Tuple2::.ctor()
      dup
      ldloc.0
      ldloc.1
      ldc.i4.1
      sub
      dup
      stloc.1
      call instance wchar System.String::GetChars(int32)
      newobj instance void StockWerk.Char::.ctor(wchar)
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      dup
      ldloc.2
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      stloc.2
      br.s loop
    return:
      ldloc.2
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$size extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call instance int32 System.String::GetLength()
      newobj instance void StockWerk.Int::.ctor(int32)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$str extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Char
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      call class System.String System.Char::ToString(wchar)
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$sub extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.String
	ldfld class System.String StockWerk.String::Value
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	call instance wchar System.String::GetChars(int32)
	newobj instance void StockWerk.Char::.ctor(wchar)
	leave.s return
      } catch System.ArgumentOutOfRangeException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed String$substring extends StockWerk.Procedure3 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure3::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
    retry3:
      ldarg.3
      isinst StockWerk.Int
      brfalse.s testTransient3
      newobj instance void StockWerk.String::.ctor(class System.String)
      .try {
	ldarg.1
	castclass StockWerk.String
	ldfld class System.String StockWerk.String::Value
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldarg.3
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	call instance class System.String System.String::Substring(int32, int32)
	leave.s return
      } catch System.ArgumentOutOfRangeException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$closeIn extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.IO.Stream
      callvirt instance void System.IO.Stream::Close()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$closeOut extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.IO.Stream
      callvirt instance void System.IO.Stream::Close()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$inputAll extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class System.IO.Stream, class System.StringBuilder)
    retry:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.IO.Stream
      stloc.0
      .try {
	newobj instance void System.StringBuilder::.ctor()
	stloc.1
      loop:
	ldloc.1
	ldloc.0
	callvirt instance wchar System.IO.Stream::ReadChar()
	call instance class System.StringBuilder System.StringBuilder::Append(wchar)
	pop
	br.s loop
      } catch System.IO.EOFException {
	leave.s return
      }
    return:
      ldloc.1
      callvirt instance class System.String System.StringBuilder::ToString()
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$openIn extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldc.i4.3   // Open
      ldc.i4.1   // Read
      newobj instance void System.IO.FileStream::.ctor(class System.String, value class System.IO.FileMode, value class System.IO.FileAccess)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$openOut extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldc.i4.2   // Create
      ldc.i4.2   // Write
      newobj instance void System.IO.FileStream::.ctor(class System.String, value class System.IO.FileMode, value class System.IO.FileAccess)
      newobj instance void StockWerk.ForeignObject::.ctor(class System.Object)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$output extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.String
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.IO.Stream
      ldarg.2
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      callvirt instance void System.IO.Stream::WriteStringChars(class System.String)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$output1 extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.ForeignObject
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Char
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.ForeignObject
      ldfld class System.Object StockWerk.ForeignObject::Object
      castclass System.IO.Stream
      ldarg.2
      castclass StockWerk.Char
      ldfld wchar StockWerk.Char::Value
      callvirt instance void System.IO.Stream::WriteChar(wchar)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed TextIO$print extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.String
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      call void System.Console::Write(class System.String)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Thread$current extends StockWerk.Procedure0 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure0::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      newobj instance void StockWerk.Thread::.ctor()
      ret
    }
  }

  .class sealed Thread$resume extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Thread
      brfalse.s testTransient
      ldarg.1
      ldfld class System.Thread StockWerk.Thread::Value
      call instance void System.Thread::Resume()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Thread$sleep extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Int
      brfalse.s testTransient
      ldarg.1
      ldfld int32 StockWerk.Int::Value
      call void System.Thread::Sleep(int32)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Thread$spawn extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      newobj instance void StockWerk.ThreadStart::.ctor(class StockWerk.StockWert)
      ldftn instance void StockWerk.ThreadStart::Body()
      newobj instance void System.ThreadStart::.ctor(class System.Object, int32)
      newobj instance void System.Thread::.ctor(class System.ThreadStart)
      call instance void System.Thread::Start()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    }
  }

  .class sealed Thread$suspend extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Thread
      brfalse.s testTransient
      ldarg.1
      ldfld class System.Thread StockWerk.Thread::Value
      call instance void System.Thread::Suspend()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Transient$await extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    }
  }

  .class sealed Transient$byNeed extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      newobj instance void StockWerk.ByNeed::.ctor(class StockWerk.StockWert)
      ret
    }
  }

  .class sealed Transient$fulfill extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.1
      ldarg.2
      callvirt instance void StockWerk.StockWert::Fulfill(class StockWerk.StockWert)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    }
  }

  .class sealed Transient$future extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Future()
      ret
    }
  }

  .class sealed Transient$promise extends StockWerk.Procedure0 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure0::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      newobj instance void StockWerk.Promise::.ctor()
      ret
    }
  }

  .class sealed Vector$fromList extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(int32, int32, class StockWerk.StockWert)
      ldarga.s 1
      call int32 StockWerk.Util::TestList(class StockWerk.StockWert&)
      stloc.0
      ldloc.0
      newobj instance void StockWerk.Vector::.ctor(int32)
      dup
      ldfld class StockWerk.StockWert[] StockWerk.Vector::Values
      ldc.i4.0
      stloc.1
      ldarg.1
      stloc.2
    loop:
      ldloc.0
      ldloc.1
      beq.s done
      dup
      ldloc.1
      ldloc.2
      castclass StockWerk.ConVal
      ldfld class StockWerk.StockWert StockWerk.ConVal::Val
      castclass StockWerk.Tuple2
      dup
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      stloc.2
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      stelem.ref
      ldloc.1
      ldc.i4.1
      add
      stloc.1
      br.s loop
    done:
      pop
      ret
    }
  }

  .class sealed Vector$sub extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Vector
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      .try {
	ldarg.1
	castclass StockWerk.Vector
	ldfld class StockWerk.StockWert[] StockWerk.Vector::Values
	ldarg.2
	castclass StockWerk.Int
	ldfld int32 StockWerk.Int::Value
	ldelem.ref
	leave.s return
      } catch System.IndexOutOfRangeException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$andb extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      and
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$asr extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      shr
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$fromInt2 extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry2:
      ldarg.2
      isinst StockWerk.Int
      brfalse.s testTransient2
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$lsr extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      shr.un
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$minus extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      sub
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$mod extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      rem
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$notb extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      not
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$orb extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      or
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$plus extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      add
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$shl extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      shl
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$times extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      mul
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$toInt extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient
      .try {
	ldarg.1
	castclass StockWerk.Word
	ldfld int32 StockWerk.Word::Value
	call unsigned int32 System.Convert::ToUInt32(int32)
	newobj instance void StockWerk.Int::.ctor(int32)
	leave.s return
      } catch System.OverflowException {
	ldsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      }
    return:
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$toIntX extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      newobj instance void StockWerk.Int::.ctor(int32)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$toString extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldc.i4.s 16
      call class System.String System.Radix::ToString(int64, int32)
      ldc.i4.2
      call instance class System.String System.String::Substring(int32)
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Word$xorb extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry1:
      ldarg.1
      isinst StockWerk.Word
      brfalse.s testTransient1
    retry2:
      ldarg.2
      isinst StockWerk.Word
      brfalse.s testTransient2
      ldarg.1
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      ldarg.2
      castclass StockWerk.Word
      ldfld int32 StockWerk.Word::Value
      xor
      newobj instance void StockWerk.Word::.ctor(int32)
      ret
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::General$TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }
}

.namespace StockWerk.Builtins.Internal {
  .class public sealed Component$load extends StockWerk.Procedure0 {
    .field private class StockWerk.Komponist Komponist
    .field private class System.String Whence
    .method public void .ctor(class StockWerk.Komponist, class System.String) {
      ldarg.0
      call instance void StockWerk.Procedure0::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.Komponist StockWerk.Builtins.Internal.Component$load::Komponist
      ldarg.0
      ldarg.2
      stfld class System.String StockWerk.Builtins.Internal.Component$load::Whence
      ret
    }
    .method public virtual class StockWerk.StockWert Apply0() {
      .locals(value class System.Variant)
      ldarg.0
      ldfld class System.String StockWerk.Builtins.Internal.Component$load::Whence
      call class System.Reflection.Module System.Reflection.Module::GetModule(class System.String)
      dup
      brfalse.s raise
      ldstr "Main"
      callvirt instance class System.Type System.Reflection.Module::GetType(class System.String)
      dup
      brfalse.s raise
      ldstr "main"
      callvirt instance class System.Reflection.MethodInfo System.Type::GetMethod(class System.String)
      dup
      brfalse.s raise
      ldnull
      ldc.i4.1
      newarr value class System.Variant
      dup
      ldc.i4.0
      ldelema System.Variant
      dup
      initobj System.Variant
      ldarg.0
      ldfld class StockWerk.Komponist StockWerk.Builtins.Internal.Component$load::Komponist
      call instance void System.Variant::.ctor(class System.Object)
      callvirt instance value class System.Variant System.Reflection.MethodInfo::Invoke(class System.Object, value class System.Variant[])
      stloc.0
      ldarga.s 0
      call instance class System.Object System.Variant::ToObject()
      castclass StockWerk.StockWert
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Component$Component
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }
}

.method public static void main() {
  .entrypoint
  newobj instance void StockWerk.Komponist::.ctor()
  call class System.String[] System.Runtime::GetCommandLineArgs()
  ldc.i4.1
  ldelem.ref
  call instance class StockWerk.StockWert StockWerk.Komponist::Import(class System.String)
  callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
  pop
  ret
}
