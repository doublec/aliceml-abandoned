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
// -- Name and Constructor extend Global
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

//--** use brinst and brnull instead of brtrue and brfalse

.namespace StockWerk {
  .class sealed public Prebound extends System.Object {
    // Names, Constructors, and Exceptions
    .field static public initonly class StockWerk.StockWert false
    .field static public initonly class StockWerk.StockWert true
    .field static public initonly class StockWerk.StockWert nil
    .field static public initonly class StockWerk.Constructor cons
    .field static public initonly class StockWerk.StockWert ref
    .field static public initonly class StockWerk.StockWert Match
    .field static public initonly class StockWerk.StockWert Bind
    .field static public initonly class StockWerk.StockWert unit
    .field static public initonly class StockWerk.StockWert TypeError
    .field static public initonly class StockWerk.StockWert Promise
    .field static public initonly class StockWerk.StockWert Future
    .field static public initonly class StockWerk.StockWert Fulfill
    .field static public initonly class StockWerk.Constructor ByNeed
    .field static public initonly class StockWerk.StockWert Pickle
    .field static public initonly class StockWerk.StockWert Component
    .field static public initonly class StockWerk.StockWert General$Chr
    .field static public initonly class StockWerk.StockWert General$Div
    .field static public initonly class StockWerk.StockWert General$Domain
    .field static public initonly class StockWerk.StockWert General$Fail
    .field static public initonly class StockWerk.StockWert General$Overflow
    .field static public initonly class StockWerk.StockWert General$Size
    .field static public initonly class StockWerk.StockWert General$Span
    .field static public initonly class StockWerk.StockWert General$Subscript
    .field static public initonly class StockWerk.StockWert List$Empty
    .field static public initonly class StockWerk.StockWert Option$Option
    // Builtins
    .field static public initonly class StockWerk.StockWert show
    .field static public initonly class StockWerk.StockWert eq
    .field static public initonly class StockWerk.StockWert ne
    .field static public initonly class StockWerk.StockWert Application$exit
    .field static public initonly class StockWerk.StockWert Array$array
    .field static public initonly class StockWerk.StockWert Array$fromList //--**
    .field static public initonly class StockWerk.StockWert Array$length
    .field static public initonly class StockWerk.StockWert Array$sub
    .field static public initonly class StockWerk.StockWert Array$update
    .field static public initonly class StockWerk.StockWert Char$chr
    .field static public initonly class StockWerk.StockWert Char$isDigit
    .field static public initonly class StockWerk.StockWert Char$isHexDigit
    .field static public initonly class StockWerk.StockWert Char$isSpace
    .field static public initonly class StockWerk.StockWert Char$ord
    .field static public initonly class StockWerk.StockWert Char$toCString //--**
    .field static public initonly class StockWerk.StockWert General$assign
    .field static public initonly class StockWerk.StockWert General$exchange
    .field static public initonly class StockWerk.StockWert Int$compare //--**
    .field static public initonly class StockWerk.StockWert Int$div
    .field static public initonly class StockWerk.StockWert Int$ge
    .field static public initonly class StockWerk.StockWert Int$gt
    .field static public initonly class StockWerk.StockWert Int$le
    .field static public initonly class StockWerk.StockWert Int$less
    .field static public initonly class StockWerk.StockWert Int$minus
    .field static public initonly class StockWerk.StockWert Int$mod
    .field static public initonly class StockWerk.StockWert Int$plus
    .field static public initonly class StockWerk.StockWert Int$times
    .field static public initonly class StockWerk.StockWert Int$toString
    .field static public initonly class StockWerk.StockWert Int$uminus
    .field static public initonly class StockWerk.StockWert Pickle$load
    .field static public initonly class StockWerk.StockWert Pickle$save
    .field static public initonly class StockWerk.StockWert String$compare //--**
    .field static public initonly class StockWerk.StockWert String$conc
    .field static public initonly class StockWerk.StockWert String$explode
    .field static public initonly class StockWerk.StockWert String$size
    .field static public initonly class StockWerk.StockWert String$str
    .field static public initonly class StockWerk.StockWert String$sub
    .field static public initonly class StockWerk.StockWert String$substring
    .field static public initonly class StockWerk.StockWert String$toCString //--**
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
    .field static public initonly class StockWerk.StockWert Vector$fromList //--**
    .field static public initonly class StockWerk.StockWert Vector$sub //--**
    .field static public initonly class StockWerk.StockWert Word$andb //--**
    .field static public initonly class StockWerk.StockWert Word$asr //--**
    .field static public initonly class StockWerk.StockWert Word$fromInt2 //--**
    .field static public initonly class StockWerk.StockWert Word$lsr //--**
    .field static public initonly class StockWerk.StockWert Word$minus //--**
    .field static public initonly class StockWerk.StockWert Word$mod //--**
    .field static public initonly class StockWerk.StockWert Word$notb //--**
    .field static public initonly class StockWerk.StockWert Word$orb //--**
    .field static public initonly class StockWerk.StockWert Word$plus //--**
    .field static public initonly class StockWerk.StockWert Word$shl //--**
    .field static public initonly class StockWerk.StockWert Word$shr //--**
    .field static public initonly class StockWerk.StockWert Word$times //--**
    .field static public initonly class StockWerk.StockWert Word$toInt //--**
    .field static public initonly class StockWerk.StockWert Word$toIntX //--**
    .field static public initonly class StockWerk.StockWert Word$toString //--**
    .field static public initonly class StockWerk.StockWert Word$xorb //--**
    .method public static void .cctor() {
      // Names, Constructors, and Exceptions
      newobj instance void StockWerk.RefConstructor::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::ref
      ldstr "false"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::false
      ldstr "true"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::true
      ldstr "nil"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::nil
      ldstr "::"
      newobj instance void StockWerk.NamedConstructor::.ctor(class System.String)
      stsfld class StockWerk.Constructor StockWerk.Prebound::cons
      ldstr "Match"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Match
      ldstr "Bind"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Bind
      ldstr "()"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ldstr "TypeError"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      ldstr "Promise"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      ldstr "Future"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Future
      ldstr "Fulfill"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
      ldstr "ByNeed"
      newobj instance void StockWerk.NamedConstructor::.ctor(class System.String)
      stsfld class StockWerk.Constructor StockWerk.Prebound::ByNeed
      ldstr "Pickle"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      ldstr "Component"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Component
      ldstr "General.Char"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Chr
      ldstr "General.Div"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Div
      ldstr "General.Domain"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Domain
      ldstr "General.Fail"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Fail
      ldstr "General.Overflow"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Overflow
      ldstr "General.Size"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Size
      ldstr "General.Span"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Span
      ldstr "General.Subscript"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$Subscript
      ldstr "List.Empty"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::List$Empty
      ldstr "Option.Option"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Option$Option
      // Builtins
      newobj instance void StockWerk.Builtins.show::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::show
      newobj instance void StockWerk.Builtins.eq::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::eq
      newobj instance void StockWerk.Builtins.ne::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::ne
      newobj instance void StockWerk.Builtins.Application$exit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Application$exit
      newobj instance void StockWerk.Builtins.Array$array::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$array
      newobj instance void StockWerk.Builtins.Array$length::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$length
      newobj instance void StockWerk.Builtins.Array$sub::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$sub
      newobj instance void StockWerk.Builtins.Array$update::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Array$update
      newobj instance void StockWerk.Builtins.Char$isDigit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isDigit
      newobj instance void StockWerk.Builtins.Char$isHexDigit::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isHexDigit
      newobj instance void StockWerk.Builtins.Char$isSpace::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$isSpace
      newobj instance void StockWerk.Builtins.Char$chr::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$chr
      newobj instance void StockWerk.Builtins.Char$ord::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Char$ord
      newobj instance void StockWerk.Builtins.General$assign::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$assign
      newobj instance void StockWerk.Builtins.General$exchange::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::General$exchange
      newobj instance void StockWerk.Builtins.Int$div::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$div
      newobj instance void StockWerk.Builtins.Int$ge::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$ge
      newobj instance void StockWerk.Builtins.Int$gt::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$gt
      newobj instance void StockWerk.Builtins.Int$le::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$le
      newobj instance void StockWerk.Builtins.Int$less::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::Int$less
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
      ret
    }
  }

  .class sealed public ExceptionWrapper extends System.RuntimeException {
    .field public class StockWerk.StockWert Value
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      callvirt instance class System.String System.Object::ToString()
      call instance void System.RuntimeException::.ctor(class System.String)
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
      call instance void System.RuntimeException::.ctor(class System.String)
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
      call class System.Object System.Collections.Hashtable::GetItem(class System.Object)
      dup
      brfalse.s load
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
      brfalse.s enter
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply0() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Future() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public void Fulfill(class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public class StockWerk.StockWert Await() {
      ldarg.0
      ret
    }
    .method virtual public class System.Reflection.MemberInfo[] GetMembers(value class System.Serialization.StreamingContext) {
      ldarg.0
      call instance class System.Type System.Object::GetType()
      ldc.i4.1   // also return private members
      tail. callvirt instance class System.Reflection.MemberInfo[] System.Type::GetMembers(bool)
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      .locals(value class System.Serialization.SerializationInfo)
      ldloca.s 0
      initobj System.Serialization.SerializationInfo
      ldloca.s 0
      ldarg.0
      call instance class System.Type System.Object::GetType()
      dup
      ldarg.1
      call class System.Reflection.MemberInfo[] System.Serialization.SerializationInfoGenerator::GetSerializableMembers(class System.Type, value class System.Serialization.StreamingContext)
      ldarg.0
      call instance void System.Serialization.SerializationInfo::.ctor(class System.Type, class System.Reflection.MemberInfo[], class System.Object)
      ldarg.0
      ldloca.s 0
      ldobj System.Serialization.SerializationInfo
      call void System.Serialization.SerializationInfoGenerator::PopulateSerializationInfo(class System.Object, value class System.Serialization.SerializationInfo)
      ldloc.0
      ret
    }
    .method virtual public void PopulateData(class System.Reflection.MemberInfo[], value class System.Variant[]) {
      .locals(int32)
      ldc.i4.0
      stloc.0
    loop:
      ldloc.0
      ldarg.1
      ldlen
      bge.s return
      ldarg.1
      ldloc.0
      ldelem.ref
      castclass System.Reflection.FieldInfo
      ldarg.0
      ldarg.2
      ldloc.0
      ldelema System.Variant
      ldobj System.Variant
      call instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
      ldloc.0
      ldc.i4.1
      add
      stloc.0
      br.s loop
    return:
      ret
    }
    .method virtual public void FixupMember(class System.Reflection.MemberInfo, class System.Object) {
      .locals(value class System.Variant)
      ldarg.1
      castclass System.Reflection.FieldInfo
      ldarg.0
      ldloca.s 0
      initobj System.Variant
      ldloca.s 0
      ldarg.2
      call instance void System.Variant::.ctor(class System.Object)
      ldloca.s 0
      ldobj System.Variant
      tail. call instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
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
      ldloca.s 0
      initobj System.Guid
      ldloca.s 0
      call instance void System.Guid::.ctor()
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
      brfalse.s enter
      castclass System.WeakReference
      call instance class System.Object System.WeakReference::GetTarget()
      dup
      brfalse.s enter
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
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
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
      //--** putting a tail. instruction here breaks things (NullReferenceException in super)
      call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(value class System.Serialization.StreamingContext)
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
      //--** throw exception Chr if out of range
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
      beq.s appendFrac
      dup
      ldc.i4.s 101   // 'e'
      callvirt instance int32 System.String::IndexOf(wchar)
      ldc.i4.M1
      bne.un.s return
    appendFrac:
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

  .class public Name extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Global::.ctor()
      ret
    }
    .method virtual public class System.String ToString() {
      ldstr "<N>"
      ret
    }
  }

  .class sealed public NamedName extends StockWerk.Name {
    .field public class System.String Name
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.Name::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.NamedName::Name
      ret
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld class System.String StockWerk.NamedName::Name
      ret
    }
  }

  .class sealed public UniqueName extends StockWerk.NamedName {
    .method public void .ctor(class System.String) {
      ldarg.0
      ldarg.1
      call instance void StockWerk.NamedName::.ctor(class System.String)
      ldarg.0
      ldarg.1
      tail. call void StockWerk.Global::Enter(class System.Object, class StockWerk.StockWert)
      ret
    }
  }

  .class public Constructor extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Global::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.Constructor)
      dup
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      ret
    }
    .method virtual public class System.String ToString() {
      ldstr "<C>"
      ret
    }
  }

  .class sealed public NamedConstructor extends StockWerk.Constructor {
    .field public class System.String Name
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.Constructor::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.NamedConstructor::Name
      ret
    }
    .method virtual public class System.String ToString() {
      ldarg.0
      ldfld class System.String StockWerk.NamedName::Name
      ret
    }
  }

  .class sealed public UniqueConstructor extends StockWerk.NamedConstructor {
    .method public void .ctor(class System.String) {
      ldarg.0
      ldarg.1
      call instance void StockWerk.NamedConstructor::.ctor(class System.String)
      ldarg.0
      ldarg.1
      tail. call void StockWerk.Global::Enter(class System.Object, class StockWerk.StockWert)
      ret
    }
  }

  .class sealed public ConVal extends StockWerk.StockWert {
    .field public class StockWerk.Constructor Con
    .field public class StockWerk.StockWert Val
    .method public void .ctor(class StockWerk.Constructor) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.Constructor StockWerk.ConVal::Con
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
      ldfld class StockWerk.Constructor StockWerk.ConVal::Con
      ldarg.0
      ldfld class StockWerk.Constructor StockWerk.ConVal::Con
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
      ldfld class StockWerk.Constructor StockWerk.ConVal::Con
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
    .method synchronized public class StockWerk.StockWert Access() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method synchronized public void Assign(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method synchronized public class StockWerk.StockWert Exchange(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method synchronized virtual public class System.String ToString() {
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Match
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Match
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Match
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
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ldarg.1
      tail. call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(value class System.Serialization.StreamingContext)
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
      brfalse.s raise
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool Equals(class System.Object) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brfalse.s raise
      ldarg.1
      tail. callvirt instance bool System.Object::Equals(class System.Object)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brfalse.s testEqual
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method synchronized virtual public void Fulfill(class StockWerk.StockWert) {
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method synchronized virtual public class StockWerk.StockWert Await() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brfalse.s raise
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method synchronized virtual public class System.String ToString() {
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
    .method synchronized virtual public class System.String ToString() {
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
	ldsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
	newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
	throw
      } catch StockWerk.ExceptionWrapper {
	ldfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
	stloc.0
	ldsfld class StockWerk.Constructor StockWerk.Prebound::ByNeed
	newobj instance void StockWerk.ConVal::.ctor(class StockWerk.Constructor)
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
    .method synchronized virtual public class System.String ToString() {
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  //
  // Arrays and Vectors
  //

  .class public sealed Array extends StockWerk.StockWert {
    .field public class StockWerk.StockWert[] Values
    .method public void .ctor(int32, class StockWerk.StockWert) {
      .locals(class StockWerk.Stockwert[])
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.1
      newarr class StockWerk.StockWert
      stloc.0
    loop:
      ldarg.1
      ldc.i4.1
      sub
      starg.s 1
      ldarg.1
      ldc.i4.M1
      ble.s return
      ldloc.0
      ldarg.1
      ldarg.2
      stelem.ref
      br.s loop
    return:
      ldarg.0
      ldloc.0
      stfld class StockWerk.StockWert[] StockWerk.Array::Values
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
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
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Array$length extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      /*tail.*/ call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply1(class StockWerk.StockWert) {
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldarg.1
      castclass StockWerk.Array
      ldfld class StockWerk.StockWert[] StockWerk.Array::Values
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldelem.ref
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldarg.1
      castclass StockWerk.Array
      ldfld class StockWerk.StockWert[] StockWerk.Array::Values
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.3
      stelem.ref
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      newobj instance void StockWerk.Char::.ctor(wchar)
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed Int$less extends StockWerk.Procedure2 {
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      rem   //--** rem.un?
      ret
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldnull
      callvirt instance class System.Object System.Serialization.Formatter::Deserialize(class System.Serialization.HeaderHandler)
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldnull
      callvirt instance void System.Serialization.Formatter::Serialize(class System.Object, value class System.Serialization.Header[])
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.Constructor StockWerk.Prebound::cons
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.Constructor)
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldarg.1
      castclass StockWerk.String
      ldfld class System.String StockWerk.String::Value
      ldarg.2
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      call instance wchar System.String::GetChars(int32)
      newobj instance void StockWerk.Char::.ctor(wchar)
      ret
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      newobj instance void StockWerk.String::.ctor(class System.String)
      ret
    testTransient3:
      ldarg.3
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.3
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 3
      br.s retry3
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    testTransient1:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      brfalse.s raise1
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise1:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise2
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise2:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      brfalse.s raise1
      ldarg.1
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry1
    raise1:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    testTransient2:
      ldarg.2
      isinst StockWerk.Transient
      brfalse.s raise2
      ldarg.2
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 2
      br.s retry2
    raise2:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
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
//--** castclass StockWerk.StockWert //--** does not work
pop ldsfld class StockWerk.StockWert StockWerk.Prebound::unit
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Component
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
