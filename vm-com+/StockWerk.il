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
// -- replicable data structures (literals, tuples, records, names,
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

//--** use brinst and brnull instead of brtrue and brfalse

.namespace StockWerk {
  .class sealed public Prebound extends System.Object {
    .field static public initonly class StockWerk.StockWert Unit
    .field static public initonly class StockWerk.StockWert TypeError
    .field static public initonly class StockWerk.StockWert Promise
    .field static public initonly class StockWerk.StockWert Future
    .field static public initonly class StockWerk.StockWert Fulfill
    .field static public initonly class StockWerk.StockWert ByNeed
    .field static public initonly class StockWerk.StockWert Pickle
    .method public static void .cctor() {
      ldstr "()"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Unit
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
      stsfld class StockWerk.StockWert StockWerk.Prebound::ByNeed
      ldstr "Pickle"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      ret
    }
  }

  .class sealed public RecordArity extends System.Object {
    .field public static class System.Collections.Hashtable ArityTable
    .field public class System.String[] Labels
    .method public static void .cctor() {
      newobj instance void System.Collections.Hashtable::.ctor()
      stsfld class System.Collections.Hashtable StockWerk.RecordArity::ArityTable
      ret
    }
    .method public void .ctor(class System.String[]) {
      ldarg.0
      call instance void System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String[] StockWerk.RecordArity::Labels
      ret
    }
    .method virtual public int32 GetHashCode() {
      .locals(int32, int32)
      ldc.i4 0
      stloc.0
      ldc.i4 0
      stloc.1
    loop:
      ldloc.0
      ldarg.1
      ldlen
      bge.s return
      ldarg.1
      ldloc.0
      ldelem.ref
      callvirt instance int32 System.Object::GetHashCode()
      ldloc.1
      xor
      stloc.1
      ldloc.0
      ldc.i4 1
      add
      stloc.0
      br.s loop
    return:
      ldloc.1
      ret
    }
  }

  .class sealed public ExceptionWrapper extends System.RuntimeException {
    .field public initonly class StockWerk.StockWert Value
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      tail. call instance void System.RuntimeException::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
      ret
    }
  }

  //
  // Value Representation
  //

  .class abstract public StockWert extends System.Object implements System.Serialization.ISerializable {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void System.Object::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
    retry:
      ldarg.0
      ldarg.1
      bne.un.s testTransient
      ldc.i4 1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4 0
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldc.i4 0
      ret
    }
    .method virtual public class System.Reflection.MemberInfo[0 ...] GetMembers(value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class System.Type System.Object::GetType()
      ldc.i4 1
      tail. callvirt instance class System.Reflection.MemberInfo[0 ...] System.Type::GetMembers(bool)
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      .locals(value class System.Serialization.SerializationInfo)
      ldloca.s 0
      ldarg.0
      callvirt instance class System.Type System.Object::GetType()
      ldarg.1
      call class System.Serialization.SerializationTypeData System.Serialization.SerializationInfoGenerator::GetSerializationTypeData(class System.Type, int32)
      ldarg.0
      call instance void System.Serialization.SerializationInfo::.ctor(class System.Serialization.SerializationTypeData, class System.Object)
      ldarg.0
      ldloc.0
      call void System.Serialization.SerializationInfoGenerator::PopulateSerializationInfo(class System.Object, value class System.Serialization.SerializationInfo)
      ldloc.0
      ret
    }
    .method virtual public void PopulateData(class System.Reflection.MemberInfo[0 ...], value class System.Variant[0 ...]) {
      .locals(int32)
      ldc.i4 0
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
      callvirt instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
      ldloc.0
      ldc.i4 1
      add
      stloc.0
      br.s loop
    return:
      ret
    }
    .method virtual public void FixupMember(class System.Reflection.MemberInfo, class System.Object) {
      ldarg.1
      castclass System.Reflection.FieldInfo
      ldarg.0
      ldarg.2
      call value class System.Variant System.Variant::$widen(class System.Object)
      tail. callvirt instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
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
    .method public static class System.Object Globalize(class StockWerk.StockWert) {
      .locals(value class System.Guid, class System.Object)
      ldloca.s 0
      call instance void System.Guid::.ctor()
      ldloca.s 0
      box System.Guid
      stloc.1
      ldsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ldloc.1
      ldarg.0
      newobj instance void System.WeakReference::.ctor(class System.Object)
      call instance void System.Collections.Hashtable::Add(class System.Object, class System.Object)
      ldloc.1
      ret
    }
    .method public static class StockWerk.StockWert Localize(class System.Object, class StockWerk.StockWert) {
      ldsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ldarg.0
      call instance class System.Object System.Collections.Hashtable::get$Item(class System.Object)
      dup
      brfalse.s enter
      castclass System.WeakReference
      call instance class System.Object System.WeakReference::get$Target()
      dup
      brfalse.s enter
      castclass StockWerk.StockWert
      ret
    enter:
      pop
      ldsfld class System.Collections.Hashtable StockWerk.Global::GuidTable
      ldarg.0
      ldarg.1
      newobj instance void System.WeakReference::.ctor(class System.Object)
      call instance void System.Collections.Hashtable::Add(class System.Object, class System.Object)
      ldarg.1
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldarg.0
      ldfld class System.Object StockWerk.Global::Guid
      brtrue.s serialize
      ldarg.0
      dup
      call class System.Object StockWerk.Global::Globalize(class StockWerk.StockWert)
      stfld class System.Object StockWerk.Global::Guid
    serialize:
      //--** workaround until value parameters work correctly:
      .locals(value class System.Serialization.SerializationInfo)
      ldloca.s 0
      ldarg.0
      callvirt instance class System.Type System.Object::GetType()
      ldarg.1
      call class System.Serialization.SerializationTypeData System.Serialization.SerializationInfoGenerator::GetSerializationTypeData(class System.Type, int32)
      ldarg.0
      call instance void System.Serialization.SerializationInfo::.ctor(class System.Serialization.SerializationTypeData, class System.Object)
      ldarg.0
      ldloc.0
      call void System.Serialization.SerializationInfoGenerator::PopulateSerializationInfo(class System.Object, value class System.Serialization.SerializationInfo)
      ldloc.0
      ret
/*
      ldarg.0
      ldarg.1
      ldarg.2
      tail. call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(int32, value class System.Serialization.StreamContext)
      ret
*/
    }
    .method virtual public class System.Object GetRealObject() {
      ldarg.0
      ldfld class System.Object StockWerk.Global::Guid
      ldarg.0
      tail. call class StockWerk.StockWert StockWerk.Global::Localize(class System.Object, class StockWerk.StockWert)
      ret
    }
  }

  //
  // Literals
  //

  .class sealed public Int extends StockWerk.StockWert {
    .field public initonly int32 Value
    .method public void .ctor(int32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld int32 StockWerk.Int::Value
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
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
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4 0
      ret
    }
  }

  //
  // Names, Constructors, and Constructed Values
  //

  .class public Name extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Global::.ctor()
      ret
    }
  }

  .class sealed public NamedName extends StockWerk.Name {
    .field public initonly class System.String Name
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.Name::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.NamedName::Name
      ret
    }
  }

  .class public Constructor extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Global::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.Constructor, class StockWerk.StockWert)
      ret
    }
  }

  .class sealed public NamedConstructor extends StockWerk.Constructor {
    .field public initonly class System.String Name
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.Constructor::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.NamedConstructor::Name
      ret
    }
  }

  .class sealed public ConVal extends StockWerk.StockWert {
    .field public initonly class StockWerk.Constructor Con
    .field public initonly class StockWerk.StockWert Val
    .method public void .ctor(class StockWerk.Constructor, class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.Constructor StockWerk.ConVal::Con
      ldarg.0
      ldarg.2
      stfld class StockWerk.StockWert StockWerk.ConVal::Val
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
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
      tail. callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      ret
    retFalse:
      ldc.i4 0
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    }
  }

  .class sealed public RefConstructor extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      newobj instance void StockWerk.Ref::.ctor(class StockWerk.StockWert)
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.RefConstructor
      brfalse.s testTransient
      ldc.i4 1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4 0
      ret
    }
  }

  .class sealed public Ref extends StockWerk.StockWert {
    .field private class StockWerk.StockWert Value
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method public class StockWerk.StockWert Deref() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method public void Assign(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  //
  // Tuples and Records
  //

  .class public Tuple extends StockWerk.StockWert {
    .field public initonly class StockWerk.StockWert[] Values
    .method public void .ctor(class StockWerk.StockWert[]) {
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Tuple::Values
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      .locals(class StockWerk.StockWert[], class StockWerk.StockWert[], int32)
    retry:
      ldarg.1
      isinst StockWerk.Tuple
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Tuple
      ldfld class StockWerk.Values[] StockWerk.Tuple::Values
      dup
      stloc.1
      ldlen
      ldarg.0
      ldfld class StockWerk.Values[] StockWerk.Tuple::Values
      dup
      stloc.0
      ldlen
      bne.un.s retFalse
      ldc.i4 0
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
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      brfalse.s retFalse
      ldloc.2
      ldc.i4 1
      add
      stloc.2
      br.s loop
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4 0
      ret
    retTrue:
      ldc.i4 1
      ret
    }
  }

  .class sealed public Record extends StockWerk.Tuple {
    .field public initonly class StockWerk.RecordArity Arity
    .method public void .ctor(class StockWerk.RecordArity, class StockWerk.StockWert[]) {
      ldarg.0
      ldarg.2
      call instance void StockWerk.Tuple::.ctor(class StockWerk.StockWert[])
      ldarg.0
      ldarg.1
      stfld class StockWerk.RecordArity StockWerk.Record::Arity
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      .locals(class StockWerk.StockWert[], class StockWerk.StockWert[], int32)
    retry:
      ldarg.1
      isinst StockWerk.Record
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Record
      dup
      ldfld class StockWerk.Values[] StockWerk.Tuple::Values
      stloc.1
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      ldarg.0
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      bne.un.s retFalse
      ldarg.0
      ldfld class StockWerk.Values[] StockWerk.Tuple::Values
      stloc.0
      ldc.i4 0
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
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      brfalse.s retFalse
      ldloc.2
      ldc.i4 1
      add
      stloc.2
      br.s loop
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4 0
      ret
    retTrue:
      ldc.i4 1
      ret
    }
  }

  //
  // Transients
  //

  .class public abstract Transient extends StockWerk.StockWert implements System.Serialization.IObjectReference {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method abstract virtual public class StockWerk.StockWert Request() {}
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
      pop
      ldarg.0
      ldarg.1
      ldarg.2
      tail. call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(int32, value class System.Serialization.StreamContext)
      ret
    }
    .method virtual public class System.Object GetRealObject() {
      ldarg.0
      tail. callvirt instance class StockWerk.StockWert StockWerk.Transient::Request()
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
    .method public class StockWerk.StockWert GetFuture() {
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
    .method public class StockWerk.StockWert RequestInternal() {
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      brfalse.s return
      ldarg.0
      call instance void System.Object::Wait()
    return:
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      ret
    }
    .method public void Fulfill(class StockWerk.StockWert) {
      ldarg.0
      call void System.CriticalSection::Enter(class System.Object)
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
      ldarg.0
      tail. call void System.CriticalSection::Exit(class System.Object)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Promise::Ref
      dup
      brfalse.s raise
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
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
    .method virtual public class StockWerk.StockWert Request() {
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
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      call instance class StockWerk.StockWert StockWerk.Promise::RequestInternal()
      ldarg.1
      tail. call instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      call instance class StockWerk.StockWert StockWerk.Promise::RequestInternal()
      ldarg.1
      tail. call instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Request() {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      tail. call instance class StockWerk.StockWert StockWerk.Promise::RequestInternal()
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
      ret
    }
  }

  .class sealed public Ref extends StockWerk.StockWert implements System.Serialization.IObjectReference {
    // this class is only instantiated during serialization
    .field private class StockWerk.StockWert Ref
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Ref
      ret
    }
    .method virtual public class System.Object GetRealObject() {
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Ref::Ref
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
      ldc.i4 0
      stfld int32 StockWerk.ByNeed::State
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ByNeed::Ref
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Request()
      ldarg.1
      tail. call instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Request()
      ldarg.1
      tail. call instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      ret
    }
    .method virtual public class StockWerk.StockWert Request() {
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
        ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
        callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
        ldloc.0
        stloc.0
        ldarg.0
        callvirt instance bool StockWerk.StockWert::IsVariantOf(class StockWerk.Transient)
        brtrue.s raiseFulfill
        ldarg.0
        ldc.i4 2
        stfld int32 StockWerk.ByNeed::State
        ldarg.0
        ldloc.0
        stfld class StockWerk.StockWert StockWerk.ByNeed::Ref
        ldarg.0
        call instance void System.Object::NotifyAll()
        ldarg.0
        call void System.CriticalSection::Exit(class System.Object)
        ldloc.0
        ret
      raiseFulfill:
        ldsfld class StockWerk.StockWert StockWerk.Prebound::Fulfill
        newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
        throw
      } catch StockWerk.ExceptionWrapper {
        ldfld class StockWerk.StockWert StockWerk.ExceptionWrapper::Value
        stloc.0
        ldarg.0
        ldc.i4 1
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
      ret
    }
    .method virtual public bool IsVariantOf(class StockWerk.Transient) {
      ldarg.0
      ldarg.1
      ceq
      ret
    }
  }

  //
  // Procedures
  //

  .class public abstract Procedure extends StockWerk.Global {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Global::.ctor()
      ret
    }
  }

  //
  // Thread
  //

  .class public sealed Thread extends StockWerk.StockWert {
    .field private class StockWerk.StockWert Proc
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Thread::Proc
      ldarg.0
      ldftn instance void StockWerk.Thread::Body()
      newobj instance void System.ThreadStart::.ctor(class System.Object, int32)
      newobj instance void System.Thread::.ctor(class System.ThreadStart)
      tail. call instance void System.Thread::Start()
      ret
    }
    .method private void Body() {
      //--** catch and print exceptions?
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Thread::Proc
      ldarg.0
      ldnull
      stfld class StockWerk.StockWert StockWerk.Thread::Proc
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      pop
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }
}

.namespace StockWerk.Builtins {
  .class sealed public BIPromise extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public static class StockWerk.StockWert Apply0() {
      newobj instance void StockWerk.Promise::.ctor()
      ret
    }
  }

  .class sealed public BIFuture extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public static class StockWerk.StockWert Apply1(class StockWerk.StockWert) {
    retry:
      ldarg.0
      isinst StockWerk.Promise
      brfalse.s testFuture
      ldarg.0
      castclass StockWerk.Promise
      tail. call instance class StockWerk.StockWert StockWerk.Promise::GetFuture()
      ret
    testFuture:
      ldarg.0
      isinst StockWerk.Future
      brfalse.s raise
      ldarg.0
      castclass StockWerk.Future
      call instance class StockWerk.StockWert StockWerk.Future::Request()
      starg.s 0
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed public BIFulfill extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public static class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
    retry:
      ldarg.0
      isinst StockWerk.Promise
      brfalse.s testFuture
      ldarg.0
      castclass StockWerk.Promise
      ldarg.1
      call instance void StockWerk.Promise::Fulfill(class StockWerk.StockWert)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      ret
    testFuture:
      ldarg.0
      isinst StockWerk.Future
      brfalse.s raise
      ldarg.0
      castclass StockWerk.Future
      call instance class StockWerk.StockWert StockWerk.Future::Request()
      starg.s 0
      br.s retry
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Future
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class sealed public BIAwait extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public static class StockWerk.StockWert Apply1(class StockWerk.StockWert) {
      ldarg.0
      isinst StockWerk.Transient
      brfalse.s return
      ldarg.0
      castclass StockWerk.Transient
      tail. call instance class StockWerk.StockWert StockWerk.Transient::Request()
      ret
    return:
      ldarg.0
      ret
    }
  }

  .class sealed public BIThread extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public static class StockWerk.StockWert Apply1(class StockWerk.StockWert) {
      ldarg.0
      newobj instance void StockWerk.Thread::.ctor(class StockWerk.StockWert)
      ret
    }
  }
}

.method public static void DllMain() {
  .entrypoint
  //--** the following instructions just make the DLL verifiable:
  newobj instance void StockWerk.Builtins.BIAwait::.ctor()
  pop
  ret
}
