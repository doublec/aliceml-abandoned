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

//
// Design Limitations:
// -- Tuple/Record need an additional array object and pointer to it.
//    In C, these would be allocated as part of the tuple object.
//

//--** use brinst and brnull instead of brtrue and brfalse

.namespace StockWerk {
  .class sealed public Prebound extends System.Object {
    // Names and Exceptions
    .field static public initonly class StockWerk.StockWert Unit
    .field static public initonly class StockWerk.StockWert TypeError
    .field static public initonly class StockWerk.StockWert Promise
    .field static public initonly class StockWerk.StockWert Future
    .field static public initonly class StockWerk.StockWert Fulfill
    .field static public initonly class StockWerk.StockWert ByNeed
    .field static public initonly class StockWerk.StockWert Pickle
    .field static public initonly class StockWerk.StockWert Match
    // Builtins
    .field static public initonly class StockWerk.StockWert promise
    .field static public initonly class StockWerk.StockWert future
    .field static public initonly class StockWerk.StockWert fulfill
    .field static public initonly class StockWerk.StockWert await
    .field static public initonly class StockWerk.StockWert thread
    .field static public initonly class StockWerk.StockWert currentThread
    .method public static void .cctor() {
      // Names and Exceptions
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
      ldstr "Match"
      newobj instance void StockWerk.NamedName::.ctor(class System.String)
      stsfld class StockWerk.StockWert StockWerk.Prebound::Match
      // Builtins
      newobj instance void StockWerk.Builtins.BIPromise::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::promise
      newobj instance void StockWerk.Builtins.BIFuture::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::future
      newobj instance void StockWerk.Builtins.BIFulfill::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::fulfill
      newobj instance void StockWerk.Builtins.BIAwait::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::await
      newobj instance void StockWerk.Builtins.BIThread::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::thread
      newobj instance void StockWerk.Builtins.BICurrentThread::.ctor()
      stsfld class StockWerk.StockWert StockWerk.Prebound::currentThread
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
      newobj instance void StockWerk.RecordArity::.ctor()
      call instance class System.Object StockWerk.RecordArity::GetRealObject()
      castclass StockWerk.RecordArity
      ret
    }
    .method virtual public int32 GetHashCode() {
      .locals(int32, int32)
      ldc.i4.0
      stloc.0
      ldc.i4.0
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
      call instance bool System.String::Equals(class System.Object)
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
    .method public virtual class System.Object GetRealObject() {
      ldsfld class System.Collections.Hashtable StockWerk.RecordArity::ArityTable
      ldarg.0
      call instance class System.Object System.Collections.Hashtable::get$Item(class System.Object)
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
      bge.s raise
      ldloc.0
      ldloc.1
      ldelem.ref
      ldarg.1
      call bool System.String::Equals(class System.String, class System.String)
      beq.s return
      ldloc.1
      ldc.i4.1
      add
      stloc.1
      br.s loop
    return:
      ldloc.1
      ret
    raise:
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Match
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
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
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
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
    .method public virtual class StockWerk.StockWert Future() {
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
    .method virtual public class System.Reflection.MemberInfo[0 ...] GetMembers(value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class System.Type System.Object::GetType()
      ldc.i4.1
      tail. callvirt instance class System.Reflection.MemberInfo[0 ...] System.Type::GetMembers(bool)
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      .locals(class System.Serialization.SerializationTypeData, value class System.Serialization.SerializationInfo)
      ldarg.0
      callvirt instance class System.Type System.Object::GetType()
      ldarg.1
      call class System.Serialization.SerializationTypeData System.Serialization.SerializationInfoGenerator::GetSerializationTypeData(class System.Type, int32)
      stloc.0
      ldloca.s 1
      ldloc.0
      ldarg.0
      call instance void System.Serialization.SerializationInfo::.ctor(class System.Serialization.SerializationTypeData, class System.Object)
      ldarg.0
      ldloc.1
      call void System.Serialization.SerializationInfoGenerator::PopulateSerializationInfo(class System.Object, value class System.Serialization.SerializationInfo)
      ldloc.1
      ret
    }
    .method virtual public void PopulateData(class System.Reflection.MemberInfo[0 ...], value class System.Variant[0 ...]) {
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
      callvirt instance void System.Reflection.FieldInfo::SetValue(class System.Object, value class System.Variant)
      ldloc.0
      ldc.i4.1
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
      ldarg.0
      ldarg.1
      ldarg.2
      tail. call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(int32, value class System.Serialization.StreamingContext)
      ret
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

  .class public Int extends StockWerk.StockWert {
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
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class sealed public Word extends StockWerk.Int {
    .method public void .ctor(int32) {
      ldarg.0
      ldarg.1
      tail. call instance void StockWerk.Int::.ctor(int32)
      ret
    }
  }

  .class sealed public Char extends StockWerk.Int {
    .method public void .ctor(int32) {
      ldarg.0
      ldarg.1
      tail. call instance void StockWerk.Int::.ctor(int32)
      ret
    }
  }

  .class sealed public String extends StockWerk.StockWert {
    .field public initonly class System.String Value
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.String::Value
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
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
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
  }

  .class sealed public Real extends StockWerk.StockWert {
    .field public initonly float32 Value
    .method public void .ctor(float32) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld float32 StockWerk.Real::Value
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
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
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
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
      ldc.i4.0
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
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
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
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
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Ref::Value
      ret
    }
    .method public class StockWerk.StockWert Access() {
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

  .class public Tuple2 extends StockWerk.StockWert {
    .field public initonly class StockWerk.StockWert Value1
    .field public initonly class StockWerk.StockWert Value2
    .method public void .ctor(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value1
      ldarg.0
      ldarg.2
      stfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      ret
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
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
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      brfalse.s retFalse
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      ldloc.0
      ldfld class StockWerk.StockWert StockWerk.Tuple2::Value2
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
      brfalse.s retFalse
      ldc.i4.1
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
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
  }

  .class public Tuple extends StockWerk.StockWert {
    .field public initonly class StockWerk.StockWert[] Values
    .method public void .ctor(class StockWerk.StockWert[]) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
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
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
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
      castclass StockWerk.Transient
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
      callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
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
      castclass StockWerk.Transient
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
      ldfld class StockWerk.StockWert[] StockWerk.Tuple::Values
      ldarg.0
      ldfld class StockWerk.RecordArity StockWerk.Record::Arity
      ldarg.1
      call instance int32 StockWerk.RecordArity::Select(class System.String)
      ldelem.ref
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
    .method virtual public class StockWerk.StockWert Select(class System.String) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::Select(class System.String)
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldarg.0
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      pop
      ldarg.0
      ldarg.1
      ldarg.2
      tail. call instance value class System.Serialization.SerializationInfo StockWerk.StockWert::GetObjectData(int32, value class System.Serialization.StreamingContext)
      ret
    }
    .method virtual public class System.Object GetRealObject() {
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
    .method public virtual class StockWerk.StockWert Future() {
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
    .method virtual public class StockWerk.StockWert Await() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Promise
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
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
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      ldfld class StockWerk.Promise StockWerk.Future::Prom
      call instance class StockWerk.StockWert StockWerk.Promise::AwaitInternal()
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
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
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      call instance class StockWerk.StockWert StockWerk.ByNeed::Await()
      ldarg.1
      tail. callvirt instance bool StockWerk.StockWert::Equals(class StockWerk.StockWert)
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
        ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
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
  }

  //
  // Procedures
  //

  .class public sealed Selector extends StockWerk.StockWert {
    .field private initonly class System.String Label
    .method public void .ctor(class System.String) {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      ldarg.1
      stfld class System.String StockWerk.Selector::Label
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      ldarg.0
      ldfld class System.String StockWerk.Selector::Label
      tail. callvirt class StockWerk.StockWert StockWerk.StockWert::Select(class System.String)
      ret
    }
    .method public bool Equals(class StockWerk.StockWert) {
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
      castclass StockWerk.Transient
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
      tail. call instance void StockWerk.Global::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply0() {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply3(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply4(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply5(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply6(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
    .method public class StockWerk.StockWert Apply7(class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert, class StockWerk.StockWert) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::TypeError
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }

  .class public abstract Procedure0 extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
    retry:
      ldarg.1
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      bne.un.s testTransient
      ldarg.0
      tail. callvirt instance class StockWerk.StockWert StockWerk.Procedure::Apply0()
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      castclass StockWerk.Transient
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
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      .locals(class StockWerk.StockTuple2)
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
      tail. callvirt instance class StockWerk.StockWert StockWerk.Procedure::Apply2(class StockWerk.StockWert, class StockWerk.StockWert)
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s raise
      ldarg.1
      castclass StockWerk.Transient
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

  .class public sealed ThreadStart extends System.Object {
    .field private class StockWerk.StockWert Proc
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ret
    }
    .method private void Body() {
      //--** catch and print exceptions?
      ldarg.0
      ldfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ldarg.0
      ldnull
      stfld class StockWerk.StockWert StockWerk.ThreadStart::Proc
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
      pop
      ret
    }
  }

  .class public sealed Thread extends StockWerk.StockWert {
    .field private class System.Thread Thr
    .method public void .ctor() {
      ldarg.0
      call instance void StockWerk.StockWert::.ctor()
      ldarg.0
      call class System.Thread System.Thread::get$CurrentThread()
      stfld class System.Thread StockWerk.Thread::Thr
      ret
    }
    .method public bool Equals(class StockWerk.StockWert) {
    retry:
      ldarg.1
      isinst StockWerk.Thread
      brfalse.s testTransient
      ldarg.1
      castclass StockWerk.Thread
      ldfld class System.Thread StockWerk.Thread::Thr
      ldarg.0
      ldfld class System.Thread StockWerk.Thread::Thr
      ceq
      ret
    testTransient:
      ldarg.1
      isinst StockWerk.Transient
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Transient
      callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      starg.s 1
      br.s retry
    retFalse:
      ldc.i4.0
      ret
    }
    .method virtual public value class System.Serialization.SerializationInfo GetObjectData(int32, value class System.Serialization.StreamingContext) {
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Pickle
      newobj instance void StockWerk.ExceptionWrapper::.ctor(class StockWerk.StockWert)
      throw
    }
  }
}

//
// Builtins
//

.namespace StockWerk.Builtins {
  .class sealed BIPromise extends StockWerk.Procedure0 {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure0::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply0() {
      newobj instance void StockWerk.Promise::.ctor()
      ret
    }
  }

  .class sealed BIFuture extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Future()
      ret
    }
  }

  .class sealed BIFulfill extends StockWerk.Procedure2 {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure2::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply2(class StockWerk.StockWert, class StockWerk.StockWert) {
      ldarg.1
      ldarg.2
      callvirt instance void StockWerk.StockWert::Fulfill(class StockWerk.StockWert)
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      ret
    }
  }

  .class sealed BIAwait extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      tail. callvirt instance class StockWerk.StockWert StockWerk.StockWert::Await()
      ret
    }
  }

  .class sealed BIThread extends StockWerk.Procedure {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.1
      newobj instance void StockWerk.ThreadStart::.ctor(class StockWerk.StockWert)
      ldftn instance void StockWerk.ThreadStart::Body()
      newobj instance void System.ThreadStart::.ctor(class System.Object, int32)
      newobj instance void System.Thread::.ctor(class System.ThreadStart)
      call instance void System.Thread::Start()
      ldsfld class StockWerk.StockWert StockWerk.Prebound::Unit
      ret
    }
  }

  .class sealed BICurrentThread extends StockWerk.Procedure0 {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.Procedure0::.ctor()
      ret
    }
    .method public class StockWerk.StockWert Apply0() {
      newobj instance void StockWerk.Thread::.ctor()
      ret
    }
  }
}
