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

.namespace StockWerk {
  .class sealed public Prebound extends System.Object {
    .field static public initonly class StockWerk.StockWert TypeError
    .field static public initonly class StockWerk.StockWert Promise
    .field static public initonly class StockWerk.StockWert Future
    .field static public initonly class StockWerk.StockWert Fulfill
    .field static public initonly class StockWerk.StockWert ByNeed
  }

  .class abstract public StockWert extends System.Object {
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
    .method abstract virtual public bool Equals(class StockWerk.StockWert) {}
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
      beq.s retTrue
    retFalse:
      ldc.i4 0
      ret
    retTrue:
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
    }
  }

  .class public Name extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
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

  .class public Constructor extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      newobj instance void StockWerk.ConVal::.ctor(class StockWerk.Constructor, class StockWerk.StockWert)
      ret
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

  .class public abstract Transient extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method abstract virtual public class StockWerk.StockWert Request() {}
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
      brtrue.s raise   //--** brinst.s
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
      brfalse.s return   //--** brnull.s
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
      brtrue.s raise   //--** brinst.s
      //--** if arg 0 and arg 1 are variants of the same transient, raise Fulfill
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.Promise::Ref
      ldarg.0
      call void System.CriticalSection::Exit(class System.Object)
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
      brfalse.s raise   //--** brnull.s
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
      brfalse.s raise   //--** brnull.s
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
  }

/*--**
  .class sealed public ByNeed extends StockWerk.Transient {
    .field private class StockWerk.StockWert Proc
    .method public void .ctor(class StockWerk.StockWert) {
      ldarg.0
      call instance void StockWerk.Transient::.ctor()
      ldarg.0
      ldarg.1
      stfld class StockWerk.StockWert StockWerk.ByNeed::Proc
      ret
    }
    .method virtual public class StockWerk.StockWert Request() {
      //--** apply Proc
    }
  }
*/

  .class BuiltinTable {
    .method public static class StockWerk.StockWert Promise() {
      newobj instance void StockWerk.Promise::.ctor()
      ret
    }

    .method public static class StockWerk.StockWert Future(class StockWerk.StockWert) {
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

    .method public static class StockWerk.StockWert Fulfill(class StockWerk.StockWert) {
    retry:
      ldarg.0
      isinst StockWerk.Promise
      brfalse.s testFuture
      ldarg.0
      castclass StockWerk.Promise
      tail. call instance void StockWerk.Promise::Fulfill()
      ldnull   //--** unit
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

    .method public static class StockWerk.StockWert Await(class StockWerk.StockWert) {
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
}

.method public static void DllMain() {
  .entrypoint
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
  ret
}
