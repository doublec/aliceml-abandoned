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
  .class abstract public StockWert extends System.Object {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void System.Object::.ctor()
      ret
    }
    .method abstract virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {}
    .method abstract virtual public bool Equals(class StockWerk.StockWert) {}
  }

  .class public TypeError extends System.RuntimeException {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void System.RuntimeException::.ctor()
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
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      newobj instance void StockWerk.TypeError::.ctor()
      throw
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.1
      isinst StockWerk.Int
      brfalse.s retFalse
      ldarg.1
      castclass StockWerk.Int
      ldfld int32 StockWerk.Int::Value
      ldarg.0
      ldfld int32 StockWerk.Int::Value
      bne.un.s retFalse
      ldc.i4 1
      ret
    retFalse:
      ldc.i4 0
      ret
    }
  }

  .class public Name extends StockWerk.StockWert {
    .method public void .ctor() {
      ldarg.0
      tail. call instance void StockWerk.StockWert::.ctor()
      ret
    }
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      newobj instance void StockWerk.TypeError::.ctor()
      throw
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.0
      ldarg.1
      bne.un.s retFalse
      ldc.i4 1
      ret
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
      ldarg.0
      ldarg.1
      bne.un.s retFalse
      ldc.i4 1
      ret
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
    .method virtual public class StockWerk.StockWert Apply(class StockWerk.StockWert) {
      newobj instance void StockWerk.TypeError::.ctor()
      throw
    }
    .method virtual public bool Equals(class StockWerk.StockWert) {
      ldarg.1
      isinst StockWerk.ConVal
      brfalse.s retFalse
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
    }
  }
}

/*
// The following makes the code verifiable as an EXE:
.method public static void main() {
  .entrypoint
  //ldc.i4 17
  //newobj instance void StockWerk.Int::.ctor(int32)
  //pop
  ldstr "test"
  newobj instance void StockWerk.NamedConstructor::.ctor(class System.String)
  ldstr "test"
  newobj instance void StockWerk.NamedName::.ctor(class System.String)
  callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)
  pop
  ret
}
*/
