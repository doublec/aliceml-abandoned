.method public static void main() {
  .entrypoint
  .locals(class System.IO.FileStream)
  ldstr "pickle"
  ldc.i4 2   // System.IO.FileMode::Create
  ldc.i4 2   // System.IO.FileAccess::Write
  newobj instance void System.IO.FileStream::.ctor(class System.String, int32, int32)
  dup
  stloc.0
  newobj instance void System.Serialization.PseudoMLFormatter::.ctor(class System.IO.Stream)

  ldstr "TestConstructor"
  newobj instance void StockWerk.NamedConstructor::.ctor(class System.String)
  ldstr "TestName"
  newobj instance void StockWerk.NamedName::.ctor(class System.String)
  callvirt instance class StockWerk.StockWert StockWerk.StockWert::Apply(class StockWerk.StockWert)

  ldc.i4 5   // System.Serialization.FormatterEnum::PublicFields | ...::PrivateFields
  callvirt instance void System.Serialization.PseudoMLFormatter::Serialize(class System.Object, int32)

  ldloc.0
  tail. callvirt instance void System.IO.FileStream::Close()

  ret
}
