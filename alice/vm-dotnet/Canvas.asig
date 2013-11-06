signature Canvas =
  sig
    type ty
    val nativeInspect : 'a * ty -> unit
    val makeArrowType : ty * ty -> ty
    val makeBasicType : string -> ty
    val makeListType : ty -> ty
    val makeRecordType : ty vector -> ty
    val makeTupleType : ty vector -> ty
  end
