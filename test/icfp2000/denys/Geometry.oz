functor
import
   BootValue(byNeedFail: ByNeedFail) at 'x-oz://boot/Value'
   NativeGeometry at 'Geometry.so{native}'
export
   '$angle': NoType   %--**
   '$vec': NoType   %--**
   '$point': NoType   %--**
   '$mat': NoType   %--**

   Vec
   GetX
   GetY
   GetZ
   NegVec
   RezVec
   AddVec
   SubVec
   MulScalVec
   MulVec
   MulMatVec
   MulMatPoint
   AbsVec
   NormalizeVec
   UnitMat
   TranslationMat
   ScaleMat
   UscaleMat
   RotationXMat
   RotationYMat
   RotationZMat
   MulMat
define
   NoType = {ByNeedFail noTypeInGeometryModule}   %--**

   Vec = NativeGeometry.vector_new
   fun {GetX V} {NativeGeometry.vector_get V 1} end
   fun {GetY V} {NativeGeometry.vector_get V 2} end
   fun {GetZ V} {NativeGeometry.vector_get V 3} end

   NegVec = NativeGeometry.vector_neg
   RezVec = NativeGeometry.vector_rez
   AddVec = NativeGeometry.vector_add
   SubVec = NativeGeometry.vector_sub
   MulScalVec = NativeGeometry.vector_scal_mul
   MulVec = NativeGeometry.vector_dot_prod
   MulMatVec = NativeGeometry.matrix_vector_mul
   MulMatPoint = NativeGeometry.matrix_point_mul
   AbsVec = NativeGeometry.vector_abs
   NormalizeVec = NativeGeometry.vector_normalize

   UnitMat = {NativeGeometry.matrix_new
	      1.0 0.0 0.0 0.0
	      0.0 1.0 0.0 0.0
	      0.0 0.0 1.0 0.0}
   TranslationMat = NativeGeometry.matrix_translation
   ScaleMat = NativeGeometry.matrix_scale
   UscaleMat = NativeGeometry.matrix_uscale
   RotationXMat = NativeGeometry.matrix_rotx
   RotationYMat = NativeGeometry.matrix_roty
   RotationZMat = NativeGeometry.matrix_rotz
   MulMat = NativeGeometry.matrix_mul
end
