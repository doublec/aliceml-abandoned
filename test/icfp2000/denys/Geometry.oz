functor
import
   BootValue(byNeedFail: ByNeedFail) at 'x-oz://boot/Value'
   NativeGeometry at 'Geometry.so{native}'
export
   'Geometry$': Geometry
define
   NoType = {ByNeedFail noTypeInGeometryModule}   %--**

   Geometry =
   'Geometry'(
      '$angle': NoType
      '$vec': NoType
      '$point': NoType
      '$mat': NoType

      vec: NativeGeometry.vector_new
      getX: fun {$ V} {NativeGeometry.vector_get V 1} end
      getY: fun {$ V} {NativeGeometry.vector_get V 2} end
      getZ: fun {$ V} {NativeGeometry.vector_get V 3} end

      negVec: NativeGeometry.vector_neg
      rezVec: NativeGeometry.vector_rez
      addVec: NativeGeometry.vector_add
      subVec: NativeGeometry.vector_sub
      mulScalVec: NativeGeometry.vector_scal_mul
      mulVec: NativeGeometry.vector_dot_prod
      mulMatVec: NativeGeometry.matrix_vector_mul
      mulMatPoint: NativeGeometry.matrix_point_mul
      absVec: NativeGeometry.vector_abs
      normalizeVec: NativeGeometry.vector_normalize

      unitMat: {NativeGeometry.matrix_new
		1.0 0.0 0.0 0.0
		0.0 1.0 0.0 0.0
		0.0 0.0 1.0 0.0}
      translationMat: NativeGeometry.matrix_translation
      scaleMat: NativeGeometry.matrix_scale
      uscaleMat: NativeGeometry.matrix_uscale
      rotationXMat: NativeGeometry.matrix_rotx
      rotationYMat: NativeGeometry.matrix_roty
      rotationZMat: NativeGeometry.matrix_rotz
      mulMat: NativeGeometry.matrix_mul)
end
