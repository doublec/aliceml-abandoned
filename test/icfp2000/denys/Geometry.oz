functor
import
   NativeGeometry at 'Geometry.so{native}'
export
   '$Geometry': Geometry
define
   Geometry =
   'Geometry'(
      vec: fun {$ X Y Z} {NativeGeometry.vector_new X Y Z} end
      getX: fun {$ V} {NativeGeometry.vector_get V 1} end
      getY: fun {$ V} {NativeGeometry.vector_get V 2} end
      getZ: fun {$ V} {NativeGeometry.vector_get V 3} end

      negVec: NativeGeometry.vector_neg
      rezVec: NativeGeometry.vector_rez
      addVec: fun {$ X Y} {NativeGeometry.vector_add X Y} end
      subVec: fun {$ X Y} {NativeGeometry.vector_sub X Y} end
      mulScalVec: fun {$ X Y} {NativeGeometry.vector_scal_mul X Y} end
      mulVec: fun {$ X Y} {NativeGeometry.vector_dot_prod X Y} end
      mulMatVec: fun {$ X Y} {NativeGeometry.matrix_vector_mul X Y} end
      mulMatPoint: fun {$ X Y} {NativeGeometry.matrix_point_mul X Y} end
      absVec: NativeGeometry.vector_abs
      normalizeVec: NativeGeometry.vector_normalize

      unitMat: {NativeGeometry.matrix_new
		1.0 0.0 0.0 0.0
		0.0 1.0 0.0 0.0
		0.0 0.0 1.0 0.0}
      translationMat:
	 fun {$ X Y Z} {NativeGeometry.matrix_translation X Y Z} end
      scaleMat:
	 fun {$ X Y Z} {NativeGeometry.matrix_scale X Y Z} end
      uscaleMat: NativeGeometry.matrix_uscale
      rotationXMat: NativeGeometry.matrix_rotx
      rotationYMat: NativeGeometry.matrix_roty
      rotationZMat: NativeGeometry.matrix_rotz
      mulMat: fun {$ X Y} {NativeGeometry.matrix_mul X Y} end)
end
