#ifndef _NATIVE_GNOME_CANVAS_SPECIAL_HH_
#define _NATIVE_GNOME_CANVAS_SPECIAL_HH_ { 0, NULL }

DEFINE3(NativeGnomeCanvas_pointsSetCoords) {
  DECLARE_UNMANAGED_POINTER(points,x0);
  DECLARE_INT(num,x1);
  DECLARE_INT(value,x2);
  (static_cast<GnomeCanvasPoints*>(points))->coords[num] = value;
  RETURN_UNIT;
} END

#endif
