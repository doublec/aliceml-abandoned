#ifndef _NATIVE_GNOME_CANVAS_SPECIAL_HH_
#define _NATIVE_GNOME_CANVAS_SPECIAL_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>

DEFINE3(NativeGnomeCanvas_pointsSetCoords) {
  DECLARE_OBJECT(points,x0);
  DECLARE_INT(num,x1);
  DECLARE_INT(value,x2);
  (static_cast<GnomeCanvasPoints*>(points))->coords[num] = value;
  RETURN_UNIT;
} END

DEFINE2(NativeGnomeCanvas_itemNew) {
  DECLARE_OBJECT(parent,x0);
  DECLARE_INT(type,x1);
  GnomeCanvasItem *ret = gnome_canvas_item_new(
		           static_cast<GnomeCanvasGroup*>(parent), 
			   static_cast<GtkType>(type), NULL);
  RETURN(PointerToObject(ret,TYPE_GTK_OBJECT));
} END

#endif
