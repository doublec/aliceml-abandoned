#ifndef _NATIVE_GDK_SPECIAL_HH_
#define _NATIVE_GDK_SPECIAL_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>

DEFINE0(NativeGdk_init) {
  gdk_init(NULL,NULL);
  RETURN_UNIT;
} END

DEFINE1(NativeGdk_pixbufNewFromXpmData) {
  DECLARE_CARRAY(in0,x0,gchar*,DECLARE_CSTRING);
  void* ret = gdk_pixbuf_new_from_xpm_data(const_cast<const gchar **>(in0));
  RETURN(Store::UnmanagedPointerToWord(ret));
} END

DEFINE3(NativeGdk_colorNew) {
  DECLARE_INT(red,x0);
  DECLARE_INT(green,x1);
  DECLARE_INT(blue,x2);
  GdkColor *col = new GdkColor;
  col->red = static_cast<guint16>(red);
  col->green = static_cast<guint16>(green);
  col->blue = static_cast<guint16>(blue);
  RETURN(Store::UnmanagedPointerToWord(col));
} END

DEFINE2(NativeGdk_pointNew) {
  DECLARE_INT(x,x0);
  DECLARE_INT(y,x1);
  GdkPoint *p = new GdkPoint;
  p->x = x;
  p->y = y;
  RETURN(Store::UnmanagedPointerToWord(p));
} END

DEFINE4(NativeGdk_rectangleNew) {
  DECLARE_INT(x,x0);
  DECLARE_INT(y,x1);
  DECLARE_INT(width,x2);
  DECLARE_INT(height,x3);
  GdkRectangle *rect = new GdkRectangle;
  rect->x = x;
  rect->y = y;
  rect->width = width;
  rect->height = height;
  RETURN(Store::UnmanagedPointerToWord(rect));
} END

#endif
