#ifndef _NATIVE_GDK_SPECIAL_HH_
#define _NATIVE_GDK_SPECIAL_HH_ { 0, NULL }

DEFINE0(NativeGdk_init) {
  gdk_init(NULL,NULL);
  RETURN_UNIT;
} END

DEFINE1(NativeGdk_pixbufNewFromXpmData) {
  DECLARE_CARRAY(in0,x0,gchar*,DECLARE_CSTRING);
  void* ret = gdk_pixbuf_new_from_xpm_data(const_cast<const gchar **>(in0));
  RETURN(Store::UnmanagedPointerToWord(ret));
} END

#endif
