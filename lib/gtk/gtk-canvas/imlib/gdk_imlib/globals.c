
#define _GNU_SOURCE
#include <config.h>
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

ImlibData	_gdk_real_imlib_data = { 0, 0, };
ImlibData          *_gdk_imlib_data = &_gdk_real_imlib_data;
