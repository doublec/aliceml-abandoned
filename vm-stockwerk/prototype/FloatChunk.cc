//
// Authors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

#include <mozart.h>

union FloatChunk {
  unsigned char c[sizeof(double)];
  int i[sizeof(double) / sizeof(int)];
  double d;
};

static bool littleEndian;

OZ_BI_define(toOz,8,1) {
  OZ_declareInt(0,i0); OZ_declareInt(1,i1);
  OZ_declareInt(2,i2); OZ_declareInt(3,i3);
  OZ_declareInt(4,i4); OZ_declareInt(5,i5);
  OZ_declareInt(6,i6); OZ_declareInt(7,i7);

  FloatChunk x;
  if (littleEndian) {
    x.c[7] = i0; x.c[6] = i1; x.c[5] = i2; x.c[4] = i3;
    x.c[3] = i4; x.c[2] = i5; x.c[1] = i6; x.c[0] = i7;
  } else {
    x.c[0] = i0; x.c[1] = i1; x.c[2] = i2; x.c[3] = i3;
    x.c[4] = i4; x.c[5] = i5; x.c[6] = i6; x.c[7] = i7;
  }
  OZ_RETURN(OZ_float(x.d));
} OZ_BI_end

OZ_BI_define(fromOz,1,8) {
  OZ_declareFloat(0,d);

  FloatChunk x;
  x.d = d;
  if (littleEndian) {
    OZ_out(0) = OZ_int(x.c[7]); OZ_out(1) = OZ_int(x.c[6]);
    OZ_out(2) = OZ_int(x.c[5]); OZ_out(3) = OZ_int(x.c[4]);
    OZ_out(4) = OZ_int(x.c[3]); OZ_out(5) = OZ_int(x.c[2]);
    OZ_out(6) = OZ_int(x.c[1]); OZ_out(7) = OZ_int(x.c[0]);
  } else {
    OZ_out(0) = OZ_int(x.c[0]); OZ_out(1) = OZ_int(x.c[1]);
    OZ_out(2) = OZ_int(x.c[2]); OZ_out(3) = OZ_int(x.c[3]);
    OZ_out(4) = OZ_int(x.c[4]); OZ_out(5) = OZ_int(x.c[5]);
    OZ_out(6) = OZ_int(x.c[6]); OZ_out(7) = OZ_int(x.c[7]);
  }
  return PROCEED;
} OZ_BI_end

char oz_module_name[] = "FloatChunk";

OZ_C_proc_interface *oz_init_module() {
  static OZ_C_proc_interface interface[] = {
    {"toOz",8,1,toOz},
    {"fromOz",1,8,fromOz},
    {0,0,0,0}
  };

  FloatChunk x;
  x.i[0] = 1;
  for (int i = 1; i < sizeof(double) / sizeof(int); i++)
    x.i[i] = 0;
  if (x.c[0] == 1)
    littleEndian = true;
  else
    littleEndian = false;

  return interface;
}
