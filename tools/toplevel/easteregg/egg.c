#include "gdk/gdk.h"
#include "gtk/gtk.h"
#include "egg.h"

#include <stdio.h>

int main (void) {
  FILE *f = fopen("egg.txt", "w");
  int i = 0;
  
  int size = (int)my_pixbuf[4]*0x1000000+
    (int)my_pixbuf[5]*0x10000+
    (int)my_pixbuf[6]*0x100+
    (int)my_pixbuf[7];

  fwrite((void*)my_pixbuf, size, 1, f);
  fclose(f);
  return 0;
}
