#include <config.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

int
saver_ps (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
      int                 bx, by, bxx, byy;
      int                 w, h;
      int                 sx, sy;
      int                 tx = 35, ty = 35;
      int                 x, y;
      unsigned char      *ptr;
      int                 v;
      FILE                *f;
      
      sx = 0;
      sy = 0;
      f = fopen(file, "wb");

      if (f == NULL)
	return 0;

      w = im->rgb_width;
      h = im->rgb_height;

      switch (info->page_size)
	{
	case PAGE_SIZE_EXECUTIVE:
	  sx = 540;
	  sy = 720;
	  break;
	case PAGE_SIZE_LETTER:
	  sx = 612;
	  sy = 792;
	  break;
	case PAGE_SIZE_LEGAL:
	  sx = 612;
	  sy = 1008;
	  break;
	case PAGE_SIZE_A4:
	  sx = 595;
	  sy = 842;
	  break;
	case PAGE_SIZE_A3:
	  sx = 842;
	  sy = 1190;
	  break;
	case PAGE_SIZE_A5:
	  sx = 420;
	  sy = 595;
	  break;
	case PAGE_SIZE_FOLIO:
	  sx = 612;
	  sy = 936;
	  break;
	}
      bxx = ((sx - (tx * 2)) * info->scaling) >> 10;
      byy = (int)(((float)h / (float)w) * (float)bxx);
      if ((((sy - (ty * 2)) * info->scaling) >> 10) < byy)
	{
	  byy = ((sy - (ty * 2)) * info->scaling) >> 10;
	  bxx = (int)(((float)w / (float)h) * (float)byy);
	}
      bx = tx + ((((sx - (tx * 2)) - bxx) * info->xjustification) >> 10);
      by = ty + ((((sy - (ty * 2)) - byy) * info->yjustification) >> 10);
      if (f)
	{
	  fprintf(f, "%%!PS-Adobe-2.0 EPSF-2.0\n");
	  fprintf(f, "%%%%Title: %s\n", file);
	  fprintf(f, "%%%%Creator: Imlib by The Rasterman\n");
	  fprintf(f, "%%%%BoundingBox: %i %i %i %i\n", bx, by, bxx, byy);
	  fprintf(f, "%%%%Pages: 1\n");
	  fprintf(f, "%%%%DocumentFonts:\n");
	  fprintf(f, "%%%%EndComments\n");
	  fprintf(f, "%%%%EndProlog\n");
	  fprintf(f, "%%%%Page: 1 1\n");
	  fprintf(f, "/origstate save def\n");
	  fprintf(f, "20 dict begin\n");
	  if (info->color)
	    {
	      fprintf(f, "/pix %i string def\n", w * 3);
	      fprintf(f, "/grays %i string def\n", w);
	      fprintf(f, "/npixls 0 def\n");
	      fprintf(f, "/rgbindx 0 def\n");
	      fprintf(f, "%i %i translate\n", bx, by);
	      fprintf(f, "%i %i scale\n", bxx, byy);
	      fprintf(f,
		      "/colorimage where\n"
		      "{ pop }\n"
		      "{\n"
		      "/colortogray {\n"
		      "/rgbdata exch store\n"
		      "rgbdata length 3 idiv\n"
		      "/npixls exch store\n"
		      "/rgbindx 0 store\n"
		      "0 1 npixls 1 sub {\n"
		      "grays exch\n"
		      "rgbdata rgbindx       get 20 mul\n"
		      "rgbdata rgbindx 1 add get 32 mul\n"
		      "rgbdata rgbindx 2 add get 12 mul\n"
		      "add add 64 idiv\n"
		      "put\n"
		      "/rgbindx rgbindx 3 add store\n"
		      "} for\n"
		      "grays 0 npixls getinterval\n"
		      "} bind def\n"
		      "/mergeprocs {\n"
		      "dup length\n"
		      "3 -1 roll\n"
		      "dup\n"
		      "length\n"
		      "dup\n"
		      "5 1 roll\n"
		      "3 -1 roll\n"
		      "add\n"
		      "array cvx\n"
		      "dup\n"
		      "3 -1 roll\n"
		      "0 exch\n"
		      "putinterval\n"
		      "dup\n"
		      "4 2 roll\n"
		      "putinterval\n"
		      "} bind def\n"
		      "/colorimage {\n"
		      "pop pop\n"
		      "{colortogray} mergeprocs\n"
		      "image\n"
		      "} bind def\n"
		      "} ifelse\n");
	      fprintf(f, "%i %i 8\n", w, h);
	      fprintf(f, "[%i 0 0 -%i 0 %i]\n", w, h, h);
	      fprintf(f, "{currentfile pix readhexstring pop}\n");
	      fprintf(f, "false 3 colorimage\n");
	      fprintf(f, "\n");
	      ptr = im->rgb_data;
	      for (y = 0; y < h; y++)
		{
		  for (x = 0; x < w; x++)
		    {
		      v = (int)(*ptr++);
		      if (v < 0x10)
			fprintf(f, "0%x", v);
		      else
			fprintf(f, "%x", v);
		      v = (int)(*ptr++);
		      if (v < 0x10)
			fprintf(f, "0%x", v);
		      else
			fprintf(f, "%x", v);
		      v = (int)(*ptr++);
		      if (v < 0x10)
			fprintf(f, "0%x", v);
		      else
			fprintf(f, "%x", v);
		    }
		  fprintf(f, "\n");
		}
	    }
	  else
	    {
	      fprintf(f, "/pix %i string def\n", w);
	      fprintf(f, "/grays %i string def\n", w);
	      fprintf(f, "/npixls 0 def\n");
	      fprintf(f, "/rgbindx 0 def\n");
	      fprintf(f, "%i %i translate\n", bx, by);
	      fprintf(f, "%i %i scale\n", bxx, byy);
	      fprintf(f, "%i %i 8\n", w, h);
	      fprintf(f, "[%i 0 0 -%i 0 %i]\n", w, h, h);
	      fprintf(f, "{currentfile pix readhexstring pop}\n");
	      fprintf(f, "image\n");
	      fprintf(f, "\n");
	      ptr = im->rgb_data;
	      for (y = 0; y < h; y++)
		{
		  for (x = 0; x < w; x++)
		    {
		      v = (int)(*ptr++);
		      v += (int)(*ptr++);
		      v += (int)(*ptr++);
		      v /= 3;
		      if (v < 0x10)
			fprintf(f, "0%x", v);
		      else
			fprintf(f, "%x", v);
		    }
		  fprintf(f, "\n");
		}
	    }
	  fprintf(f, "\n");
	  fprintf(f, "showpage\n");
	  fprintf(f, "end\n");
	  fprintf(f, "origstate restore\n");
	  fprintf(f, "%%%%Trailer\n");
	  fclose(f);
	  return 1;
	}
      return 0;
}
