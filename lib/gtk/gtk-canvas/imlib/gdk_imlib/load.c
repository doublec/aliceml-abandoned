#define _GNU_SOURCE
#include <config.h>
#include <glib.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

#include <setjmp.h>

/*      Split the ID - damages input    */

static char        *
g_SplitID(char *file)
{
  char               *p = strrchr(file, ':');

  if (p == NULL)
    return "";
  else
    {
      *p++ = 0;
      return p;
    }
}

/*
 *     Doesn't damage the input
 */

char               *
_gdk_imlib_GetExtension(char *file)
{
  char               *p = strrchr(file, '.');

  if (p == NULL)
    return "";
  else
    return p + 1;
}

int
_gdk_imlib_ispnm(FILE *f)
{
  char                buf[8];

  if (!f)
    return 0;
  fgets(buf, 8, f);
  rewind(f);
  if (!strcmp("P6\n", buf))
    return 1;
  if (!strcmp("P5\n", buf))
    return 1;
  if (!strcmp("P4\n", buf))
    return 1;
  if (!strcmp("P3\n", buf))
    return 1;
  if (!strcmp("P2\n", buf))
    return 1;
  if (!strcmp("P1\n", buf))
    return 1;
  return 0;
}

int
_gdk_imlib_isjpeg(FILE *f)
{
  unsigned char       buf[8];

  if (!f)
    return 0;
  fread(buf, 1, 2, f);
  rewind(f);
  if ((buf[0] == 0xff) && (buf[1] == 0xd8))
    return 1;
  return 0;
}

int
_gdk_imlib_ispng(FILE *f)
{
	unsigned char       buf[8];
	
	if (!f)
		return 0;
	fread(buf, 1, 8, f);
	rewind(f);
	if (buf [0] != 0x89 ||
	    buf [1] != 'P' ||
	    buf [2] != 'N' ||
	    buf [3] != 'G' ||
	    buf [4] != 0x0d ||
	    buf [5] != 0x0a ||
	    buf [6] != 0x1a ||
	    buf [7] != 0x0a)
		return FALSE;
	return TRUE;
}

int
_gdk_imlib_istiff(FILE *f)
{
  char                buf[8];

  if (!f)
    return 0;
  fgets(buf, 5, f);
  rewind(f);
  if ((buf[0] == 'M') && (buf[1] == 'M') && (buf[2] == 0x00) && (buf[3] == 0x2a))
    return 1;
  if ((buf[0] == 'I') && (buf[1] == 'I') && (buf[2] == 0x2a) && (buf[3] == 0x00))
    return 1;
  return 0;
}

int
_gdk_imlib_iseim(FILE *f)
{
  char                buf[8];

  if (!f)
    return 0;
  fread(buf, 1, 4, f);
  rewind(f);
  if (!strncmp("EIM ", buf, 4))
    return 1;
  return 0;
}

int
_gdk_imlib_isgif(FILE *f)
{
  char                buf[8];

  if (!f)
    return 0;
  fread(buf, 1, 4, f);
  rewind(f);
  buf[4] = 0;
  if (!strcmp("GIF8", buf))
    return 1;
  return 0;
}

int
_gdk_imlib_isxpm(FILE *f)
{
  char                buf[11];

  if (!f)
    return 0;
  fread(buf, 1, 9, f);
  rewind(f);
  buf[9] = 0;
  if (!strcmp("/* XPM */", buf))
    return 1;
  return 0;
}

int
_gdk_imlib_isbmp(FILE *f)
{
  char                buf[3];

  if (!f)
    return 0;
  fread(buf, 1, 2, f);
  rewind(f);

  buf[2] = 0;
  if (!strcmp("BM", buf))
    return 1;
  return 0;
}



GdkImlibImage      *
gdk_imlib_load_image(char *file)
{
  int                 w, h;
  int                 needs_conv = 1;
  unsigned char      *data;
  GdkImlibImage      *im;
  char               *e;
  FILE               *p;
  int                 eim;
  int                 fmt;
  int                 trans;

  eim = 0;
  fmt = 0;
  p = NULL;
  data = NULL;

  if (!file)
    return NULL;
  if (id->cache.on_image)
    if ((im = _gdk_imlib_find_image(file)))
      {
        if (im->rgb_width == 0 || im->rgb_height == 0)
          {
            gdk_imlib_destroy_image(im);
            return NULL;
          }
        else
          return im;
      }
  if (!strcmp(file,"-"))
    p = stdin;
  else
    p = fopen(file, "rb");
  if (!p)
    return NULL;
  

  e = _gdk_imlib_GetExtension(file);

  if (_gdk_imlib_ispnm(p))
    {
      needs_conv = 0;
      fmt = 0;
    }
  else if (_gdk_imlib_isjpeg(p))
    {
      needs_conv = 0;
      fmt = 2;
    }
  else if (_gdk_imlib_istiff(p))
    {
      needs_conv = 0;
      fmt = 3;
    }
  else if (_gdk_imlib_iseim(p))
    {
      needs_conv = 0;
      eim = 1;
      fmt = 9999;
    }
  else if (_gdk_imlib_isxpm(p))
    {
      needs_conv = 0;
      fmt = 5;
    }
  else if (_gdk_imlib_ispng(p))
    {
      needs_conv = 0;
      fmt = 1;
    }
  else if (_gdk_imlib_isgif(p))
    {
      needs_conv = 0;
      fmt = 4;
    }
  else if (_gdk_imlib_isbmp(p))
    {
      needs_conv = 0;
      fmt = 6;
    }

  if (needs_conv && id->fallback)
    {
      if (p != stdin)
        fclose(p);
      p = _gdk_imlib_open_helper("%C/convert %s pnm:-", file, "rb");
    }
  
  trans = 0;
  if (!eim && !data)
    {
      switch (fmt)
	{
	case 6:
	  if (p)
	    data = (*_gdk_imlib_LoadBMP)(p, &w, &h, &trans);
	  break;
	case 5:
	  if (p)
	    data = (*_gdk_imlib_LoadXPM)(p, &w, &h, &trans);
	  break;
	case 4:
	  if (p)
	    data = (*_gdk_imlib_LoadGIF)(p, &w, &h, &trans);
	  break;
	case 3:
	  if (p)
	    data = (*_gdk_imlib_LoadTIFF)(p, file, &w, &h, &trans);
	  break;
	case 2:
	  if (p)
	    data = (*_gdk_imlib_LoadJPEG)(p, &w, &h, &trans);
	  break;
	case 1:
	  if (p)
	    data = (*_gdk_imlib_LoadPNG)(p, &w, &h, &trans);
	  break;
	case 0:
	  if (p)
	    data = (*_gdk_imlib_LoadPPM)(p, &w, &h, &trans);
	  break;
	}
    }

  if (p && !needs_conv) {
	if (p != stdin)
      fclose(p);
  }
  else if (p)
    _gdk_imlib_close_helper(p);
  

  if ((!data) && (id->fallback))
    {
      p = _gdk_imlib_open_helper("%C/convert %s pnm:-", file, "rb");
      if (p)
	{
	  data = _gdk_imlib_LoadPPM (p, &w, &h, &trans);
	  _gdk_imlib_close_helper(p);
	}
    }

#if S_WAS_NOT_INITIALIZED_HERE_FIX_THIS
  if ((!eim) && (!data) && (id->fallback))
    {
      if (!strcasecmp(s, "jpeg"))
	strcpy(cmd, "%J %s");
      else if (!strcasecmp(s, "jpg"))
	strcpy(cmd, "%J %s");
      else if (!strcasecmp(s, "bmp"))
	strcpy(cmd, "%P/bmptoppm %s");
      else if (!strcasecmp(s, "ilbm"))
	strcpy(cmd, "%P/ilbmtoppm %s");
      else if (!strcasecmp(s, "ilb"))
	strcpy(cmd, "%P/ilbmtoppm %s");
      else if (!strcasecmp(s, "iff"))
	strcpy(cmd, "%P/ilbmtoppm %s");
      else if (!strcasecmp(s, "img"))
	strcpy(cmd, "%P/imgtoppm %s");
      else if (!strcasecmp(s, "mtv"))
	strcpy(cmd, "%P/mtvtoppm %s");
      else if (!strcasecmp(s, "pcx"))
	strcpy(cmd, "%P/pcxtoppm %s");
      else if (!strcasecmp(s, "pgm"))
	strcpy(cmd, "%P/pgmtoppm rgb:ffff/ffff/ffff %s");
      else if (!strcasecmp(s, "pi1"))
	strcpy(cmd, "%P/pi1toppm %s");
      else if (!strcasecmp(s, "pict"))
	strcpy(cmd, "%P/picttoppm %s");
      else if (!strcasecmp(s, "pic"))
	strcpy(cmd, "%P/picttoppm %s");
      else if (!strcasecmp(s, "pj"))
	strcpy(cmd, "%P/pjtoppm %s");
      else if (!strcasecmp(s, "qrt"))
	strcpy(cmd, "%P/qrttoppm %s");
      else if (!strcasecmp(s, "sld"))
	strcpy(cmd, "%P/sldtoppm %s");
      else if (!strcasecmp(s, "spc"))
	strcpy(cmd, "%P/spctoppm %s");
      else if (!strcasecmp(s, "spu"))
	strcpy(cmd, "%P/sputoppm %s");
      else if (!strcasecmp(s, "tga"))
	strcpy(cmd, "%P/tgatoppm %s");
      else if (!strcasecmp(s, "xim"))
	strcpy(cmd, "%P/ximtoppm %s");
      else if (!strcasecmp(s, "xpm"))
	strcpy(cmd, "%P/xpmtoppm %s");
      else if (!strcasecmp(s, "gif"))
	strcpy(cmd, "%P/giftopnm %s");
      else if (!strcasecmp(s, "rast"))
	strcpy(cmd, "%P/rasttopnm %s");
      else if (!strcasecmp(s, "ras"))
	strcpy(cmd, "%P/rasttopnm %s");
      else if (!strcasecmp(s, "sgi"))
	strcpy(cmd, "%P/sgitopnm %s");
      else if (!strcasecmp(s, "sir"))
	strcpy(cmd, "%P/sirtopnm %s");
      else if (!strcasecmp(s, "tiff"))
	strcpy(cmd, "%P/tifftopnm %s");
      else if (!strcasecmp(s, "tif"))
	strcpy(cmd, "%P/tifftopnm %s");
      else if (!strcasecmp(s, "wxd"))
	strcpy(cmd, "%P/wxdtopnm %s");
      else if (!strcasecmp(s, "zeiss"))
	strcpy(cmd, "%P/zeisstopnm -ppm %s");
      else if (!strcasecmp(s, "zei"))
	strcpy(cmd, "%P/zeisstopnm -ppm %s");
      else if (!strcasecmp(s, "zis"))
	strcpy(cmd, "%P/zeisstopnm -ppm %s");
      else
	strcpy(cmd, "%P/anytopnm %s");
      p = _gdk_imlib_open_helper(cmd, file, "rb");
      if (p)
	{
	  data = _gdk_imlib_LoadPPM(p, &w, &h);
	  _gdk_imlib_close_helper(p);
	}
    }
#endif
  if (!eim && !data)
    {
      fprintf(stderr, "gdk_imlib ERROR: Cannot load image: %s\nAll fallbacks failed.\n", file);
      return NULL;
    }
    
  if (w == 0 || h == 0)
    {
    	if (data)
    	  free(data);
    	return NULL;
    }

  im = (GdkImlibImage *) malloc(sizeof(GdkImlibImage));
  if (!im)
    {
      fprintf(stderr, "gdk_imlib ERROR: Cannot allocate RAM for image data\n");
      if (data)
	free(data);
      return NULL;
    }
  im->alpha_data = NULL;
  im->map = NULL;
  if (trans)
    {
      im->shape_color.r = 255;
      im->shape_color.g = 0;
      im->shape_color.b = 255;
    }
  else
    {
      im->shape_color.r = -1;
      im->shape_color.g = -1;
      im->shape_color.b = -1;
    }
  im->border.left = 0;
  im->border.right = 0;
  im->border.top = 0;
  im->border.bottom = 0;
  im->cache = 1;
  im->rgb_data = data;
  im->rgb_width = w;
  im->rgb_height = h;
  im->pixmap = NULL;
  im->shape_mask = NULL;

#if THIS_CODE_IS_BROKEN_WITH_REGARD_TO_FILENAMES_WITH_COLONS

  /*
   * The code here depends on the filenames not having a ':',
   * which is not the case for a lot of my files.
   */
  if (eim)
    {
      char                s1[256], s2[256];
      char                s[4096];
      int                 num, size;
      int                 r, g, b;
      int                 br, bl, bt, bb;

      /* Load Native-as-can-be EIM format (Enlightenment IMlib format) */
      if (!strcmp(file,"-"))
	p = stdin;
      else
        p = fopen(file, "r");
      if (!p)
	{
	  free(im);
	  return NULL;
	}
      fgets(s, sizeof (s), p);
      if ((s[0] != 'E') && (s[1] != 'I') && (s[2] != 'M') && (s[3] != ' '))
	{
	  fclose(p);
	  free(im);
	  return NULL;
	}
      sscanf(s, "%256s %i", s1, &num);
      if (num <= 0)
	{
	  fclose(p);
	  free(im);
	  return NULL;
	}
      while (fgets(s, sizeof (s), p))
	{
	  sscanf(s, "%256s", s1);
	  if (!strcmp("IMAGE", s1))
	    {
	      sscanf(s, "%256s %i %256s %i %i %i %i %i %i %i %i %i", s1, &size, s2, &w, &h, &r, &g, &b, &bl, &br, &bt, &bb);
	      if (!iden[0])
		break;
	      else if (!strcmp(iden, s2))
		break;
	      if (size > 0)
		fseek(p, size, SEEK_CUR);
	    }
	}
      im->rgb_data = malloc(w * h * 3);
      if (!im->rgb_data)
	{
	  fclose(p);
	  free(im);
	  return NULL;
	}
      im->shape_color.r = r;
      im->shape_color.g = g;
      im->shape_color.b = b;
      im->rgb_width = w;
      im->rgb_height = h;
      im->border.left = bl;
      im->border.right = br;
      im->border.top = bt;
      im->border.bottom = bb;
      fread(im->rgb_data, 1, w * h * 3, p);
      fclose(p);
    }
#endif
  
  im->mod.gamma = id->mod.gamma;
  im->mod.brightness = id->mod.brightness;
  im->mod.contrast = id->mod.contrast;
  im->rmod.gamma = id->rmod.gamma;
  im->rmod.brightness = id->rmod.brightness;
  im->rmod.contrast = id->rmod.contrast;
  im->gmod.gamma = id->gmod.gamma;
  im->gmod.brightness = id->gmod.brightness;
  im->gmod.contrast = id->gmod.contrast;
  im->bmod.gamma = id->bmod.gamma;
  im->bmod.brightness = id->bmod.brightness;
  im->bmod.contrast = id->bmod.contrast;
  im->filename = malloc(strlen(file) + 1);
  if (im->filename)
    strcpy(im->filename, file);
  if ((id->cache.on_image && im))
    _gdk_imlib_add_image(im, file);
  _gdk_imlib_calc_map_tables(im);
  return im;
}

gint
gdk_imlib_save_image_to_eim(GdkImlibImage * im, char *file)
{
  char                fil[4096];
  char               *iden;
  FILE               *f;
  int                 size;

  if ((!id) || (!im) || (!file))
    return 0;
  strncpy(fil, file, sizeof(fil));
  iden = g_SplitID(fil);
  if (!iden[0])
    iden = "default";
  f = fopen(fil, "w");
  if (!f)
    return 0;

  size = im->rgb_width * im->rgb_height * 3;
  fprintf(f, "EIM 1\n");
  fprintf(f, "IMAGE %i %s %i %i %i %i %i %i %i %i %i\n",
	  size,
	  iden,
	  im->rgb_width,
	  im->rgb_height,
	  im->shape_color.r,
	  im->shape_color.g,
	  im->shape_color.b,
	  im->border.left,
	  im->border.right,
	  im->border.top,
	  im->border.bottom);
  if (fwrite(im->rgb_data, size, 1, f) != 1)
    {
      fclose(f);
      return 0;
    }
  fclose(f);
  return 1;
}

gint
gdk_imlib_add_image_to_eim(GdkImlibImage * im, char *file)
{
  char                fil[4096];
  char               *iden;
  FILE               *f;
  int                 size;

  if ((!id) || (!im) || (!file))
    return 0;
  strncpy(fil, file, sizeof(fil));

  iden = g_SplitID(file);
  if (!iden[0])
    strcpy(iden, "default");

  f = fopen(fil, "a");
  if (!f)
    return 0;

  size = im->rgb_width * im->rgb_height * 3;
  fprintf(f, "IMAGE %i %s %i %i %i %i %i %i %i %i %i\n",
	  size,
	  iden,
	  im->rgb_width,
	  im->rgb_height,
	  im->shape_color.r,
	  im->shape_color.g,
	  im->shape_color.b,
	  im->border.left,
	  im->border.right,
	  im->border.top,
	  im->border.bottom);

  if (fwrite(im->rgb_data, size, 1, f) != 1)
    {
      fclose(f);
      return 0;
    }
  fclose(f);
  return 1;
}

gint
gdk_imlib_save_image_to_ppm(GdkImlibImage * im, char *file)
{
  FILE               *f;

  if ((!id) || (!im) || (!file))
    return 0;
  f = fopen(file, "w");
  if (!f)
    return 0;

  fprintf(f, "P6\n");
  fprintf(f, "%i %i\n255\n",
	  im->rgb_width,
	  im->rgb_height);
  if (fwrite(im->rgb_data, im->rgb_width * im->rgb_height * 3, 1, f) != 1)
    {
      fclose(f);
      return 0;
    }
  fclose(f);
  return 1;
}
