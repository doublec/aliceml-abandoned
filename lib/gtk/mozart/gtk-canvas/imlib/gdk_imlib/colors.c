#define _GNU_SOURCE
#include <config.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

static int
PaletteLUTGet(void)
{
  unsigned char      *retval;
  Atom                type_ret;
  unsigned long       bytes_after, num_ret;
  int                 format_ret;
  long                length;
  Atom                to_get;
  
  retval = NULL;
  length = 0x7fffffff;
  to_get = XInternAtom(id->x.disp, "_IMLIB_COLORMAP", False);
  XGetWindowProperty(id->x.disp, id->x.root, to_get, 0, length, False,
		     XA_CARDINAL, &type_ret, &format_ret, &num_ret,
		     &bytes_after, &retval);
  if ((retval) && (num_ret > 0) && (format_ret > 0))
    {
      if (format_ret == 8)
	{
	  int j, i, pnum;
	  
	  pnum = (int)(retval[0]);
	  j = 1;
	  if (pnum != id->num_colors)
	    {
	      XFree(retval);
	      return 0;
	    }
	  for (i = 0; i < id->num_colors; i++)
	    {
	      if (retval[j++] != ((unsigned char)id->palette[i].r))
		{
		  XFree(retval);
		  return 0;
		}
	      if (retval[j++] != ((unsigned char)id->palette[i].g))
		{
		  XFree(retval);
		  return 0;
		}
	      if (retval[j++] != ((unsigned char)id->palette[i].b))
		{
		  XFree(retval);
		  return 0;
		}
	      if (retval[j++] != ((unsigned char)id->palette[i].pixel))
		{
		  XFree(retval);
		  return 0;
		}
	    }
	  if (id->fast_rgb)
	    free(id->fast_rgb);
	  id->fast_rgb = malloc(sizeof(unsigned char) * 32 * 32 * 32);
	  for (i = 0; (i < (32 * 32 * 32)) && (j < num_ret); i++)
	    id->fast_rgb[i] = retval[j++];
	  XFree(retval);
	  return 1;
	}
      else
	XFree(retval);
    }
  return 0;
}

static void
PaletteLUTSet(void)
{
  Atom                to_set;
  unsigned char       *prop;
  int                 i, j;
  
  to_set = XInternAtom(id->x.disp, "_IMLIB_COLORMAP", False);  
  prop = malloc((id->num_colors * 4) + 1 + (32 * 32 * 32));
  prop[0] = id->num_colors;
  j = 1;
  for (i = 0; i < id->num_colors; i++)
    {
      prop[j++] = (unsigned char)id->palette[i].r;
      prop[j++] = (unsigned char)id->palette[i].g;
      prop[j++] = (unsigned char)id->palette[i].b;
      prop[j++] = (unsigned char)id->palette[i].pixel;
    }
  for (i = 0; i < (32 * 32 * 32); i++)
    prop[j++] = (unsigned char)id->fast_rgb[i];
  XChangeProperty(id->x.disp, id->x.root, to_set, XA_CARDINAL, 8,
		  PropModeReplace, (unsigned char *)prop, j);
  free(prop);
}

static void
PaletteAlloc(int num, const int *cols)
{
  XColor              xcl;
  int                 colnum, i, j;
  int                 r, g, b;
  int                 used[256], num_used, is_used;

  if (id->palette)
    free(id->palette);
  id->palette = malloc(sizeof(GdkImlibColor) * num);
  if (id->palette_orig)
    free(id->palette_orig);
  id->palette_orig = malloc(sizeof(GdkImlibColor) * num);
  num_used = 0;
  colnum = 0;
  for (i = 0; i < num; i++)
    {
      r = cols[(i * 3) + 0];
      g = cols[(i * 3) + 1];
      b = cols[(i * 3) + 2];
      xcl.red = (unsigned short)((r << 8) | (r));
      xcl.green = (unsigned short)((g << 8) | (g));
      xcl.blue = (unsigned short)((b << 8) | (b));
      xcl.flags = DoRed | DoGreen | DoBlue;
      XAllocColor(id->x.disp, id->x.root_cmap, &xcl);
      is_used = 0;
      for (j = 0; j < num_used; j++)
	{
	  if (xcl.pixel == used[j])
	    {
	      is_used = 1;
	      j = num_used;
	    }
	}
      if (!is_used)
	{
	  id->palette[colnum].r = xcl.red >> 8;
	  id->palette[colnum].g = xcl.green >> 8;
	  id->palette[colnum].b = xcl.blue >> 8;
	  id->palette[colnum].pixel = xcl.pixel;
	  used[num_used++] = xcl.pixel;
	  colnum++;
	}
      else
	xcl.pixel = 0;
      id->palette_orig[i].r = r;
      id->palette_orig[i].g = g;
      id->palette_orig[i].b = b;
      id->palette_orig[i].pixel = xcl.pixel;
    }
  id->num_colors = colnum;
}

static void
alloc_colors (const int *pal, int i)
{
  int r, g, b;
  int rr, gg, bb;

  XGrabServer(id->x.disp);
  PaletteAlloc((i / 3), pal);
  if (!PaletteLUTGet())
    {
      if (id->fast_rgb)
	free(id->fast_rgb);
      id->fast_rgb = malloc(sizeof(unsigned char) * 32 * 32 * 32);
      
      for (r = 0; r < 32; r++)
	{
	  for (g = 0; g < 32; g++)
	    {
	      for (b = 0; b < 32; b++)
		{
		  rr = (r << 3) | (r >> 2);
		  gg = (g << 3) | (g >> 2);
		  bb = (b << 3) | (b >> 2);
		  INDEX_RGB(r, g, b) = _gdk_imlib_index_best_color_match(&rr, &gg, &bb);
		}
	    }
	}
      PaletteLUTSet();
    }
  XUngrabServer(id->x.disp);
}

gint
gdk_imlib_load_colors(char *file)
{
  FILE               *f;
  char                s[256];
  int                 i;
  int                 pal[768];
  int                 r, g, b;

  f = fopen(file, "r");
  if (!f)
      return 0;

  i = 0;
  while (fgets(s, 256, f))
    {
      if (s[0] == '0')
	{
	  sscanf(s, "%x %x %x", &r, &g, &b);
	  if (r < 0)
	    r = 0;
	  if (r > 255)
	    r = 255;
	  if (g < 0)
	    g = 0;
	  if (g > 255)
	    g = 255;
	  if (b < 0)
	    b = 0;
	  if (b > 255)
	    b = 255;
	  pal[i++] = r;
	  pal[i++] = g;
	  pal[i++] = b;
	}
      if (i >= 768)
	break;
    }
  fclose(f);

  alloc_colors (pal, i);
  return 1;
}

void
gdk_imlib_load_default_colors__private (void)
{
  static const int default_pal[] = {
    0x0, 0x0, 0x0,
    0xff, 0xff, 0xff,
    0xff, 0x0, 0x0,
    0xff, 0xff, 0x0,
    0x0, 0xff, 0x0,
    0x0, 0x0, 0xff,
    0x0, 0xff, 0xff,
    0x99, 0x99, 0x99,
    0xff, 0x88, 0x0,
    0x88, 0x0, 0x0,
    0x0, 0x88, 0x88,
    0x88, 0x88, 0x0,
    0xff, 0xcc, 0x97,
    0xbb, 0xbb, 0xbb,
    0x9f, 0x6b, 0x42,
    0x55, 0x55, 0x55,
    0xdd, 0xdd, 0xdd,
    0x77, 0x77, 0x77,
    0x33, 0x33, 0x33,
    0xcc, 0x0, 0x0,
    0xff, 0x44, 0x0,
    0xff, 0xcc, 0x0,
    0xcc, 0xcc, 0x0,
    0x60, 0x60, 0x0,
    0x0, 0x43, 0x0,
    0x0, 0x7f, 0x0,
    0x0, 0xcc, 0x0,
    0x0, 0x44, 0x44,
    0x0, 0x0, 0x44,
    0x0, 0x0, 0x88,
    0xef, 0xb1, 0x7b,
    0xdf, 0x98, 0x5f,
    0xbf, 0x87, 0x56,
    0x7f, 0x57, 0x26,
    0x5f, 0x39, 0xc,
    0x3f, 0x1c, 0x0,
    0x21, 0x0, 0x0,
    0x0, 0x43, 0x87,
    0x2d, 0x70, 0xaf,
    0x5a, 0x9e, 0xd7,
    0x87, 0xcc, 0xff,
    0xff, 0xe0, 0xba,
    0x21, 0x43, 0xf,
    0x3d, 0x5d, 0x25,
    0x59, 0x78, 0x3a,
    0x75, 0x93, 0x4f,
    0x91, 0xae, 0x64,
    0xad, 0xc8, 0x7a,
    0xf0, 0xa8, 0xef,
    0xd0, 0x88, 0xd0,
    0xaf, 0x66, 0xaf,
    0x8e, 0x44, 0x8e,
    0x6d, 0x22, 0x6d,
    0x4b, 0x0, 0x4b,
    0xff, 0xc0, 0xbc,
    0xff, 0x93, 0x91,
    0xff, 0x66, 0x67,
    0xd8, 0xf2, 0xbf,
    0xff, 0xc9, 0x68,
    0xff, 0x96, 0x67,
    0xa5, 0x60, 0xff,
    0x51, 0xff, 0x99,
    0x3f, 0xa5, 0x63,
    0x98, 0x90, 0x67
  };

  alloc_colors (default_pal, sizeof (default_pal) / sizeof (default_pal[0]));
}

void
gdk_imlib_free_colors()
{
  int                 i;
  unsigned long       pixels[256];

  for (i = 0; i < id->num_colors; i++)
    pixels[i] = id->palette[i].pixel;
  XFreeColors(id->x.disp, id->x.root_cmap, pixels, id->num_colors, 0);
  id->num_colors = 0;
}

void
gdk_imlib_best_color_get(GdkColor * c)
{
  int                 r, g, b, rr, gg, bb;

  rr = r = c->red >> 8;
  gg = g = c->green >> 8;
  bb = b = c->blue >> 8;
  c->pixel = gdk_imlib_best_color_match(&r, &g, &b);
  rr = rr - r;
  gg = gg - g;
  bb = bb - b;
  if (rr > 0xff)
    rr = 0xff;
  if (gg > 0xff)
    gg = 0xff;
  if (bb > 0xff)
    bb = 0xff;
  c->red = (rr << 8) | rr;
  c->green = (gg << 8) | gg;
  c->blue = (bb << 8) | bb;
}
