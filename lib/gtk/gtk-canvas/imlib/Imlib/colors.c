#define _GNU_SOURCE
#include <config.h>
#include "Imlib.h"
#include "Imlib_private.h"

static int PaletteLUTGet(ImlibData *id);
static void PaletteLUTSet(ImlibData *id);

static int
PaletteLUTGet(ImlibData *id)
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
PaletteLUTSet(ImlibData *id)
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

void
_PaletteAlloc(ImlibData * id, int num, int *cols)
{
  XColor              xcl;
  int                 colnum, i, j;
  int                 r, g, b;
  int                 used[256], num_used, is_used;

  if (id->palette)
    free(id->palette);
  id->palette = malloc(sizeof(ImlibColor) * num);
  if (id->palette_orig)
    free(id->palette_orig);
  id->palette_orig = malloc(sizeof(ImlibColor) * num);
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

int
Imlib_load_colors(ImlibData * id, char *file)
{
  FILE               *f;
  char                s[256];
  int                 i;
  int                 pal[768];
  int                 r, g, b;
  int                 rr, gg, bb;

  f = fopen(file, "r");
  if (!f)
    {
      fprintf(stderr, "ImLib ERROR: Cannot find palette file %s\n", file);
      return 0;
    }
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
  XGrabServer(id->x.disp);
  _PaletteAlloc(id, (i / 3), pal);
  if (!PaletteLUTGet(id))
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
		  INDEX_RGB(r, g, b) = index_best_color_match(id, &rr, &gg, &bb);
		}
	    }
	}
      PaletteLUTSet(id);
    }
  XUngrabServer(id->x.disp);
  return 1;
}

void
Imlib_free_colors(ImlibData * id)
{
  int                 i;
  unsigned long       pixels[256];

  for (i = 0; i < id->num_colors; i++)
    pixels[i] = id->palette[i].pixel;
  XFreeColors(id->x.disp, id->x.root_cmap, pixels, id->num_colors, 0);
  id->num_colors = 0;
}
