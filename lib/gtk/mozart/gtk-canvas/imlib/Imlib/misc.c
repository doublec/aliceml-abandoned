#define _GNU_SOURCE
#include <config.h>
#include "Imlib.h"
#include "Imlib_private.h"
#include <locale.h>

#ifndef HAVE_SNPRINTF
#define snprintf my_snprintf
#ifdef HAVE_STDARGS
int                 my_snprintf(char *str, size_t count, const char *fmt,...);

#else
int                 my_snprintf(va_alist);

#endif
#endif

char                x_error;

static void
HandleXError(Display * d, XErrorEvent * ev)
{
  x_error = 1;
}

int
Imlib_get_render_type(ImlibData * id)
{
  if (id)
    return id->render_type;
  else
    {
      fprintf(stderr, "IMLIB ERROR: Imlib not initialised\n");
      return -1;
    }
}

void
Imlib_set_render_type(ImlibData * id, int rend_type)
{
  if (id)
    {
      if (id->x.depth > 8)
	id->render_type = rend_type;
      else
	{
	  if ((rend_type == RT_PLAIN_TRUECOL) ||
	      (rend_type == RT_DITHER_TRUECOL))
	    id->render_type = RT_DITHER_PALETTE_FAST;
	  else
	    id->render_type = rend_type;
	}
      return;
    }
  else
    {
      fprintf(stderr, "IMLIB ERROR: Imlib not initialised\n");
      return;
    }
}

#ifdef HAVE_SHM
int
                    XShmGetEventBase(Display * disp);

#endif

ImlibData          *
Imlib_init(Display * disp)
{
  ImlibData          *id;
  XWindowAttributes   xwa;
  XVisualInfo         xvi, *xvir;
  char               *homedir;
  char                s[4096], *s1, *s2;
  FILE               *f;
  int                 override = 0;
  int                 dither = 0;
  int                 remap = 1;
  int                 num;
  int                 i, max, maxn;
  int                 clas;
  char               *palfile;
  int                 loadpal;
  int                 vis;
  int                 newcm;
  char               *old_locale;

  /* fprintf(stderr, "Imlib Init\n");
  fflush(stderr); */

  palfile = NULL;
  if (!disp)
    {
      fprintf(stderr, "IMLIB ERROR: no display\n");
      return NULL;
    }
  vis = -1;
  loadpal = 0;
  id = (ImlibData *) malloc(sizeof(ImlibData));
  if (!id)
    {
      fprintf(stderr, "IMLIB ERROR: Cannot alloc RAM for Initial data struct\n");
      return NULL;
    }
  id->palette = NULL;
  id->palette_orig = NULL;
  id->fast_rgb = NULL;
  id->fast_err = NULL;
  id->fast_erg = NULL;
  id->fast_erb = NULL;
  id->x.disp = disp;
  id->x.screen = DefaultScreen(disp);	/* the screen number */
  id->x.root = DefaultRootWindow(disp);		/* the root window id */
  id->x.visual = DefaultVisual(disp, id->x.screen);	/* the visual type */
  id->x.depth = DefaultDepth(disp, id->x.screen);	/* the depth of the screen in bpp */

  id->x.shm = 0;
  id->x.shmp = 0;
  id->max_shm = 0;
#ifdef HAVE_SHM
  if (XShmQueryExtension(id->x.disp))
    {
      int                 maj, min, dum;
      Bool                pm;

      if (XQueryExtension(id->x.disp, "MIT-SHM", &dum, &dum, &dum))
	{
	  if (XShmQueryVersion(id->x.disp, &maj, &min, &pm) == True)
	    {
	      id->x.shm = 1;
	      id->x.shm_event = XShmGetEventBase(id->x.disp) + ShmCompletion;
	      id->x.last_xim = NULL;
	      id->x.last_sxim = NULL;
	      id->max_shm = 0x7fffffff;
	      if ((XShmPixmapFormat(id->x.disp) == ZPixmap) &&
		  (pm == True))
		id->x.shmp = 1;
	    }
	}
    }
#endif
  id->cache.on_image = 0;
  id->cache.size_image = 0;
  id->cache.num_image = 0;
  id->cache.used_image = 0;
  id->cache.image = NULL;
  id->cache.on_pixmap = 0;
  id->cache.size_pixmap = 0;
  id->cache.num_pixmap = 0;
  id->cache.used_pixmap = 0;
  id->cache.pixmap = NULL;
  id->byte_order = 0;
  id->fastrend = 0;
  id->hiq = 0;
  id->fallback = 1;
  id->mod.gamma = 256;
  id->mod.brightness = 256;
  id->mod.contrast = 256;
  id->rmod.gamma = 256;
  id->rmod.brightness = 256;
  id->rmod.contrast = 256;
  id->gmod.gamma = 256;
  id->gmod.brightness = 256;
  id->gmod.contrast = 256;
  id->bmod.gamma = 256;
  id->bmod.brightness = 256;
  id->bmod.contrast = 256;
  id->ordered_dither = 1;

  if (XGetWindowAttributes(disp, id->x.root, &xwa))
    {
      if (xwa.colormap)
	id->x.root_cmap = xwa.colormap;
      else
	id->x.root_cmap = 0;
    }
  else
    id->x.root_cmap = 0;
  id->num_colors = 0;
  homedir = getenv("HOME");
  snprintf(s, sizeof(s), "%s/.imrc", homedir);

  old_locale = strdup(setlocale(LC_NUMERIC, NULL));
  setlocale(LC_NUMERIC, "C");
  f = fopen(s, "r");
  if (!f)
    f = fopen(SYSTEM_IMRC, "r");
  if (f)
    {
      while (fgets(s, 4096, f))
	{
	  if (s[0] == '#')
	    continue;

	  s1 = strtok(s, " \t\n");

	  /* Blank line ? */

	  if (s1 == NULL)
	    continue;

	  s2 = strtok(NULL, " \t\n");
	  if (s2 == NULL)
	    s2 = "";		/* NULL argument */

	  if (!strcasecmp("PaletteFile", s1))
	    {
	      palfile = strdup(s2);
	    }
	  else if (!strcasecmp("PaletteOverride", s1))
	    {
	      if (!strcasecmp("yes", s2))
		override = 1;
	      else
		override = 0;
	    }
	  else if (!strcasecmp("Dither", s1))
	    {
	      if (!strcasecmp("yes", s2))
		dither = 1;
	      else
		dither = 0;
	    }
	  else if (!strcasecmp("Remap", s1))
	    {
	      if (!strcasecmp("fast", s2))
		remap = 1;
	      else
		remap = 0;
	    }
	  else if (!strcasecmp("Mit-Shm", s1))
	    {
#ifdef HAVE_SHM
	      if (!strcasecmp("off", s2))
#endif
		{
		  id->x.shm = 0;
		  id->x.shmp = 0;
		}
	    }
	  else if (!strcasecmp("SharedPixmaps", s1))
	    {
#ifdef HAVE_SHM
	      if (!strcasecmp("off", s2))
#endif
		id->x.shmp = 0;
	    }
	  else if (!strcasecmp("FastRender", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->fastrend = 1;
	    }
	  else if (!strcasecmp("HighQuality", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->hiq = 1;
	    }
	  else if (!strcasecmp("Shm_Max_Size", s1))
	    {
	      num = atoi(s2);
	      id->max_shm = num;
	    }
	  else if (!strcasecmp("Image_Cache_Size", s1))
	    {
	      num = atoi(s2);
	      id->cache.size_image = num;
	    }
	  else if (!strcasecmp("Pixmap_Cache_Size", s1))
	    {
	      num = atoi(s2);
	      id->cache.size_pixmap = num;
	    }
	  else if (!strcasecmp("Image_Cache", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->cache.on_image = 1;
	    }
	  else if (!strcasecmp("Pixmap_Cache", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->cache.on_pixmap = 1;
	    }
	  else if (!strcasecmp("ForceVisualID", s1))
	    {
	      sscanf(s, "%1024s %x", s1, &num);
	      vis = num;
	    }
	  else if (!strcasecmp("Fallback", s1))
	    {
	      if (!strcasecmp("off", s2))
		id->fallback = 0;
	      else
		id->fallback = 1;
	    }
	  else if (!strcasecmp("Gamma", s1))
	    {
	      id->mod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Brightness", s1))
	    {
	      id->mod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Contrast", s1))
	    {
	      id->mod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Gamma", s1))
	    {
	      id->rmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Brightness", s1))
	    {
	      id->rmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Contrast", s1))
	    {
	      id->rmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Gamma", s1))
	    {
	      id->gmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Brightness", s1))
	    {
	      id->gmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Contrast", s1))
	    {
	      id->gmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Gamma", s1))
	    {
	      id->bmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Brightness", s1))
	    {
	      id->bmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Contrast", s1))
	    {
	      id->bmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Ordered_Dither", s1))
	    {
	      if (!strcasecmp("off", s2))
		id->ordered_dither = 0;
	      else
		id->ordered_dither = 1;
	    }
	}
      fclose(f);
    }
  setlocale(LC_NUMERIC, old_locale);
  if (old_locale)
    free(old_locale);
  
  /* list all visuals for the default screen */
  xvi.screen = id->x.screen;
  xvir = XGetVisualInfo(disp, VisualScreenMask, &xvi, &num);
  if (vis >= 0)
    {
      /* use the forced visual id */
      maxn = 0;
      for (i = 0; i < num; i++)
	{
	  if (xvir[i].visualid == (VisualID) vis)
	    maxn = i;
	}
      if (maxn >= 0)
	{
	  unsigned long       rmsk, gmsk, bmsk;

	  id->x.depth = xvir[maxn].depth;
	  id->x.visual = xvir[maxn].visual;
	  rmsk = xvir[maxn].red_mask;
	  gmsk = xvir[maxn].green_mask;
	  bmsk = xvir[maxn].blue_mask;

	  if ((rmsk > gmsk) && (gmsk > bmsk))
	    id->byte_order = BYTE_ORD_24_RGB;
	  else if ((rmsk > bmsk) && (bmsk > gmsk))
	    id->byte_order = BYTE_ORD_24_RBG;
	  else if ((bmsk > rmsk) && (rmsk > gmsk))
	    id->byte_order = BYTE_ORD_24_BRG;
	  else if ((bmsk > gmsk) && (gmsk > rmsk))
	    id->byte_order = BYTE_ORD_24_BGR;
	  else if ((gmsk > rmsk) && (rmsk > bmsk))
	    id->byte_order = BYTE_ORD_24_GRB;
	  else if ((gmsk > bmsk) && (bmsk > rmsk))
	    id->byte_order = BYTE_ORD_24_GBR;
	  else
	    id->byte_order = 0;
	}
      else
	fprintf(stderr, "IMLIB ERROR: Visual Id no 0x%x specified in the imrc file is invalid on this display.\nUsing Default Visual.\n", vis);
    }
  else
    {
      if (xvir)
	{
	  /* find the highest bit-depth supported by visuals */
	  max = 0;
	  for (i = 0; i < num; i++)
	    {
	      if (xvir[i].depth > max)
		max = xvir[i].depth;
	    }
	  if (max > 8)
	    {
	      id->x.depth = max;
	      clas = -1;
	      maxn = -1;
	      for (i = 0; i < num; i++)
		{
		  if (xvir[i].depth == id->x.depth)
		    {
		      if ((xvir[i].class > clas) && (xvir[i].class != DirectColor))
			{
			  maxn = i;
			  clas = xvir[i].class;
			}
		    }
		}
	      if (maxn >= 0)
		{
		  unsigned long       rmsk, gmsk, bmsk;

		  id->x.visual = xvir[maxn].visual;
		  rmsk = xvir[maxn].red_mask;
		  gmsk = xvir[maxn].green_mask;
		  bmsk = xvir[maxn].blue_mask;

		  if ((rmsk > gmsk) && (gmsk > bmsk))
		    id->byte_order = BYTE_ORD_24_RGB;
		  else if ((rmsk > bmsk) && (bmsk > gmsk))
		    id->byte_order = BYTE_ORD_24_RBG;
		  else if ((bmsk > rmsk) && (rmsk > gmsk))
		    id->byte_order = BYTE_ORD_24_BRG;
		  else if ((bmsk > gmsk) && (gmsk > rmsk))
		    id->byte_order = BYTE_ORD_24_BGR;
		  else if ((gmsk > rmsk) && (rmsk > bmsk))
		    id->byte_order = BYTE_ORD_24_GRB;
		  else if ((gmsk > bmsk) && (bmsk > rmsk))
		    id->byte_order = BYTE_ORD_24_GBR;
		  else
		    id->byte_order = 0;
		}
	    }
	}
    }
  id->x.render_depth = id->x.depth;
  XFree(xvir);
  if (id->x.depth == 16)
    {
      xvi.visual = id->x.visual;
      xvi.visualid = XVisualIDFromVisual(id->x.visual);
      xvir = XGetVisualInfo(disp, VisualIDMask, &xvi, &num);
      if (xvir)
	{
	  if (xvir->red_mask != 0xf800)
	    id->x.render_depth = 15;
	  XFree(xvir);
	}
    }
  if (id->x.depth < 8)
    id->x.shmp = 0;
  if ((id->x.depth <= 8) || (override == 1))
    loadpal = 1;
  if (loadpal)
    {
      if (dither == 1)
	{
	  if (remap == 1)
	    id->render_type = RT_DITHER_PALETTE_FAST;
	  else
	    id->render_type = RT_DITHER_PALETTE;
	}
      else
	{
	  if (remap == 1)
	    id->render_type = RT_PLAIN_PALETTE_FAST;
	  else
	    id->render_type = RT_PLAIN_PALETTE;
	}
      if (palfile != NULL)
	Imlib_load_colors(id, palfile);
      if (id->num_colors == 0)
	{
	  fprintf(stderr, "IMLIB ERROR: Cannot Find Palette. A Palette is required for this mode\n");
	  free(id);
	  if (palfile)
	    free(palfile);
	  return NULL;
	}
    }
  else
    {
      if (id->hiq == 1)
	id->render_type = RT_DITHER_TRUECOL;
      else
	id->render_type = RT_PLAIN_TRUECOL;
    }
  {
    XSetWindowAttributes at;
    unsigned long       mask;

    at.border_pixel = 0;
    at.backing_store = NotUseful;
    at.background_pixel = 0;
    at.save_under = False;
    at.override_redirect = True;
    mask = CWOverrideRedirect | CWBackPixel | CWBorderPixel |
      CWBackingStore | CWSaveUnder;
    newcm = 0;
    if (id->x.visual != DefaultVisual(disp, id->x.screen))
      {
	Colormap            cm;

	cm = XCreateColormap(id->x.disp, id->x.root,
			     id->x.visual, AllocNone);
	if (cm)
	  {
	    mask |= CWColormap;
	    id->x.root_cmap = cm;
	    at.colormap = cm;
	    newcm = 1;
	  }
      }
    id->x.base_window = XCreateWindow(id->x.disp, id->x.root,
				      -100, -100, 10, 10, 0,
				      id->x.depth, InputOutput,
				      id->x.visual, mask, &at);
  }
  {
    /* Turn off fastrender if there is an endianess diff between */
    /* client and Xserver */
    int                 byt, bit;

    byt = ImageByteOrder(id->x.disp);	/* LSBFirst | MSBFirst */
    bit = BitmapBitOrder(id->x.disp);	/* LSBFirst | MSBFirst */
    id->x.byte_order = byt;
    id->x.bit_order = bit;
    /* if little endian && server big */
    if ((htonl(1) != 1) && (byt == MSBFirst))
      id->fastrend = 0;
    /* if big endian && server little */
    if ((htonl(1) == 1) && (byt == LSBFirst))
      id->fastrend = 0;
  }
  if (palfile)
    free(palfile);

#ifdef HAVE_SHM
  if (id->x.shm)
    {
      XImage             *xim;

      xim = XShmCreateImage(id->x.disp, id->x.visual, id->x.depth,
			    ZPixmap, NULL, &id->x.last_shminfo, 10, 10);
      if (!xim)
	{
	  id->x.shm = 0;
	  id->x.shmp = 0;
	}
      else
	{
	  id->x.last_shminfo.shmid =
	    shmget(IPC_PRIVATE, xim->bytes_per_line * xim->height,
		   IPC_CREAT | 0777);
	  if (id->x.last_shminfo.shmid < 0)
	    {
	      XDestroyImage(xim);
	      id->x.shm = 0;
	      id->x.shmp = 0;
	    }
	  else
	    {
	      id->x.last_shminfo.shmaddr = xim->data =
		shmat(id->x.last_shminfo.shmid, 0, 0);
	      if (id->x.last_shminfo.shmaddr == (char *)-1)
		{
		  XDestroyImage(xim);
		  shmctl(id->x.last_shminfo.shmid, IPC_RMID, 0);
		  id->x.shm = 0;
		  id->x.shmp = 0;
		}
	      else
		{
		  id->x.last_shminfo.readOnly = False;
		  XSetErrorHandler((XErrorHandler) HandleXError);
		  x_error = 0;
		  XShmAttach(id->x.disp, &id->x.last_shminfo);
		  XSync(disp, False);
		  if (x_error)
		    {
		      id->x.shm = 0;
		      id->x.shmp = 0;
		    }
		  else
		    XShmDetach(id->x.disp, &id->x.last_shminfo);
		  XDestroyImage(xim);
		  shmdt(id->x.last_shminfo.shmaddr);
		  shmctl(id->x.last_shminfo.shmid, IPC_RMID, 0);
		}
	    }
	}
    }
#endif

  return id;
}

ImlibData          *
Imlib_init_with_params(Display * disp, ImlibInitParams * p)
{
  ImlibData          *id;
  XWindowAttributes   xwa;
  XVisualInfo         xvi, *xvir;
  char               *homedir;
  char                s[4096], *s1, *s2;
  FILE               *f;
  int                 override = 0;
  int                 dither = 0;
  int                 remap = 1;
  int                 num;
  int                 i, max, maxn;
  int                 clas;
  char               *palfile;
  int                 loadpal;
  int                 vis;
  int                 newcm = 0;
  char               *old_locale;

  palfile = NULL;
  if (!disp)
    {
      fprintf(stderr, "IMLIB ERROR: no display\n");
      return NULL;
    }
  vis = -1;
  loadpal = 0;
  id = (ImlibData *) malloc(sizeof(ImlibData));
  if (!id)
    {
      fprintf(stderr, "IMLIB ERROR: Cannot alloc RAM for Initial data struct\n");
      return NULL;
    }
  id->palette = NULL;
  id->palette_orig = NULL;
  id->fast_rgb = NULL;
  id->fast_err = NULL;
  id->fast_erg = NULL;
  id->fast_erb = NULL;
  id->x.disp = disp;
  id->x.screen = DefaultScreen(disp);	/* the screen number */
  id->x.root = DefaultRootWindow(disp);		/* the root window id */
  id->x.visual = DefaultVisual(disp, id->x.screen);	/* the visual type */
  id->x.depth = DefaultDepth(disp, id->x.screen);	/* the depth of the screen in bpp */
#ifdef HAVE_SHM
  if (XShmQueryExtension(id->x.disp))
    {
      int                 maj, min, dum;
      Bool                pm;

      if (XQueryExtension(id->x.disp, "MIT-SHM", &dum, &dum, &dum))
	{
	  if (XShmQueryVersion(id->x.disp, &maj, &min, &pm) == True)
	    {
	      id->x.shm = 1;
	      id->x.shm_event = XShmGetEventBase(id->x.disp) + ShmCompletion;
	      id->x.last_xim = NULL;
	      id->x.last_sxim = NULL;
	      id->max_shm = 0x7fffffff;
	      if (XShmPixmapFormat(id->x.disp) == ZPixmap)
		id->x.shmp = 1;
	    }
	}
    }
  else
#endif
    {
      id->x.shm = 0;
      id->x.shmp = 0;
    }
  id->cache.on_image = 0;
  id->cache.size_image = 0;
  id->cache.num_image = 0;
  id->cache.used_image = 0;
  id->cache.image = NULL;
  id->cache.on_pixmap = 0;
  id->cache.size_pixmap = 0;
  id->cache.num_pixmap = 0;
  id->cache.used_pixmap = 0;
  id->cache.pixmap = NULL;
  id->byte_order = 0;
  id->fastrend = 0;
  id->hiq = 0;
  id->fallback = 1;
  id->mod.gamma = 256;
  id->mod.brightness = 256;
  id->mod.contrast = 256;
  id->rmod.gamma = 256;
  id->rmod.brightness = 256;
  id->rmod.contrast = 256;
  id->gmod.gamma = 256;
  id->gmod.brightness = 256;
  id->gmod.contrast = 256;
  id->bmod.gamma = 256;
  id->bmod.brightness = 256;
  id->bmod.contrast = 256;
  id->ordered_dither = 1;

  if (XGetWindowAttributes(disp, id->x.root, &xwa))
    {
      if (xwa.colormap)
	id->x.root_cmap = xwa.colormap;
      else
	id->x.root_cmap = 0;
    }
  else
    id->x.root_cmap = 0;
  id->num_colors = 0;
  homedir = getenv("HOME");
  snprintf(s, sizeof(s), "%s/.imrc", homedir);
  old_locale = strdup(setlocale(LC_NUMERIC, NULL));
  setlocale(LC_NUMERIC, "C");
  f = fopen(s, "r");
  if (!f)
    f = fopen(SYSTEM_IMRC, "r");
  if (f)
    {
      while (fgets(s, 4096, f))
	{
	  if (s[0] == '#')
	    continue;

	  s1 = strtok(s, " \t\n");

	  /* Blank line ? */

	  if (s1 == NULL)
	    continue;

	  s2 = strtok(NULL, " \t\n");
	  if (s2 == NULL)
	    s2 = "";		/* NULL argument */

	  if (!strcasecmp("PaletteFile", s1))
	    {
	      palfile = strdup(s2);
	    }
	  else if (!strcasecmp("PaletteOverride", s1))
	    {
	      if (!strcasecmp("yes", s2))
		override = 1;
	      else
		override = 0;
	    }
	  else if (!strcasecmp("Dither", s1))
	    {
	      if (!strcasecmp("yes", s2))
		dither = 1;
	      else
		dither = 0;
	    }
	  else if (!strcasecmp("Remap", s1))
	    {
	      if (!strcasecmp("fast", s2))
		remap = 1;
	      else
		remap = 0;
	    }
	  else if (!strcasecmp("Mit-Shm", s1))
	    {
#ifdef HAVE_SHM
	      if (!strcasecmp("off", s2))
#endif
		{
		  id->x.shm = 0;
		  id->x.shmp = 0;
		}
	    }
	  else if (!strcasecmp("SharedPixmaps", s1))
	    {
#ifdef HAVE_SHM
	      if (!strcasecmp("off", s2))
#endif
		id->x.shmp = 0;
	    }
	  else if (!strcasecmp("FastRender", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->fastrend = 1;
	    }
	  else if (!strcasecmp("HighQuality", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->hiq = 1;
	    }
	  else if (!strcasecmp("Shm_Max_Size", s1))
	    {
	      num = atoi(s2);
	      id->max_shm = num;
	    }
	  else if (!strcasecmp("Image_Cache_Size", s1))
	    {
	      num = atoi(s2);
	      id->cache.size_image = num;
	    }
	  else if (!strcasecmp("Pixmap_Cache_Size", s1))
	    {
	      num = atoi(s2);
	      id->cache.size_pixmap = num;
	    }
	  else if (!strcasecmp("Image_Cache", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->cache.on_image = 1;
	    }
	  else if (!strcasecmp("Pixmap_Cache", s1))
	    {
	      if (!strcasecmp("on", s2))
		id->cache.on_pixmap = 1;
	    }
	  else if (!strcasecmp("ForceVisualID", s1))
	    {
	      sscanf(s, "%1024s %x", s1, &num);
	      vis = num;
	    }
	  else if (!strcasecmp("Fallback", s1))
	    {
	      if (!strcasecmp("off", s2))
		id->fallback = 0;
	      else
		id->fallback = 1;
	    }
	  else if (!strcasecmp("Gamma", s1))
	    {
	      id->mod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Brightness", s1))
	    {
	      id->mod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Contrast", s1))
	    {
	      id->mod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Gamma", s1))
	    {
	      id->rmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Brightness", s1))
	    {
	      id->rmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Red_Contrast", s1))
	    {
	      id->rmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Gamma", s1))
	    {
	      id->gmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Brightness", s1))
	    {
	      id->gmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Green_Contrast", s1))
	    {
	      id->gmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Gamma", s1))
	    {
	      id->bmod.gamma = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Brightness", s1))
	    {
	      id->bmod.brightness = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Blue_Contrast", s1))
	    {
	      id->bmod.contrast = (int)(256.0 * atof(s2));
	    }
	  else if (!strcasecmp("Ordered_Dither", s1))
	    {
	      if (!strcasecmp("off", s2))
		id->ordered_dither = 0;
	      else
		id->ordered_dither = 1;
	    }
	}
      fclose(f);
    }
  setlocale(LC_NUMERIC, old_locale);
  if (old_locale)
    free(old_locale);
  
  if (p)
    {
      if (p->flags & PARAMS_VISUALID)
	vis = p->visualid;
      if (p->flags & PARAMS_PALETTEFILE)
	strcpy(palfile, p->palettefile);
      if (p->flags & PARAMS_SHAREDMEM)
	{
	  if (!p->sharedmem)
	    {
	      id->x.shm = 0;
	      id->x.shmp = 0;
	    }
	  else
	    {
	      id->x.shm = 1;
	      id->x.shmp = 0;
	    }
	}
      if (p->flags & PARAMS_SHAREDPIXMAPS)
	{
	  if (id->x.shm)
	    id->x.shmp = p->sharedpixmaps;
	}
      if (p->flags & PARAMS_PALETTEOVERRIDE)
	override = p->paletteoverride;
      if (p->flags & PARAMS_REMAP)
	remap = p->remap;
      if (p->flags & PARAMS_FASTRENDER)
	id->fastrend = p->fastrender;
      if (p->flags & PARAMS_HIQUALITY)
	id->hiq = p->hiquality;
      if (p->flags & PARAMS_DITHER)
	dither = p->dither;
      if (p->flags & PARAMS_IMAGECACHESIZE)
	id->cache.size_image = p->imagecachesize;
      if (p->flags & PARAMS_PIXMAPCACHESIZE)
	id->cache.size_pixmap = p->pixmapcachesize;
      if (p->flags & PARAMS_COLORMAP)
	{
	  id->x.root_cmap = p->cmap;
	  newcm = 1;
	}
    }
  /* list all visuals for the default screen */
  xvi.screen = id->x.screen;
  xvir = XGetVisualInfo(disp, VisualScreenMask, &xvi, &num);
  if (vis >= 0)
    {
      /* use the forced visual id */
      maxn = 0;
      for (i = 0; i < num; i++)
	{
	  if (xvir[i].visualid == (VisualID) vis)
	    maxn = i;
	}
      if (maxn >= 0)
	{
	  unsigned long       rmsk, gmsk, bmsk;

	  id->x.depth = xvir[maxn].depth;
	  id->x.visual = xvir[maxn].visual;
	  rmsk = xvir[maxn].red_mask;
	  gmsk = xvir[maxn].green_mask;
	  bmsk = xvir[maxn].blue_mask;

	  if ((rmsk > gmsk) && (gmsk > bmsk))
	    id->byte_order = BYTE_ORD_24_RGB;
	  else if ((rmsk > bmsk) && (bmsk > gmsk))
	    id->byte_order = BYTE_ORD_24_RBG;
	  else if ((bmsk > rmsk) && (rmsk > gmsk))
	    id->byte_order = BYTE_ORD_24_BRG;
	  else if ((bmsk > gmsk) && (gmsk > rmsk))
	    id->byte_order = BYTE_ORD_24_BGR;
	  else if ((gmsk > rmsk) && (rmsk > bmsk))
	    id->byte_order = BYTE_ORD_24_GRB;
	  else if ((gmsk > bmsk) && (bmsk > rmsk))
	    id->byte_order = BYTE_ORD_24_GBR;
	  else
	    id->byte_order = 0;
	}
      else
	fprintf(stderr, "IMLIB ERROR: Visual Id no 0x%x specified in the imrc file is invalid on this display.\nUsing Default Visual.\n", vis);
    }
  else
    {
      if (xvir)
	{
	  /* find the highest bit-depth supported by visuals */
	  max = 0;
	  for (i = 0; i < num; i++)
	    {
	      if (xvir[i].depth > max)
		max = xvir[i].depth;
	    }
	  if (max > 8)
	    {
	      id->x.depth = max;
	      clas = -1;
	      maxn = -1;
	      for (i = 0; i < num; i++)
		{
		  if (xvir[i].depth == id->x.depth)
		    {
		      if ((xvir[i].class > clas) && (xvir[i].class != DirectColor))
			{
			  maxn = i;
			  clas = xvir[i].class;
			}
		    }
		}
	      if (maxn >= 0)
		{
		  unsigned long       rmsk, gmsk, bmsk;

		  id->x.visual = xvir[maxn].visual;
		  rmsk = xvir[maxn].red_mask;
		  gmsk = xvir[maxn].green_mask;
		  bmsk = xvir[maxn].blue_mask;

		  if ((rmsk > gmsk) && (gmsk > bmsk))
		    id->byte_order = BYTE_ORD_24_RGB;
		  else if ((rmsk > bmsk) && (bmsk > gmsk))
		    id->byte_order = BYTE_ORD_24_RBG;
		  else if ((bmsk > rmsk) && (rmsk > gmsk))
		    id->byte_order = BYTE_ORD_24_BRG;
		  else if ((bmsk > gmsk) && (gmsk > rmsk))
		    id->byte_order = BYTE_ORD_24_BGR;
		  else if ((gmsk > rmsk) && (rmsk > bmsk))
		    id->byte_order = BYTE_ORD_24_GRB;
		  else if ((gmsk > bmsk) && (bmsk > rmsk))
		    id->byte_order = BYTE_ORD_24_GBR;
		  else
		    id->byte_order = 0;
		}
	    }
	}
    }
  id->x.render_depth = id->x.depth;
  XFree(xvir);
  if (id->x.depth == 16)
    {
      xvi.visual = id->x.visual;
      xvi.visualid = XVisualIDFromVisual(id->x.visual);
      xvir = XGetVisualInfo(disp, VisualIDMask, &xvi, &num);
      if (xvir)
	{
	  if (xvir->red_mask != 0xf800)
	    id->x.render_depth = 15;
	  XFree(xvir);
	}
    }
  if (id->x.depth < 8)
    id->x.shmp = 0;
  if ((id->x.depth <= 8) || (override == 1))
    loadpal = 1;
  if (loadpal)
    {
      if (dither == 1)
	{
	  if (remap == 1)
	    id->render_type = RT_DITHER_PALETTE_FAST;
	  else
	    id->render_type = RT_DITHER_PALETTE;
	}
      else
	{
	  if (remap == 1)
	    id->render_type = RT_PLAIN_PALETTE_FAST;
	  else
	    id->render_type = RT_PLAIN_PALETTE;
	}
      if (palfile != NULL)
	Imlib_load_colors(id, palfile);
      if (id->num_colors == 0)
	{
	  fprintf(stderr, "IMLIB ERROR: Cannot Find Palette. A Palette is required for this mode\n");
	  free(id);
	  if (palfile)
	    free(palfile);
	  return NULL;
	}
    }
  else
    {
      if (id->hiq == 1)
	id->render_type = RT_DITHER_TRUECOL;
      else
	id->render_type = RT_PLAIN_TRUECOL;
    }
  {
    XSetWindowAttributes at;
    unsigned long       mask;

    at.border_pixel = 0;
    at.backing_store = NotUseful;
    at.background_pixel = 0;
    at.save_under = False;
    at.override_redirect = True;
    mask = CWOverrideRedirect | CWBackPixel | CWBorderPixel |
      CWBackingStore | CWSaveUnder;
    if (id->x.visual != DefaultVisual(disp, id->x.screen))
      {
	Colormap            cm;

	cm = XCreateColormap(id->x.disp, id->x.root,
			     id->x.visual, AllocNone);
	if (cm)
	  {
	    mask |= CWColormap;
	    id->x.root_cmap = cm;
	    at.colormap = cm;
	    newcm = 1;
	  }
      }
    else if (newcm)
      {
	mask |= CWColormap;
	at.colormap = id->x.root_cmap;
      }
    id->x.base_window = XCreateWindow(id->x.disp, id->x.root,
				      -100, -100, 10, 10, 0,
				      id->x.depth, InputOutput,
				      id->x.visual, mask, &at);
  }
  {
    /* Turn off fastrender if there is an endianess diff between */
    /* client and Xserver */
    int                 byt, bit;

    byt = ImageByteOrder(id->x.disp);	/* LSBFirst | MSBFirst */
    bit = BitmapBitOrder(id->x.disp);	/* LSBFirst | MSBFirst */
    /* if little endian && server big */
    if ((htonl(1) != 1) && (byt == MSBFirst))
      id->fastrend = 0;
    /* if big endian && server little */
    if ((htonl(1) == 1) && (byt == LSBFirst))
      id->fastrend = 0;
  }
  if (palfile)
    free(palfile);
#ifdef HAVE_SHM
  if (id->x.shm)
    {
      XImage             *xim;

      xim = XShmCreateImage(id->x.disp, id->x.visual, id->x.depth,
			    ZPixmap, NULL, &id->x.last_shminfo, 10, 10);
      if (!xim)
	{
	  id->x.shm = 0;
	  id->x.shmp = 0;
	}
      else
	{
	  id->x.last_shminfo.shmid =
	    shmget(IPC_PRIVATE, xim->bytes_per_line * xim->height,
		   IPC_CREAT | 0777);
	  if (id->x.last_shminfo.shmid < 0)
	    {
	      XDestroyImage(xim);
	      id->x.shm = 0;
	      id->x.shmp = 0;
	    }
	  else
	    {
	      id->x.last_shminfo.shmaddr = xim->data =
		shmat(id->x.last_shminfo.shmid, 0, 0);
	      if (id->x.last_shminfo.shmaddr == (char *)-1)
		{
		  XDestroyImage(xim);
		  shmctl(id->x.last_shminfo.shmid, IPC_RMID, 0);
		  id->x.shm = 0;
		  id->x.shmp = 0;
		}
	      else
		{
		  id->x.last_shminfo.readOnly = False;
		  XSetErrorHandler((XErrorHandler) HandleXError);
		  x_error = 0;
		  XShmAttach(id->x.disp, &id->x.last_shminfo);
		  XSync(disp, False);
		  if (x_error)
		    {
		      id->x.shm = 0;
		      id->x.shmp = 0;
		    }
		  else
		    XShmDetach(id->x.disp, &id->x.last_shminfo);
		  XDestroyImage(xim);
		  shmdt(id->x.last_shminfo.shmaddr);
		  shmctl(id->x.last_shminfo.shmid, IPC_RMID, 0);
		}
	    }
	}
    }
#endif
  return id;
}

Pixmap
Imlib_copy_image(ImlibData * id, ImlibImage * im)
{
  Pixmap              p;
  GC                  tgc;
  XGCValues           gcv;

  if (!im || !im->pixmap)
    return 0;
  p = XCreatePixmap(id->x.disp, id->x.base_window, im->width, im->height, id->x.depth);
  gcv.graphics_exposures = False;
  tgc = XCreateGC(id->x.disp, p, GCGraphicsExposures, &gcv);
  XCopyArea(id->x.disp, im->pixmap, p, tgc, 0, 0, im->width, im->height, 0, 0);
  XFreeGC(id->x.disp, tgc);
  return p;
}

Pixmap
Imlib_move_image(ImlibData * id, ImlibImage * im)
{
  Pixmap              p;

  if (!im)
    return 0;
  p = im->pixmap;
  im->pixmap = 0;
  return p;
}

Pixmap
Imlib_copy_mask(ImlibData * id, ImlibImage * im)
{
  Pixmap              p;
  GC                  tgc;
  XGCValues           gcv;

  if (!im || !im->shape_mask)
    return 0;
  p = XCreatePixmap(id->x.disp, id->x.base_window, im->width, im->height, 1);
  gcv.graphics_exposures = False;
  tgc = XCreateGC(id->x.disp, p, GCGraphicsExposures, &gcv);
  XCopyArea(id->x.disp, im->shape_mask, p, tgc, 0, 0, im->width, im->height, 0, 0);
  XFreeGC(id->x.disp, tgc);
  return p;
}

Pixmap
Imlib_move_mask(ImlibData * id, ImlibImage * im)
{
  Pixmap              p;

  if (!im)
    return 0;
  p = im->shape_mask;
  im->shape_mask = 0;
  return p;
}

void
Imlib_destroy_image(ImlibData * id, ImlibImage * im)
{
  if (im)
    {
      if (id->cache.on_image)
	{
	  free_image(id, im);
	  clean_caches(id);
	}
      else
	nullify_image(id, im);
    }
}

void
Imlib_kill_image(ImlibData * id, ImlibImage * im)
{
  if (im)
    {
      if (id->cache.on_image)
	{
	  free_image(id, im);
	  flush_image(id, im);
	  clean_caches(id);
	}
      else
	nullify_image(id, im);
    }
}

void
Imlib_free_pixmap(ImlibData * id, Pixmap pmap)
{
  if (pmap)
    {
      free_pixmappmap(id, pmap);
      clean_caches(id);
    }
}

void
Imlib_set_image_border(ImlibData * id, ImlibImage * im, ImlibBorder * border)
{
  if ((im) && (border))
    {
      if ((im->border.left != border->left) ||
	  (im->border.right != border->right) ||
	  (im->border.top != border->top) ||
	  (im->border.bottom != border->bottom))
	{
	  dirty_pixmaps(id, im);

	  im->border.left = border->left;
	  im->border.right = border->right;
	  im->border.top = border->top;
	  im->border.bottom = border->bottom;
	}
    }
}

void
Imlib_get_image_border(ImlibData * id, ImlibImage * im, ImlibBorder * border)
{
  if ((im) && (border))
    {
      border->left = im->border.left;
      border->right = im->border.right;
      border->top = im->border.top;
      border->bottom = im->border.bottom;
    }
}

void
Imlib_get_image_shape(ImlibData * id, ImlibImage * im, ImlibColor * color)
{
  if ((!im) || (!color))
    return;

  color->r = im->shape_color.r;
  color->g = im->shape_color.g;
  color->b = im->shape_color.b;
}

void
Imlib_set_image_shape(ImlibData * id, ImlibImage * im, ImlibColor * color)
{
  if ((!im) || (!color))
    return;
  if ((im->shape_color.r != color->r) || (im->shape_color.g != color->g) || (im->shape_color.b != color->b))
    {
      im->shape_color.r = color->r;
      im->shape_color.g = color->g;
      im->shape_color.b = color->b;
      dirty_pixmaps(id, im);
    }
}

int
Imlib_get_fallback(ImlibData * id)
{
  if (!id)
    return 0;
  return id->fallback;
}

void
Imlib_set_fallback(ImlibData * id, int fallback)
{
  if (!id)
    return;
  id->fallback = fallback;
}

Visual             *
Imlib_get_visual(ImlibData * id)
{
  if (!id)
    return NULL;
  return id->x.visual;
}

Colormap
Imlib_get_colormap(ImlibData * id)
{
  if (!id)
    return 0;
  return id->x.root_cmap;
}

char               *
Imlib_get_sysconfig(ImlibData * id)
{
  return strdup(SYSTEM_IMRC);
}
