/*
 * modules.c: Dynamically loading the format handlers for save and load
 * operations in Imlib.
 *
 * Author:
 *    Miguel de Icaza (miguel@gnu.org).
 */
#include <config.h>
#include <glib.h>
#include <gmodule.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

#ifdef USE_GMODULE

static unsigned char *loader_bmp (FILE *f, int *w, int *h, int *t);
static unsigned char *loader_xpm (FILE *f, int *w, int *h, int *t);
static unsigned char *loader_gif (FILE *f, int *w, int *h, int *t);
static unsigned char *loader_tiff (FILE *f, char *n, int *w, int *h, int *t);
static unsigned char *loader_jpeg (FILE *f, int *w, int *h, int *t);
static unsigned char *loader_png (FILE *f, int *w, int *h, int *t);
static unsigned char *loader_ppm (FILE *f, int *w, int *h, int *t);

static gint saver_tiff (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info);
static gint saver_png  (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info);
static gint saver_jpeg (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info);
static gint saver_ps   (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info);
static gint saver_ppm  (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info);

static GdkImlibImage *loader_alpha_png (char *file);

static GdkImlibImage *inline_png (unsigned char *data, int data_size);

#else
#   include "io-png.c"
#   include "io-bmp.c"
#   include "io-gif.c"
#   include "io-jpeg.c"
#   include "io-ppm.c"
#   include "io-ps.c"
#   include "io-tiff.c"
#   include "io-xpm.c"
#endif

gdk_imlib_loader_fn _gdk_imlib_LoadBMP   = loader_bmp;
gdk_imlib_loader_fn _gdk_imlib_LoadXPM   = loader_xpm;
gdk_imlib_loader_fn _gdk_imlib_LoadGIF   = loader_gif;
gdk_imlib_loader_fn2 _gdk_imlib_LoadTIFF = loader_tiff;
gdk_imlib_loader_fn _gdk_imlib_LoadJPEG  = loader_jpeg;
gdk_imlib_loader_fn _gdk_imlib_LoadPNG   = loader_png;
gdk_imlib_loader_fn _gdk_imlib_LoadPPM   = loader_ppm;

gdk_imlib_saver_fn _gdk_imlib_SaveTIFF   = saver_tiff;
gdk_imlib_saver_fn _gdk_imlib_SavePNG    = saver_png;
gdk_imlib_saver_fn _gdk_imlib_SaveJPEG   = saver_jpeg;
gdk_imlib_saver_fn _gdk_imlib_SavePS     = saver_ps;
gdk_imlib_saver_fn _gdk_imlib_SavePPM    = saver_ppm;

gdk_imlib_inline_fn _gdk_imlib_inlined_png_to_image = inline_png;
gdk_imlib_load_alpha_fn _gdk_imlib_load_alpha_png = loader_alpha_png;

#ifdef USE_GMODULE
static unsigned char *
load_fail_fn (FILE *f, int *w, int *h, int *t)
{
	return NULL;
}

static gint
save_fail_fn (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return 0;
}

static gboolean
get_module_loader_saver (char *mod,
			 void **loader, void *def_loader,
			 void **saver,  void *def_saver)
{
	char *path, *modname;
	GModule *m;
	gboolean v;
	void *ptr;

	modname = g_strconcat ("imlib-", mod, NULL);
	path = g_module_build_path (IMLIB_LIB, modname);
	g_free (modname);

	m = g_module_open (path, G_MODULE_BIND_LAZY);
	g_free (path);
	if (!m){
		if (loader)
			*loader = def_loader;
		if (saver)
			*saver  = def_saver;
		return FALSE;
	}

	if (loader){
		char *loader_name;
		
		loader_name = g_strconcat ("loader_", mod, NULL);
		v = g_module_symbol (m, loader_name, (void **) &ptr);
		if (v)
			*loader = ptr;
		else
			*loader = def_loader;
		g_free (loader_name);
	}

	if (saver){
		char *saver_name;
		
		saver_name = g_strconcat ("saver_", mod, NULL);
		v = g_module_symbol (m, saver_name, (void **) &ptr);
		if (v)
			*saver = ptr;
		else
			*saver = def_saver;
		g_free (saver_name);
	}

	/* Ugly hack, this is an exception */
	if (strcmp (mod, "png") == 0){
		char *inline_name, *alpha_name;
		
		inline_name = g_strconcat ("inline_", mod, NULL);
		v = g_module_symbol (m, inline_name, (void **) &ptr);
		if (v)
			_gdk_imlib_inlined_png_to_image = ptr;
		else
			_gdk_imlib_inlined_png_to_image = (gdk_imlib_inline_fn) load_fail_fn;
		g_free (inline_name);

		alpha_name = g_strconcat ("loader_alpha_", mod, NULL);
		v = g_module_symbol (m, alpha_name, (void **) &ptr);
		if (v)
			_gdk_imlib_load_alpha_png = ptr;
		else
			_gdk_imlib_load_alpha_png = (gdk_imlib_load_alpha_fn) load_fail_fn;
		g_free (alpha_name);
	}

	return TRUE; /* FIXME: return value is never checked */
}
			 
static unsigned char *
load_module_relay (char *mod, gdk_imlib_loader_fn *lf, gdk_imlib_saver_fn *sf, FILE *f, int *w, int *h, int *t)
{
	get_module_loader_saver (mod,
				 (void **) lf, (void *) load_fail_fn,
				 (void **) sf, (void *) save_fail_fn);

	return (*lf)(f, w, h, t);
}

static gint
save_module_relay (char *mod, gdk_imlib_loader_fn *lf, gdk_imlib_saver_fn *sf,
		   GdkImlibImage *im, char *fname, GdkImlibSaveInfo *info)
{
	get_module_loader_saver (mod,
				 (void **) lf, (void *) load_fail_fn,
				 (void **) sf, (void *) save_fail_fn);

	return (*sf)(im, fname, info);
}

static unsigned char *
loader_tiff (FILE *f, char *n, int *w, int *h, int *t)
{
	get_module_loader_saver ("tiff",
				 (void **) &_gdk_imlib_LoadTIFF, (void *) load_fail_fn,
				 (void **) &_gdk_imlib_SaveTIFF, (void *) save_fail_fn);

	return _gdk_imlib_LoadTIFF (f, n, w, h, t);
}

static unsigned char *
loader_bmp (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("bmp", &_gdk_imlib_LoadBMP, NULL, f, w, h, t);
}

static unsigned char *
loader_xpm (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("xpm", &_gdk_imlib_LoadXPM, NULL, f, w, h, t);
}

static unsigned char *
loader_gif (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("gif", &_gdk_imlib_LoadGIF, NULL, f, w, h, t);
}

static unsigned char *
loader_jpeg (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("jpeg",
				  &_gdk_imlib_LoadJPEG,
				  &_gdk_imlib_SaveJPEG, f, w, h, t);
}

static unsigned char *
loader_png (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("png",
				  &_gdk_imlib_LoadPNG,
				  &_gdk_imlib_SavePNG, f, w, h, t);
}

static unsigned char *
loader_ppm (FILE *f, int *w, int *h, int *t)
{
	return load_module_relay ("ppm",
				  &_gdk_imlib_LoadPPM,
				  &_gdk_imlib_SavePPM, f, w, h, t);
}

static gint
saver_tiff (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return save_module_relay ("tiff",
				  (gdk_imlib_loader_fn *) &_gdk_imlib_LoadTIFF,
				  &_gdk_imlib_SaveTIFF, im, file, info);
}

static gint
saver_png  (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return save_module_relay ("png",
				  &_gdk_imlib_LoadPNG,
				  &_gdk_imlib_SavePNG, im, file, info);
}

static gint
saver_jpeg (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return save_module_relay ("jpeg",
				  &_gdk_imlib_LoadJPEG,
				  &_gdk_imlib_SaveJPEG, im, file, info);
}

static gint
saver_ps   (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return save_module_relay ("ps", NULL, &_gdk_imlib_SavePS, im, file, info);
}

static gint
saver_ppm   (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return save_module_relay ("ppm", &_gdk_imlib_LoadPPM, &_gdk_imlib_SavePPM, im, file, info);
}

static GdkImlibImage *
inline_png (unsigned char *data, int data_size)
{
	get_module_loader_saver ("png",
				 (void **) &_gdk_imlib_LoadPNG, load_fail_fn,
				 (void **) &_gdk_imlib_SavePNG, save_fail_fn);
	return _gdk_imlib_inlined_png_to_image (data, data_size);
}

static GdkImlibImage *
loader_alpha_png (char *file)
{
	get_module_loader_saver ("png",
				 (void **) &_gdk_imlib_LoadPNG, load_fail_fn,
				 (void **) &_gdk_imlib_SavePNG, save_fail_fn);
	return _gdk_imlib_load_alpha_png (file);
}
#endif


GdkImlibImage *
gdk_imlib_inlined_png_to_image(unsigned char *data, int data_size)
{
	return _gdk_imlib_inlined_png_to_image (data, data_size);
}

GdkImlibImage *
gdk_imlib_load_alpha (char *file)
{
	/* We only support png for now */
	return _gdk_imlib_load_alpha_png (file);
}
