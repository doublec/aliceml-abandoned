#include <config.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

#ifdef HAVE_LIBPNG
#include <png.h>

unsigned char      *
loader_png (FILE * f, int *w, int *h, int *t)
{
  png_structp         png_ptr;
  png_infop           info_ptr;
  unsigned char      *data, *ptr, **lines, *ptr2, r, g, b, a;
  int                 i, x, y, transp, bit_depth, color_type, interlace_type;
  png_uint_32         ww, hh;

  /* Init PNG Reader */
  transp = 0;
#if 0
   /* 
    *Corrupting images on load is better than no image at all
    */
  if (!strcmp("1.0.2", png_libpng_ver))
    {
      fprintf(stderr, "WARNING! You have libpng 1.0.2\n"
	      "It has a known bug that corrupts images on load.\n"
	      "please use 1.0.1. PNG support is disabled.\n");
      return NULL;
    }
#endif
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr)
    return NULL;

  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return NULL;
    }

  if (setjmp(png_ptr->jmpbuf))
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }

  if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  png_init_io(png_ptr, f);
  /* Read Header */
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &ww, &hh, &bit_depth, &color_type, &interlace_type,
	       NULL, NULL);
  *w = ww;
  *h = hh;
  /* Setup Translators */
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_expand(png_ptr);
  png_set_strip_16(png_ptr);
  png_set_packing(png_ptr);
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
    png_set_expand(png_ptr);
  png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
  data = malloc(*w ** h * 3);
  if (!data)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  lines = (unsigned char **)malloc(*h * sizeof(unsigned char *));

  if (lines == NULL)
    {
      free(data);
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  for (i = 0; i < *h; i++)
    {
      if ((lines[i] = malloc(*w * (sizeof(unsigned char) * 4))) == NULL)
	{
	  int                 n;

	  free(data);
	  for (n = 0; n < i; n++)
	    free(lines[n]);
	  free(lines);
	  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	  return NULL;
	}
    }
  png_read_image(png_ptr, lines);
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  ptr = data;
  if (color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    {
      for (y = 0; y < *h; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < *w; x++)
	    {
	      r = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  *ptr++ = r;
		  *ptr++ = r;
		  *ptr++ = r;
		}
	    }
	}
    }
  else if (color_type == PNG_COLOR_TYPE_GRAY)
    {
      for (y = 0; y < *h; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < *w; x++)
	    {
	      r = *ptr2++;
	      *ptr++ = r;
	      *ptr++ = r;
	      *ptr++ = r;
	    }
	}
    }
  else
    {
      for (y = 0; y < *h; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < *w; x++)
	    {
	      r = *ptr2++;
	      g = *ptr2++;
	      b = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  if ((r == 255) && (g == 0) && (b == 255))
		    r = 254;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	}
    }
  for (i = 0; i < *h; i++)
    free(lines[i]);
  free(lines);
  *t = transp;
  return data;
}

struct _io_info
{
  unsigned char *data;
  unsigned char *ptr;
  unsigned char *end;
};

static void 
_gdk_imlib_png_io_read(png_structp png_ptr,
	      png_bytep data, png_uint_32 size)
{
  struct _io_info *io_ptr;
  int bytes;
  
  io_ptr = (struct _io_info *)png_get_io_ptr(png_ptr);
  
  if ((io_ptr->end - io_ptr->ptr) >= size)
    {
      memcpy(data, io_ptr->ptr, size);
      io_ptr->ptr += size;
      return;
    }
  bytes = io_ptr->end - io_ptr->ptr;
  memcpy(data, io_ptr->ptr, bytes);
  io_ptr->ptr = io_ptr->end;
  return;
}

GdkImlibImage *
inline_png(unsigned char *data, int data_size)
{
  GdkImlibImage      *im;
  png_structp         png_ptr;
  png_infop           info_ptr;
  unsigned char      *ptr, **lines, *ptr2, r, g, b, a;
  int                 i, x, y, transp, bit_depth, color_type, interlace_type;
  png_uint_32         ww, hh;
  struct _io_info     io_info;
  char                s [64];
  
#if 0
   /* 
    *Corrupting images on load is better than no image at all
    */
  if (!strcmp("1.0.2", png_libpng_ver))
    {
      fprintf(stderr, "WARNING! You have libpng 1.0.2\n"
	      "It has a known bug that corrupts images on load.\n"
	      "please use 1.0.1.\n");
      return NULL;
    }
#endif
  im = malloc(sizeof(GdkImlibImage));
  if (!im)
    return NULL;
  im->map = NULL;
  im->rgb_width = 0;
  im->rgb_height = 0;
  im->rgb_data = NULL;
  im->alpha_data = NULL;
  g_snprintf (s, sizeof (s), "creat_%x_%x", (int)time(NULL), (int)rand());
  im->filename = strdup (s);
  im->width = 0;
  im->height = 0;
  im->border.left = 0;
  im->border.right = 0;
  im->border.top = 0;
  im->border.bottom = 0;
  im->pixmap = 0;
  im->shape_mask = 0;
  im->cache = 1;
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
  im->shape_color.r = -1;
  im->shape_color.g = -1;
  im->shape_color.b = -1;
  /* Init PNG Reader */
  transp = 0;
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr)
    return NULL;
  
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return NULL;
    }
  
  if (setjmp(png_ptr->jmpbuf))
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  
  if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }

  io_info.data = data;
  io_info.ptr = data;
  io_info.end = data + data_size;
  png_set_read_fn(png_ptr, (void *)(&io_info), (png_rw_ptr)_gdk_imlib_png_io_read);
  
  /* Read Header */
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &ww, &hh, &bit_depth, &color_type, &interlace_type,
	       NULL, NULL);
  im->rgb_width = ww;
  im->rgb_height = hh;  
  /* Setup Translators */
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_expand(png_ptr);
  png_set_strip_16(png_ptr);
  png_set_packing(png_ptr);
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
    png_set_expand(png_ptr);
  png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
  im->rgb_data = malloc(ww * hh * 3);
  if (!(im->rgb_data))
    {
      free(im->filename);
      free(im);
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  lines = (unsigned char **)malloc(hh * sizeof(unsigned char *));
  
  if (lines == NULL)
    {
      free(im->filename);
      free(im);
      free(im->rgb_data);
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  for (i = 0; i < hh; i++)
    {
      if ((lines[i] = malloc(ww * (sizeof(unsigned char) * 4))) == NULL)
	{
	  int                 n;
	  
	  free(im->filename);
	  free(im);
	  free(im->rgb_data);
	  for (n = 0; n < i; n++)
	    free(lines[n]);
	  free(lines);
	  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	  return NULL;
	}
    }
  png_read_image(png_ptr, lines);
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  ptr = im->rgb_data;
  if ((color_type == PNG_COLOR_TYPE_GRAY) ||
      (color_type == PNG_COLOR_TYPE_GRAY_ALPHA))
    {
      for (y = 0; y < hh; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < ww; x++)
	    {
	      r = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  *ptr++ = r;
		  *ptr++ = r;
		  *ptr++ = r;
		}
	    }
	}
    }
  else
    {
      for (y = 0; y < hh; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < ww; x++)
	    {
	      r = *ptr2++;
	      g = *ptr2++;
	      b = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  if ((r == 255) && (g == 0) && (b == 255))
		    r = 254;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	}
    }
  for (i = 0; i < hh; i++)
    free(lines[i]);
  free(lines);
  if (transp)
    {
      im->shape_color.r = 255;
      im->shape_color.g = 0;
      im->shape_color.b = 255;
    }
  if (id->cache.on_image)
    _gdk_imlib_add_image(im, im->filename);
  _gdk_imlib_calc_map_tables(im);
  return im;
}

static unsigned char *
_loader_alpha_png (FILE * f, int *w, int *h, int *t, unsigned char **alpha)
{
	png_structp         png_ptr;
	png_infop           info_ptr;
	unsigned char      *data, *ptr, **lines, *ptr2, *aptr;
	int                 i, x, y, transp, bit_depth, color_type, interlace_type;
	png_uint_32         ww, hh;
	
	/* Init PNG Reader */
	transp = 0;
	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png_ptr)
		return NULL;

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
	{
		png_destroy_read_struct(&png_ptr, NULL, NULL);
		return NULL;
	}
	
	if (setjmp(png_ptr->jmpbuf))
	{
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
		return NULL;
	}
	
	if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA)
	{
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
		return NULL;
	}
	png_init_io(png_ptr, f);

	/* Read Header */
	png_read_info(png_ptr, info_ptr);
	png_get_IHDR(png_ptr, info_ptr, &ww, &hh, &bit_depth, &color_type, &interlace_type,
		     NULL, NULL);
	*w = ww;
	*h = hh;
	/* Setup Translators */
	if (color_type == PNG_COLOR_TYPE_PALETTE)
		png_set_expand(png_ptr);
	png_set_strip_16(png_ptr);
	png_set_packing(png_ptr);
	if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
		png_set_expand(png_ptr);
	png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
	data = malloc(*w ** h * 3);
	if (!data)
	{
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
		return NULL;
	}
	if (color_type != PNG_COLOR_TYPE_GRAY){
		*alpha = malloc(*w * *h);
		if (!*alpha)
		{
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			return NULL;
		}
		transp = 1;
	} else {
		*alpha = NULL;
		transp = 0;
	}
	
	lines = (unsigned char **)malloc(*h * sizeof(unsigned char *));
	
	if (lines == NULL)
	{
		free(data);
		png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
		return NULL;
	}
	for (i = 0; i < *h; i++)
	{
		if ((lines[i] = malloc(*w * (sizeof(unsigned char) * 4))) == NULL)
		{
			int                 n;
			
			free(data);
			free(*alpha);
			for (n = 0; n < i; n++)
				free(lines[n]);
			free(lines);
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			return NULL;
		}
	}
	png_read_image(png_ptr, lines);
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	ptr = data;
	aptr = *alpha;
	if (color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
	{
		unsigned char r;

		for (y = 0; y < *h; y++)
		{
			ptr2 = lines[y];
			for (x = 0; x < *w; x++)
			{
				r = *ptr2++;
				*aptr++ = *ptr2++;
				*ptr++ = r;
				*ptr++ = r;
				*ptr++ = r;
			}
		}
	}
	else if (color_type == PNG_COLOR_TYPE_GRAY)
	{
		unsigned char r;

		for (y = 0; y < *h; y++)
		{
			ptr2 = lines[y];
			for (x = 0; x < *w; x++)
			{
				r = *ptr2++;
				*ptr++ = r;
				*ptr++ = r;
				*ptr++ = r;
			}
		}
	}
	else
	{
		for (y = 0; y < *h; y++)
		{
			ptr2 = lines[y];
			for (x = 0; x < *w; x++)
			{
				*ptr++  = *ptr2++;
				*ptr++  = *ptr2++;
				*ptr++  = *ptr2++;
				*aptr++ = *ptr2++;
			}
		}
	}
	for (i = 0; i < *h; i++)
		free(lines[i]);
	free(lines);
	*t = transp;
	return data;
}

GdkImlibImage *
loader_alpha_png (char *file)
{
	FILE *f;
	int w, h, trans;
	GdkImlibImage *im;
	unsigned char *data, *alpha;
	
	g_return_val_if_fail (file != NULL, NULL);

	f = fopen (file, "rb");
	if (!f)
		return NULL;

	data = _loader_alpha_png (f, &w, &h, &trans, &alpha);
	fclose (f);

	if (!data)
		return NULL;

	im = (GdkImlibImage *) malloc (sizeof (GdkImlibImage));
	if (!im){
		free (data);
		if (alpha)
			free (alpha);
		return NULL;
	}
	memset (im, 0, sizeof (GdkImlibImage));
	
        im->alpha_data = alpha;
	im->shape_color.r = -1;
	im->shape_color.g = -1;
	im->shape_color.b = -1;
	im->rgb_data = data;
	im->rgb_width = w;
	im->rgb_height = h;

	return im;
}

gint
saver_png (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
      png_structp         png_ptr;
      png_infop           info_ptr;
      unsigned char      *data, *ptr;
      int                 x, y;
      png_bytep           row_ptr;
      png_color_8         sig_bit;
      FILE               *f;
      
      f = fopen(file, "wb");
      if (f)
	{
	  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
					    NULL, NULL, NULL);
	  if (!png_ptr)
	    {
	      fclose(f);
	      return 0;
	    }
	  info_ptr = png_create_info_struct(png_ptr);
	  if (info_ptr == NULL)
	    {
	      fclose(f);
	      png_destroy_write_struct(&png_ptr, (png_infopp) NULL);
	      return 0;
	    }
	  if (setjmp(png_ptr->jmpbuf))
	    {
	      fclose(f);
	      png_destroy_write_struct(&png_ptr, (png_infopp) NULL);
	      return 0;
	    }
	  png_init_io(png_ptr, f);
	  png_set_IHDR(png_ptr, info_ptr, im->rgb_width, im->rgb_height, 8,
		       PNG_COLOR_TYPE_RGB_ALPHA, PNG_INTERLACE_NONE,
		       PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
	  sig_bit.red = 8;
	  sig_bit.green = 8;
	  sig_bit.blue = 8;
	  sig_bit.alpha = 8;
	  png_set_sBIT(png_ptr, info_ptr, &sig_bit);
	  png_write_info(png_ptr, info_ptr);
	  png_set_shift(png_ptr, &sig_bit);
	  png_set_packing(png_ptr);
	  data = malloc(im->rgb_width * 4);
	  if (!data)
	    {
	      fclose(f);
	      png_destroy_write_struct(&png_ptr, (png_infopp) NULL);
	      return 0;
	    }
	  for (y = 0; y < im->rgb_height; y++)
	    {
	      ptr = im->rgb_data + (y * im->rgb_width * 3);
	      for (x = 0; x < im->rgb_width; x++)
		{
		  data[(x << 2) + 0] = *ptr++;
		  data[(x << 2) + 1] = *ptr++;
		  data[(x << 2) + 2] = *ptr++;
		  if ((data[(x << 2) + 0] == im->shape_color.r) &&
		      (data[(x << 2) + 1] == im->shape_color.g) &&
		      (data[(x << 2) + 2] == im->shape_color.b))
		    data[(x << 2) + 3] = 0;
		  else
		    data[(x << 2) + 3] = 255;
		}
	      row_ptr = data;
	      png_write_rows(png_ptr, &row_ptr, 1);
	    }
	  free(data);
	  png_write_end(png_ptr, info_ptr);
	  png_destroy_write_struct(&png_ptr, (png_infopp) NULL);

	  fclose(f);
	  return 1;
	}
      return 0;
}

#else
unsigned char      *
loader_png (FILE * f, int *w, int *h, int *t)
{
	return NULL;
}

gint
saver_png (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return 0;
}

GdkImlibImage *
inline_png (unsigned char *data, int data_size)
{
	return NULL;
}

GdkImlibImage *
loader_alpha_png (char *file)
{
	return NULL;
}
#endif

