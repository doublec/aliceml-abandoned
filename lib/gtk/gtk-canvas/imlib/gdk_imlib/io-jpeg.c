#include <config.h>
#include <setjmp.h>
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

#ifdef HAVE_LIBJPEG
#include <jpeglib.h>

/** 
 * This error handling is broken beyond belief, but oh well it works
 **/

struct ImLib_JPEG_error_mgr
{
  struct jpeg_error_mgr pub;
  sigjmp_buf          setjmp_buffer;
};

typedef struct ImLib_JPEG_error_mgr *emptr;

static void
g_JPEGFatalErrorHandler(j_common_ptr cinfo)
{
  /* FIXME:
   * We should somehow signal what error occurred to the caller so the
   * caller can handle the error message */
  emptr               errmgr;

  errmgr = (emptr) cinfo->err;
  cinfo->err->output_message(cinfo);
  siglongjmp(errmgr->setjmp_buffer, 1);
  return;
}

unsigned char      *
loader_jpeg (FILE * f, int *w, int *h, int *t)
{
  struct jpeg_decompress_struct cinfo;
  struct ImLib_JPEG_error_mgr jerr;
  unsigned char      *data, *line[16], *ptr;
  int                 x, y, i;

  *t = 0;
  cinfo.err = jpeg_std_error(&(jerr.pub));
  jerr.pub.error_exit = g_JPEGFatalErrorHandler;

  /* error handler to longjmp to, we want to preserve signals */
  if (sigsetjmp(jerr.setjmp_buffer, 1))
    {
      /* Whoops there was a jpeg error */
      jpeg_destroy_decompress(&cinfo);
      return NULL;
    }

  jpeg_create_decompress(&cinfo);
  jpeg_stdio_src(&cinfo, f);
  jpeg_read_header(&cinfo, TRUE);
  cinfo.do_fancy_upsampling = FALSE;
  cinfo.do_block_smoothing = FALSE;
  jpeg_start_decompress(&cinfo);
  *w = cinfo.output_width;
  *h = cinfo.output_height;
  data = malloc(*w ** h * 3);
  if (!data)
    {
      jpeg_destroy_decompress(&cinfo);
      return NULL;
    }
  ptr = data;

  if (cinfo.rec_outbuf_height > 16)
    {
      fprintf(stderr, "gdk_imlib ERROR: JPEG uses line buffers > 16. Cannot load.\n");
      return NULL;
    }
  if (cinfo.output_components == 3)
    {
      for (y = 0; y < *h; y += cinfo.rec_outbuf_height)
	{
	  for (i = 0; i < cinfo.rec_outbuf_height; i++)
	    {
	      line[i] = ptr;
	      ptr += *w * 3;
	    }
	  jpeg_read_scanlines(&cinfo, line, cinfo.rec_outbuf_height);
	}
    }
  else if (cinfo.output_components == 1)
    {
      for (i = 0; i < cinfo.rec_outbuf_height; i++)
	{
	  if ((line[i] = malloc(*w)) == NULL)
	    {
	      int                 t = 0;

	      for (t = 0; t < i; t++)
		free(line[t]);
	      jpeg_destroy_decompress(&cinfo);
	      return NULL;
	    }
	}
      for (y = 0; y < *h; y += cinfo.rec_outbuf_height)
	{
	  jpeg_read_scanlines(&cinfo, line, cinfo.rec_outbuf_height);
	  for (i = 0; i < cinfo.rec_outbuf_height; i++)
	    {
	      for (x = 0; x < *w; x++)
		{
		  *ptr++ = line[i][x];
		  *ptr++ = line[i][x];
		  *ptr++ = line[i][x];
		}
	    }
	}
      for (i = 0; i < cinfo.rec_outbuf_height; i++)
	free(line[i]);
    }
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);

  return data;
}

gint
saver_jpeg (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
      struct jpeg_compress_struct cinfo;
      struct jpeg_error_mgr jerr;
      JSAMPROW            row_pointer[1];
      int                 row_stride;
      FILE *f;
	     
      f = fopen(file, "wb");
      if (f)
	{
	  cinfo.err = jpeg_std_error(&jerr);
	  jpeg_create_compress(&cinfo);
	  jpeg_stdio_dest(&cinfo, f);
	  cinfo.image_width = im->rgb_width;
	  cinfo.image_height = im->rgb_height;
	  cinfo.input_components = 3;
	  cinfo.in_color_space = JCS_RGB;
	  jpeg_set_defaults(&cinfo);
	  jpeg_set_quality(&cinfo, (100 * info->quality) >> 8, TRUE);
	  jpeg_start_compress(&cinfo, TRUE);
	  row_stride = cinfo.image_width * 3;
	  while (cinfo.next_scanline < cinfo.image_height)
	    {
	      row_pointer[0] = im->rgb_data + (cinfo.next_scanline * row_stride);
	      jpeg_write_scanlines(&cinfo, row_pointer, 1);
	    }
	  jpeg_finish_compress(&cinfo);
	  fclose(f);
	  return 1;
	}
      return 0;
}

#else
unsigned char      *
loader_jpeg (FILE * f, int *w, int *h, int *t)
{
	return NULL;
}

gint
saver_jpeg (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
	return 0;
}
#endif
