#include <config.h>
#include <setjmp.h>
#define id _gdk_imlib_data
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

#ifdef HAVE_LIBTIFF
#include <tiffio.h>

unsigned char      *
loader_tiff(FILE *f, char *file, int *w, int *h, int *t)
{
  TIFF               *tif;
  unsigned char      *data, *ptr, r, g, b, a;
  int                 x, y;
  uint32              ww, hh, *rast, *tptr;
  size_t              npix;
  int                 istransp;
  int                 fd;

  istransp = 0;
  if (!f)
    return NULL;

  fd = fileno(f);
  /* Apparently rewind(f) isn't sufficient */
  lseek(fd, (long) 0, 0);  
  /* So why does libtif need a filename here ??? */
  tif = TIFFFdOpen(fd, file, "r");

  if (!tif)
    return NULL;

  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &ww);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &hh);
  npix = ww * hh;
  *w = (int)ww;
  *h = (int)hh;
  rast = (uint32 *) _TIFFmalloc(npix * sizeof(uint32));
  if (!rast)
    {
      TIFFClose(tif);
      return NULL;
    }
  data = NULL;
  if (TIFFReadRGBAImage(tif, ww, hh, rast, 0))
    {
      data = (unsigned char *)malloc(*w ** h * 3);
      if (!data)
	{
	  _TIFFfree(rast);
	  TIFFClose(tif);
	  return NULL;
	}
      ptr = data;
      for (y = 0; y < *h; y++)
	{
	  tptr = rast;
	  tptr += ((*h - y - 1) ** w);
	  for (x = 0; x < *w; x++)
	    {
	      a = TIFFGetA(*tptr);
	      b = TIFFGetB(*tptr);
	      g = TIFFGetG(*tptr);
	      r = TIFFGetR(*tptr);
	      tptr++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  istransp = 1;
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
  _TIFFfree(rast);
  TIFFClose(tif);
  *t = istransp;
  return data;
}

gint
saver_tiff (GdkImlibImage * im, char *file, GdkImlibSaveInfo * info)
{
      TIFF               *tif;
      unsigned char      *data;
      int                 y;
      int                 w;

      tif = TIFFOpen(file, "w");
      if (tif)
	{
	  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, im->rgb_width);
	  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, im->rgb_height);
	  TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
	  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
	  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	  TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
	  {
	    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 3);
	    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	    w = TIFFScanlineSize(tif);
	    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP,
			 TIFFDefaultStripSize(tif, -1));
	    for (y = 0; y < im->rgb_height; y++)
	      {
		data = im->rgb_data + (y * im->rgb_width * 3);
		TIFFWriteScanline(tif, data, y, 0);
	      }
	  }
	  TIFFClose(tif);
	  return 1;
	}
      return 0;
}

#else
unsigned char      *
loader_tiff (FILE * f, int *w, int *h, int *t)
{
	return NULL;
}

gint
saver_tiff (GdkImlibImage * im, char *file, GdkImlibSaveInfo * info)
{
	return 0;
}
#endif
