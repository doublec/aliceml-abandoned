#define _GNU_SOURCE
#include <config.h>
#include "Imlib.h"
#include "Imlib_private.h"
#include <setjmp.h>

/*      Split the ID - damages input    */

static char        *
_SplitID(char *file)
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
 * *     Doesn't damage the input
 */

char               *
_GetExtension(char *file)
{
  char               *p = strrchr(file, '.');

  if (p == NULL)
    return "";
  else
    return p + 1;
}

#ifdef HAVE_LIBJPEG

/** 
 *  * This error handling is broken beyond belief, but oh well it works
 *  **/

struct ImLib_JPEG_error_mgr
{
  struct jpeg_error_mgr pub;
  sigjmp_buf          setjmp_buffer;
};

typedef struct ImLib_JPEG_error_mgr *emptr;

void
_JPEGFatalErrorHandler(j_common_ptr cinfo)
{
  /* 
   * FIXME:
   * We should somehow signal what error occurred to the caller so the
   * caller can handle the error message 
   */
  emptr               errmgr;

  errmgr = (emptr) cinfo->err;
  cinfo->err->output_message(cinfo);
  siglongjmp(errmgr->setjmp_buffer, 1);
  return;
}

unsigned char      *
_LoadJPEG(ImlibData * id, FILE * f, int *w, int *h)
{
  struct jpeg_decompress_struct cinfo;
  struct ImLib_JPEG_error_mgr jerr;
  unsigned char      *data, *line[16], *ptr;
  int                 x, y, i;

  cinfo.err = jpeg_std_error(&(jerr.pub));
  jerr.pub.error_exit = _JPEGFatalErrorHandler;

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
      fprintf(stderr, "Imlib ERROR: JPEG uses line buffers > 16. Cannot load.\n");
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
#endif /* HAVE_LIBJPEG */

#ifdef HAVE_LIBPNG
unsigned char      *
_LoadPNG(ImlibData * id, FILE * f, int *w, int *h, int *t)
{
  png_structp         png_ptr;
  png_infop           info_ptr;
  unsigned char      *data, *ptr, **lines, *ptr2, r, g, b, a;
  int                 i, x, y, transp, bit_depth, color_type, interlace_type;
  png_uint_32         ww, hh;

  /* Init PNG Reader */
  transp = 0;
/*
  if (!strcmp("1.0.2", png_libpng_ver))
    {
      fprintf(stderr, "WARNING! You have libpng 1.0.2\n"
	      "It has a known bug that corrupts images on load.\n"
	      "please use 1.0.1. PNG support is disabled.\n");
      data = malloc(broke_width * broke_height * 3);
      memcpy(data, broke, broke_width * broke_height * 3);
      *t = 0;
      *w = broke_width;
      *h = broke_height;
      return data;
    }
*/
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
#endif /* HAVE_LIBPNG */

#ifdef HAVE_LIBTIFF
unsigned char      *
_LoadTIFF(ImlibData * id, FILE *f, char *file, int *w, int *h, int *t)
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

#endif /* HAVE_LIBTIFF */

#ifdef HAVE_LIBGIF
unsigned char      *
_LoadGIF(ImlibData * id, FILE *f, int *w, int *h, int *t)
{
  unsigned char      *data, *ptr;
  GifFileType        *gif;
  GifRowType         *rows;
  GifRecordType       rec;
  ColorMapObject     *cmap;
  int                 i, j, done, bg, csize, r, g, b;
  int                 intoffset[] = {0, 4, 2, 1};
  int                 intjump[] = {8, 8, 4, 2};
  int                 istransp, transp;
  int                 fd;

  done = 0;
  istransp = 0;
  data = NULL;
  rows = NULL;
  transp = -1;

  fd = fileno(f);
  /* Apparently rewind(f) isn't sufficient */
  lseek(fd, (long) 0, 0);
  gif = DGifOpenFileHandle(fd);

  if (!gif)
    return NULL;
  do
    {
      if (DGifGetRecordType(gif, &rec) == GIF_ERROR)
	{
	  PrintGifError();
	  rec = TERMINATE_RECORD_TYPE;
	}
      if ((rec == IMAGE_DESC_RECORD_TYPE) && (!done))
	{
	  if (DGifGetImageDesc(gif) == GIF_ERROR)
	    {
	      PrintGifError();
	      rec = TERMINATE_RECORD_TYPE;
	    }
	  *w = gif->Image.Width;
	  *h = gif->Image.Height;
	  rows = malloc(*h * sizeof(GifRowType *));
	  if (!rows)
	    {
	      DGifCloseFile(gif);
	      return NULL;
	    }
	  data = malloc(*w ** h * 3);
	  if (!data)
	    {
	      DGifCloseFile(gif);
	      free(rows);
	      return NULL;
	    }
	  for (i = 0; i < *h; i++)
	    rows[i] = NULL;
	  for (i = 0; i < *h; i++)
	    {
	      rows[i] = malloc(*w * sizeof(GifPixelType));
	      if (!rows[i])
		{
		  DGifCloseFile(gif);
		  for (i = 0; i < *h; i++)
		    if (rows[i])
		      free(rows[i]);
		  free(rows);
		  free(data);
		  return NULL;
		}
	    }
	  if (gif->Image.Interlace)
	    {
	      for (i = 0; i < 4; i++)
		{
		  for (j = intoffset[i]; j < *h; j += intjump[i])
		    DGifGetLine(gif, rows[j], *w);
		}
	    }
	  else
	    {
	      for (i = 0; i < *h; i++)
		DGifGetLine(gif, rows[i], *w);
	    }
	  done = 1;
	}
      else if (rec == EXTENSION_RECORD_TYPE)
	{
	  int                 ext_code;
	  GifByteType        *ext;

	  ext = NULL;
	  DGifGetExtension(gif, &ext_code, &ext);
	  while (ext)
	    {
	      if ((ext_code == 0xf9) && (ext[1] & 1) && (transp < 0))
		{
		  istransp = 1;
		  transp = (int)ext[4];
		}
	      ext = NULL;
	      DGifGetExtensionNext(gif, &ext);
	    }
	}
    }
  while (rec != TERMINATE_RECORD_TYPE);
  bg = gif->SBackGroundColor;
  cmap = (gif->Image.ColorMap ? gif->Image.ColorMap : gif->SColorMap);
  csize = cmap->ColorCount;
  ptr = data;
  if (!istransp)
    {
      for (i = 0; i < *h; i++)
	{
	  for (j = 0; j < *w; j++)
	    {
	      r = cmap->Colors[rows[i][j]].Red;
	      g = cmap->Colors[rows[i][j]].Green;
	      b = cmap->Colors[rows[i][j]].Blue;
	      *ptr++ = r;
	      *ptr++ = g;
	      *ptr++ = b;
	    }
	}
    }
  else
    {
      for (i = 0; i < *h; i++)
	{
	  for (j = 0; j < *w; j++)
	    {
	      if (rows[i][j] == transp)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		}
	      else
		{
		  r = cmap->Colors[rows[i][j]].Red;
		  g = cmap->Colors[rows[i][j]].Green;
		  b = cmap->Colors[rows[i][j]].Blue;
		  if ((r == 255) && (g == 0) && (b == 255))
		    r = 254;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	}
    }
  DGifCloseFile(gif);
  for (i = 0; i < *h; i++)
    free(rows[i]);
  free(rows);
  *t = istransp;
  return data;
}

#endif /* HAVE_LIBGIF */

unsigned char      *
_LoadBMP(ImlibData * id, FILE *file, int *w, int *h, int *t)
{
  unsigned char      *data, *ptr;
  int                 done, i, bpp, planes, comp, ncolors, line, column,
                      linesize, linepos, rshift, gshift, bshift, size;
  unsigned char       byte;
  short int           word;
  long int            dbuf[4], dword, rmask, gmask, bmask, offset;
  signed char         bbuf[4];
  struct _cmap
    {
      unsigned char       r, g, b;
    }
                     *cmap;

#define BI_RGB       0
#define BI_RLE8      1
#define BI_RLE4      2
#define BI_BITFIELDS 3

  rshift = 0;
  gshift = 0;
  bshift = 0;
  rmask = 0xff;
  gmask = 0xff;
  bmask = 0xff;
  if (!file)
    return NULL;

  done = 0;
  /* 
   * reading the bmp header 
   */

  fread(bbuf, 1, 2, file);

  fread(dbuf, 4, 4, file);

  size = dbuf[0];
  offset = dbuf[2];

  fread(dbuf, 4, 2, file);
  *w = (int)dbuf[0];
  *h = (int)dbuf[1];
  if (*w > 32767)
    {
      fprintf(stderr, "IMLIB ERROR: Image width > 32767 pixels for file\n");
      return NULL;
    }
  if (*h > 32767)
    {
      fprintf(stderr, "IMLIB ERROR: Image height > 32767 pixels for file\n");
      return NULL;
    }
  fread(&word, 2, 1, file);
  planes = (int)word;
  fread(&word, 2, 1, file);
  bpp = (int)word;
  if (bpp != 1 && bpp != 4 && bpp != 8 && bpp && 16 && bpp != 24 && bpp != 32)
    {
      fprintf(stderr, "IMLIB ERROR: unknown bitdepth in file\n");
      return NULL;
    }
  fread(dbuf, 4, 4, file);
  comp = (int)dbuf[0];
  if (comp != BI_RGB && comp != BI_RLE4 && comp != BI_RLE8 && comp != BI_BITFIELDS)
    {
      fprintf(stderr, "IMLIB ERROR: unknown encoding in Windows BMP file\n");
      return NULL;
    }
  fread(dbuf, 4, 2, file);
  ncolors = (int)dbuf[0];
  if (ncolors == 0)
    ncolors = 1 << bpp;
  /* some more sanity checks */
  if (((comp == BI_RLE4) && (bpp != 4)) || ((comp == BI_RLE8) && (bpp != 8)) || ((comp == BI_BITFIELDS) && (bpp != 16 && bpp != 32)))
    {
      fprintf(stderr, "IMLIB ERROR: encoding of BMP doesn't match bitdepth\n");
      return NULL;
    }
  if (bpp < 16)
    {
      cmap = (struct _cmap *)malloc(sizeof(struct _cmap) * ncolors);

      if (!cmap)
	{
	  fprintf(stderr, "IMLIB ERROR: Cannot allocate RAM for color map in BMP file\n");
	  return NULL;
	}
    }
  else
    cmap = NULL;
  ptr = (unsigned char *)malloc(*w * *h * 3);
  if (!ptr)
    {
      fprintf(stderr, "IMLIB ERROR: Cannot allocate RAM for RGB data in file\n");
      if (cmap)
	free(cmap);
      return NULL;
    }

  /*
   * Reading the palette, if it exists.
   */
  if (bpp < 16 && ncolors != 0)
    {
      for (i = 0; i < ncolors; i++)
	{
	  fread(bbuf, 1, 4, file);
	  cmap[i].b = bbuf[0];
	  cmap[i].g = bbuf[1];
	  cmap[i].r = bbuf[2];
	}
    }
  else if (bpp == 16 || bpp == 32)
    {
      if (comp == BI_BITFIELDS)
	{
	  int                 bit = 0;

	  fread(dbuf, 4, 3, file);
	  bmask = dbuf[0];
	  gmask = dbuf[1];
	  rmask = dbuf[2];
	  /* find shift amount.. ugly, but i can't think of a better way */
	  for (bit = 0; bit < bpp; bit++)
	    {
	      if (bmask & (1 << bit))
		bshift = bit;
	      if (gmask & (1 << bit))
		gshift = bit;
	      if (rmask & (1 << bit))
		rshift = bit;
	    }
	}
      else if (bpp == 16)
	{
	  rmask = 0x7C00;
	  gmask = 0x03E0;
	  bmask = 0x001F;
	  rshift = 10;
	  gshift = 5;
	  bshift = 0;
	}
      else if (bpp == 32)
	{
	  rmask = 0x00FF0000;
	  gmask = 0x0000FF00;
	  bmask = 0x000000FF;
	  rshift = 16;
	  gshift = 8;
	  bshift = 0;
	}
    }

  /*
   * Reading the image data
   */
  fseek(file, offset, SEEK_SET);
  data = ptr;

  /* set the whole image to the background color */
  if (bpp < 16 && (comp == BI_RLE4 || comp == BI_RLE8))
    {
      for (i = 0; i < *w * *h; i++)
	{
	  *ptr++ = cmap[0].r;
	  *ptr++ = cmap[0].g;
	  *ptr++ = cmap[0].b;
	}
      ptr = data;
    }
  line = 0;
  column = 0;
#define poffset (line * *w * 3 + column * 3)

  /*
   * BMPs are stored upside down... hmmmmmmmmmm....
   */

  linesize = ((*w * bpp + 31) / 32) * 4;
  for (line = (*h - 1); line >= 0; line--)
    {
      linepos = 0;
      for (column = 0; column < *w;)
	{
	  if (bpp < 16)
	    {
	      int                 index;

	      linepos++;
	      byte = getc(file);
	      if (bpp == 1)
		{
		  int                 bit = 0;

		  for (bit = 0; bit < 8; bit++)
		    {
		      index = ((byte & (0x80 >> bit)) ? 1 : 0);
		      ptr[poffset] = cmap[index].r;
		      ptr[poffset + 1] = cmap[index].g;
		      ptr[poffset + 2] = cmap[index].b;
		      column++;
		    }
		}
	      else if (bpp == 4)
		{
		  if (comp == BI_RLE4)
		    {
		      fprintf(stderr, "can't deal with 4bit encoded yet.\n");
		      free(data);
		      free(cmap);
		      return NULL;
		    }
		  else
		    {
		      int                 nibble = 0;

		      for (nibble = 0; nibble < 2; nibble++)
			{
			  index = ((byte & (0xF0 >> nibble * 4)) >> (!nibble * 4));
			  if (index >= 16)
			    index = 15;
			  ptr[poffset] = cmap[index].r;
			  ptr[poffset + 1] = cmap[index].g;
			  ptr[poffset + 2] = cmap[index].b;
			  column++;
			}
		    }
		}
	      else if (bpp == 8)
		{
		  if (comp == BI_RLE8)
		    {
		      unsigned char       first = byte;

		      byte = getc(file);
		      if (first == 0)
			{
			  if (byte == 0)
			    {
			      /*   column = *w; */
			    }
			  else if (byte == 1)
			    {
			      column = *w;
			      line = -1;
			    }
			  else if (byte == 2)
			    {
			      byte = getc(file);
			      column += byte;
			      linepos = column * bpp / 8;
			      byte = getc(file);
			      line += byte;
			    }
			  else
			    {
			      int                 absolute = byte;

			      for (i = 0; i < absolute; i++)
				{
				  linepos++;
				  byte = getc(file);
				  ptr[poffset] = cmap[byte].r;
				  ptr[poffset + 1] = cmap[byte].g;
				  ptr[poffset + 2] = cmap[byte].b;
				  column++;
				}
			      if (absolute & 0x01)
				byte = getc(file);
			    }
			}
		      else
			{
			  for (i = 0; i < first; i++)
			    {
			      ptr[poffset] = cmap[byte].r;
			      ptr[poffset + 1] = cmap[byte].g;
			      ptr[poffset + 2] = cmap[byte].b;
			      column++;
			      linepos++;
			    }
			}
		    }
		  else
		    {
		      ptr[poffset] = cmap[byte].r;
		      ptr[poffset + 1] = cmap[byte].g;
		      ptr[poffset + 2] = cmap[byte].b;
		      column++;
		    }
		}
	    }
	  else if (bpp == 24)
	    {
	      linepos += fread(bbuf, 1, 3, file);
	      ptr[poffset] = (unsigned char)bbuf[2];
	      ptr[poffset + 1] = (unsigned char)bbuf[1];
	      ptr[poffset + 2] = (unsigned char)bbuf[0];
	      column++;
	    }
	  else if (bpp == 16)
	    {
	      unsigned char       temp;

	      linepos += fread(&word, 2, 1, file);
	      temp = (word & rmask) >> rshift;
	      ptr[poffset] = temp;
	      temp = (word & gmask) >> gshift;
	      ptr[poffset + 1] = temp;
	      temp = (word & bmask) >> gshift;
	      ptr[poffset + 2] = temp;
	      column++;
	    }
	  else
	    {
	      unsigned char       temp;

	      linepos += fread(&dword, 4, 1, file);
	      temp = (dword & rmask) >> rshift;
	      ptr[poffset] = temp;
	      temp = (dword & gmask) >> gshift;
	      ptr[poffset + 1] = temp;
	      temp = (dword & bmask) >> bshift;
	      ptr[poffset + 2] = temp;
	      column++;
	    }
	}
      while ((linepos < linesize) && (comp != 1) && (comp != 2))
	{
	  int                 temp = fread(&byte, 1, 1, file);

	  linepos += temp;
	  if (!temp)
	    break;
	}
    }
  if (cmap)
    free(cmap);
  *t = 0;
  return data;
}

unsigned char      *
_LoadXPM(ImlibData * id, FILE *file, int *w, int *h, int *t)
{
  unsigned char      *data, *ptr, *end;
  int                 pc, c, i, j, k, ncolors, cpp, comment, transp, quote,
                      context, len, done;
  char                *line, s[256], tok[128], col[256];
  XColor              xcol;
  int                 lsz = 256;
  struct _cmap
    {
      unsigned char       str[6];
      unsigned char       transp;
      short                 r, g, b;
    }
                     *cmap;
  short                 lookup[128 - 32][128 - 32];

  transp = 0;
  done = 0;
  if (!file)
    return NULL;
  i = 0;
  j = 0;
  cmap = NULL;
  *w = 10;
  *h = 10;
  ptr = NULL;
  data = NULL;
  end = NULL;
  c = ' ';
  comment = 0;
  quote = 0;
  context = 0;
  line = malloc(lsz);
  while (!done)
    {
      pc = c;
      c = fgetc(file);
      if (c == EOF)
	break;
      if (!quote)
	{
	  if ((pc == '/') && (c == '*'))
	    comment = 1;
	  else if ((pc == '*') && (c == '/') && (comment))
	    comment = 0;
	}
      if (!comment)
	{
	  if ((!quote) && (c == '"'))
	    {
	      quote = 1;
	      i = 0;
	    }
	  else if ((quote) && (c == '"'))
	    {
	      line[i] = 0;
	      quote = 0;
	      if (context == 0)
		{
		  /* Header */
		  sscanf(line, "%i %i %i %i", w, h, &ncolors, &cpp);
                  if (ncolors > 32766)
		    {
		      fprintf(stderr, "IMLIB ERROR: XPM files wth colors > 32766 not supported\n");
		      free(line);
		      return NULL;
		    }
		  if (cpp > 5)
		    {
		      fprintf(stderr, "IMLIB ERROR: XPM files with characters per pixel > 5 not supported\n");
		      free(line);
		      return NULL;
		    }
		  if (*w > 32767)
		    {
		      fprintf(stderr, "IMLIB ERROR: Image width > 32767 pixels for file\n");
		      free(line);
		      return NULL;
		    }
		  if (*h > 32767)
		    {
		      fprintf(stderr, "IMLIB ERROR: Image height > 32767 pixels for file\n");
		      free(line);
		      return NULL;
		    }
		  cmap = malloc(sizeof(struct _cmap) * ncolors);

		  if (!cmap)
		    {
		      free(line);
		      return NULL;
		    }
		  data = malloc(*w ** h * 3);
		  if (!data)
		    {
		      free(cmap);
		      free(line);
		      return NULL;
		    }
		  ptr = data;
		  end = ptr + (*w ** h * 3);
		  j = 0;
		  context++;
		}
	      else if (context == 1)
		{
		  /* Color Table */
		  if (j < ncolors)
		    {
		      int                 slen;
		      int                 hascolor, iscolor;

		      iscolor = 0;
		      hascolor = 0;
		      tok[0] = 0;
		      col[0] = 0;
		      s[0] = 0;
		      len = strlen(line);
		      strncpy(cmap[j].str, line, cpp);
		      cmap[j].str[cpp] = 0;
		      cmap[j].r = -1;
		      cmap[j].transp = 0;
		      for (k = cpp; k < len; k++)
			{
			  if (line[k] != ' ')
			    {
			      s[0] = 0;
			      sscanf(&line[k], "%65535s", s);
			      slen = strlen(s);
			      k += slen;
			      if (!strcmp(s, "c"))
				iscolor = 1;
			      if ((!strcmp(s, "m")) || (!strcmp(s, "s")) ||
				  (!strcmp(s, "g4")) || (!strcmp(s, "g")) ||
				  (!strcmp(s, "c")) || (k >= len))
				{
				  if (k >= len)
				    {
				      if (col[0])
					strcat(col, " ");
                                      if (strlen(col) + strlen(s) < sizeof(col))
					strcat(col, s);
				    }
				  if (col[0])
				    {
				      if (!strcasecmp(col, "none"))
					{
					  transp = 1;
					  cmap[j].transp = 1;
					}
				      else
					{
					  if ((((cmap[j].r < 0) ||
						(!strcmp(tok, "c"))) &&
					       (!hascolor)))
					    {
					      XParseColor(id->x.disp,
							  id->x.root_cmap,
							  col, &xcol);
					      cmap[j].r = xcol.red >> 8;
					      cmap[j].g = xcol.green >> 8;
					      cmap[j].b = xcol.blue >> 8;
					      if ((cmap[j].r == 255) &&
						  (cmap[j].g == 0) &&
						  (cmap[j].b == 255))
						cmap[j].r = 254;
					      if (iscolor)
						hascolor = 1;
					    }
					}
				    }
				  strcpy(tok, s);
				  col[0] = 0;
				}
			      else
				{
				  if (col[0])
				    strcat(col, " ");
				  strcat(col, s);
				}
			    }
			}
		    }
		  j++;
		  if (j >= ncolors)
		    {
		      if (cpp == 1)
			for (i = 0; i < ncolors; i++)
			  lookup[(int)cmap[i].str[0] - 32][0] = i;
		      if (cpp == 2)
			for (i = 0; i < ncolors; i++)
			  lookup[(int)cmap[i].str[0] - 32][(int)cmap[i].str[1] - 32] = i;
		      context++;
		    }
		}
	      else
		{
		  /* Image Data */
		  i = 0;
		  if (cpp == 0)
		    {
		      /* Chars per pixel = 0? well u never know */
		    }
		  if (cpp == 1)
		    {
		      if (transp)
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      col[0] = line[i];
			      if (cmap[lookup[(int)col[0] - 32][0]].transp)
				{
				  *ptr++ = 255;
				  *ptr++ = 0;
				  *ptr++ = 255;
				}
			      else
				{
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].r;
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].g;
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].b;
				}
			    }
			}
		      else
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      col[0] = line[i];
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].r;
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].g;
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].b;
			    }
			}
		    }
		  else if (cpp == 2)
		    {
		      if (transp)
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      col[0] = line[i++];
			      col[1] = line[i];
			      if (cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].transp)
				{
				  *ptr++ = 255;
				  *ptr++ = 0;
				  *ptr++ = 255;
				}
			      else
				{
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].r;
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].g;
				  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].b;
				}
			    }
			}
		      else
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      col[0] = line[i++];
			      col[1] = line[i];
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].r;
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].g;
			      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].b;
			    }
			}
		    }
		  else
		    {
		      if (transp)
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      for (j = 0; j < cpp; j++, i++)
				{
				  col[j] = line[i];
				}
			      col[j] = 0;
			      i--;
			      for (j = 0; j < ncolors; j++)
				{
				  if (!strcmp(col, cmap[j].str))
				    {
				      if (cmap[j].transp)
					{
					  *ptr++ = 255;
					  *ptr++ = 0;
					  *ptr++ = 255;
					}
				      else
					{
					  *ptr++ = (unsigned char)cmap[j].r;
					  *ptr++ = (unsigned char)cmap[j].g;
					  *ptr++ = (unsigned char)cmap[j].b;
					}
				      j = ncolors;
				    }
				}
			    }
			}
		      else
			{
			  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
			    {
			      for (j = 0; j < cpp; j++, i++)
				{
				  col[j] = line[i];
				}
			      col[j] = 0;
			      i--;
			      for (j = 0; j < ncolors; j++)
				{
				  if (!strcmp(col, cmap[j].str))
				    {
				      *ptr++ = (unsigned char)cmap[j].r;
				      *ptr++ = (unsigned char)cmap[j].g;
				      *ptr++ = (unsigned char)cmap[j].b;
				      j = ncolors;
				    }
				}
			    }
			}
		    }
		}
	    }
	}
      /* Scan in line from XPM file */
      if ((!comment) && (quote) && (c != '"'))
	{
	  if (c < 32)
	    c = 32;
	  else if (c > 127)
	    c = 127;
	  line[i++] = c;
	}
      if (i >= lsz)
	{
	  lsz += 256;
	  line = realloc(line, lsz);
	}
      if ((ptr) && ((ptr - data) >= *w ** h * 3))
	done = 1;
    }
  if (transp)
    *t = 1;
  else
    *t = 0;
  free(cmap);
  free(line);
  return data;
}

unsigned char      *
_LoadPPM(ImlibData * id, FILE * f, int *w, int *h)
{
  int                 done;
  unsigned char      *ptr;
  unsigned char       chr;
  char                s[256];
  int                 a, b, i, j;
  int                 color, scale, ascii, bw;

  a = b = scale = ascii = bw = color = 0;
  fgets(s, 256, f);
  s[2] = 0;
  if (!strcmp(s, "P6"))
    color = 1;
  else if (!strcmp(s, "P5"))
    color = 0;
  else if (!strcmp(s, "P4"))
    bw = 1;
  else if (!strcmp(s, "P3"))
    {
      color = 1;
      ascii = 1;
    }
  else if (!strcmp(s, "P2"))
    {
      ascii = 1;
    }
  else if (!strcmp(s, "P1"))
    {
      ascii = 1;
      bw = 1;
    }
  else
    return NULL;
  done = 1;
  ptr = NULL;
  while (done)
    {
      if (fgets(s, 256, f) == NULL)
	break;

      if (s[0] != '#')
	{
	  done = 0;
	  sscanf(s, "%i %i", w, h);
	  a = *w;
	  b = *h;
	  if (a > 32767)
	    {
	      fprintf(stderr, "IMLIB ERROR: Image width > 32767 pixels for file\n");
	      return NULL;
	    }
	  if (b > 32767)
	    {
	      fprintf(stderr, "IMLIB ERROR: Image height > 32767 pixels for file\n");
	      return NULL;
	    }
	  if (!bw)
	    {
	      fgets(s, 256, f);
	      sscanf(s, "%i", &scale);
	    }
	  else
	    scale = 99999;
	  ptr = (unsigned char *)malloc(a * b * 3);
	  if (!ptr)
	    {
	      fprintf(stderr, "IMLIB ERROR: Cannot allocate RAM for RGB data in file");
	      return ptr;
	    }
	  if ((color) && (!ascii) && (!bw))
	    {
	      fread(ptr, a * b * 3, 1, f);
	    }
	  else if ((!color) && (!ascii) && (!bw))
	    {
	      b = (a * b * 3);
	      a = 0;
	      while ((fread(&chr, 1, 1, f)) && (a < b))
		{
		  ptr[a++] = chr;
		  ptr[a++] = chr;
		  ptr[a++] = chr;
		}
	    }
	  else if ((!color) && (!ascii) && (bw))
	    {
	      b = (a * b * 3);
	      a = 0;
	      j = 0;
	      while ((fread(&chr, 1, 1, f)) && (a < b))
		{
		  for (i = 7; i >= 0; i--)
		    {
		      j++;
		      if (j <= *w)
			{
			  if (chr & (1 << i))
			    {
			      ptr[a++] = 0;
			      ptr[a++] = 0;
			      ptr[a++] = 0;
			    }
			  else
			    {
			      ptr[a++] = 255;
			      ptr[a++] = 255;
			      ptr[a++] = 255;
			    }
			}
		    }
		  if (j >= *w)
		    j = 0;
		}
	    }
	  else if ((color) && (ascii) && (!bw))
	    {
	      b = (a * b * 3);
	      a = 0;
	      i = 0;
	      if (scale != 255)
		{
		  while ((fread(&chr, 1, 1, f)) && (a < b))
		    {
		      s[i++] = chr;
		      if (!isdigit(chr))
			{
			  s[i - 1] = 0;
			  if ((i > 1) && (isdigit(s[i - 2])))
			    {
			      ptr[a++] = ((atoi(s)) * 255) / scale;
			    }
			  i = 0;
			}
		    }
		}
	      else
		{
		  while ((fread(&chr, 1, 1, f)) && (a < b))
		    {
		      s[i++] = chr;
		      if (!isdigit(chr))
			{
			  s[i - 1] = 0;
			  if ((i > 1) && (isdigit(s[i - 2])))
			    {
			      ptr[a++] = atoi(s);
			    }
			  i = 0;
			}
		    }
		}

	    }
	  else if ((!color) && (ascii) && (!bw))
	    {
	      b = (a * b * 3);
	      a = 0;
	      i = 0;
	      if (scale != 255)
		{
		  while ((fread(&chr, 1, 1, f)) && (a < b))
		    {
		      s[i++] = chr;
		      if (!isdigit(chr))
			{
			  s[i - 1] = 0;
			  if ((i > 1) && (isdigit(s[i - 2])))
			    {
			      ptr[a++] = ((atoi(s)) * 255) / scale;
			      ptr[a++] = ptr[a - 1];
			      ptr[a++] = ptr[a - 1];
			    }
			  i = 0;
			}
		    }
		}
	      else
		{
		  while ((fread(&chr, 1, 1, f)) && (a < b))
		    {
		      s[i++] = chr;
		      if (!isdigit(chr))
			{
			  s[i - 1] = 0;
			  if ((i > 1) && (isdigit(s[i - 2])))
			    {
			      ptr[a++] = atoi(s);
			      ptr[a++] = ptr[a - 1];
			      ptr[a++] = ptr[a - 1];
			    }
			  i = 0;
			}
		    }
		}
	    }
	  else if ((!color) && (ascii) && (bw))
	    {
	    }
	}
    }
  if (!ptr)
    return NULL;
  if (scale == 0)
    {
      free(ptr);
      return NULL;
    }
  if ((scale < 255) && (!ascii))
    {
      int                 rot;
      unsigned char      *po;

      if (scale <= 1)
	rot = 7;
      else if (scale <= 3)
	rot = 6;
      else if (scale <= 7)
	rot = 5;
      else if (scale <= 15)
	rot = 4;
      else if (scale <= 31)
	rot = 3;
      else if (scale <= 63)
	rot = 2;
      else
	rot = 1;

      if (rot > 0)
	{
	  po = ptr;
	  while (po < (ptr + (*w ** h * 3)))
	    {
	      *po++ <<= rot;
	      *po++ <<= rot;
	      *po++ <<= rot;
	    }
	}
    }
  return ptr;
}

int
ispnm(FILE *f)
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
isjpeg(FILE *f)
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
ispng(FILE *f)
{
#ifdef HAVE_LIBPNG
  unsigned char       buf[8];

  if (!f)
    return 0;
  fread(buf, 1, 8, f);
  rewind(f);
  return (int)png_check_sig(buf, 8);
#else
  return 0;
#endif
}

int
istiff(FILE *f)
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
iseim(FILE *f)
{
  char                buf[8];

  if (!f)
    return 0;
  fread(buf, 1, 4, f);
  rewind(f);
  buf[4] = 0;
  if (!strncmp("EIM ", buf, 4))
    return 1;
  return 0;
}

int
isgif(FILE *f)
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
isxpm(FILE *f)
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
isbmp(FILE *f)
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

ImlibImage         *
Imlib_load_image(ImlibData * id, char *file)
{
  int                 w, h;
  int                 needs_conv = 1;
  unsigned char      *data;
  ImlibImage         *im;
  char                s[4096];
  char                fil[4096];
  char               *iden;
  char               *e;
  char                cmd[4096];
  FILE               *p;
  int                 eim;
  int                 fmt;
  int                 trans;

  eim = 0;
  fmt = 0;
  data = NULL;

  if (!file)
    return NULL;
  if (id->cache.on_image)
    if ((im = find_image(id, file)))
      {
        if (im->rgb_width == 0 || im->rgb_height == 0)
          {
            Imlib_destroy_image(id, im);
            return NULL;
          }
        else
          return im;
      }
  if (!strcmp(file,"-")) {
  	p = stdin;
  }
  else {
  	p = fopen(file, "rb");
  }
  if (!p)
	return NULL;
  strncpy(fil, file, sizeof(fil));
  iden = _SplitID(fil);
  e = _GetExtension(fil);

  if (ispnm(p))
    {
      needs_conv = 0;
      fmt = 0;
    }
  else if (isjpeg(p))
    {
#ifdef HAVE_LIBJPEG
      needs_conv = 0;
      fmt = 2;
#else
      needs_conv = 1;
      fmt = 0;
#endif
    }
  else if (istiff(p))
    {
#ifdef HAVE_LIBTIFF
      needs_conv = 0;
      fmt = 3;
#else
      needs_conv = 1;
      fmt = 0;
#endif
    }
  else if (iseim(p))
    {
      needs_conv = 0;
      eim = 1;
      fmt = 9999;
    }
  else if (isxpm(p))
    {
      needs_conv = 0;
      fmt = 5;
    }
  else if (ispng(p))
    {
#ifdef HAVE_LIBPNG
      needs_conv = 0;
      fmt = 1;
#else
      needs_conv = 1;
      fmt = 0;
#endif
    }
  else if (isgif(p))
    {
#ifdef HAVE_LIBGIF
      needs_conv = 0;
      fmt = 4;
#else
      needs_conv = 1;
      fmt = 0;
#endif
    }
  else if (isbmp(p))
    {
      needs_conv = 0;
      fmt = 6;
    }
  if ((needs_conv) && (id->fallback))
    {
      if (p != stdin) 
        fclose(p);
      p = open_helper("%C/convert %s pnm:-", fil, "rb");
    }
  trans = 0;
  if ((!eim) && (!data))
    {
      switch (fmt)
	{
	case 6:
	  data = _LoadBMP(id, p, &w, &h, &trans);
	  break;
	case 5:
	  data = _LoadXPM(id, p, &w, &h, &trans);
	  break;
#ifdef HAVE_LIBGIF
	case 4:
	  data = _LoadGIF(id, p, &w, &h, &trans);
	  break;
#endif
#ifdef HAVE_LIBTIFF
	case 3:
	  data = _LoadTIFF(id, p, file, &w, &h, &trans);
	  break;
#endif
#ifdef HAVE_LIBJPEG
	case 2:
	    data = _LoadJPEG(id, p, &w, &h);
	  break;
#endif
#ifdef HAVE_LIBPNG
	case 1:
	    data = _LoadPNG(id, p, &w, &h, &trans);
	  break;
#endif
	default:
	    data = _LoadPPM(id, p, &w, &h);
	  break;
	}
    }
  if ((p) && (!needs_conv))
    {
      if (p != stdin) 
        fclose(p);
    }
  else if (p)
    close_helper(p);

  if ((!data) && (id->fallback))
    {
      p = open_helper("%C/convert %s pnm:-", fil, "rb");
      if (p)
	{
	  data = _LoadPPM(id, p, &w, &h);
	  close_helper(p);
	}
    }
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
      p = open_helper(cmd, fil, "rb");
      if (p)
	{
	  data = _LoadPPM(id, p, &w, &h);
	  close_helper(p);
	}
    }

  if ((!eim) && (!data))
    {
      fprintf(stderr, "IMLIB ERROR: Cannot load image: %s\nAll fallbacks failed.\n", fil);
      return NULL;
    }
    
  if (!w || !h)
    {
      fprintf(stderr, "IMLIB ERROR: zero image\n" );
      if(data)
        free(data);
      return NULL;
    }
    
  im = (ImlibImage *) malloc(sizeof(ImlibImage));
  if (!im)
    {
      fprintf(stderr, "IMLIB ERROR: Cannot allocate RAM for image structure\n");
      if (data)
	free(data);
      return NULL;
    }
  im->alpha_data = NULL;
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
  im->pixmap = 0;
  im->shape_mask = 0;
  if (eim)
    {
      char                s1[256], s2[256];
      int                 num, size;
      int                 r, g, b;
      int                 br, bl, bt, bb;

      /* Load Native-as-can-be EIM format (Enlightenment IMlib format) */
      if (!strcmp(file,"-"))
        p = stdin;
      else
        p = fopen(fil, "r");
      if (!p)
	{
	  free(im);
	  return NULL;
	}
      fgets(s, 4096, p);
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
      while (fgets(s, 4096, p))
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
      if (iden[0])
	{
	  strncat(fil, ":", sizeof(fil) - strlen(fil));
	  strncat(fil, iden, sizeof(fil) - strlen(fil));
	}
    }
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
  if ((id->cache.on_image) && (im))
    add_image(id, im, fil);
  calc_map_tables(id, im);
  return im;
}

int
Imlib_save_image_to_eim(ImlibData * id, ImlibImage * im, char *file)
{
  char                fil[4096];
  char               *iden;
  FILE               *f;
  int                 size;

  if ((!id) || (!im) || (!file))
    return 0;
  strncpy(fil, file, sizeof(fil));
  iden = _SplitID(fil);
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

int
Imlib_add_image_to_eim(ImlibData * id, ImlibImage * im, char *file)
{
  char                fil[4096];
  char               *iden;
  FILE               *f;
  int                 size;

  if ((!id) || (!im) || (!file))
    return 0;
  strncpy(fil, file, sizeof(fil));

  iden = _SplitID(file);
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

int
Imlib_save_image_to_ppm(ImlibData * id, ImlibImage * im, char *file)
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
