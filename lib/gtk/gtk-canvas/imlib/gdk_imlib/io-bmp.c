#include <config.h>
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

unsigned char      *
loader_bmp (FILE *file, int *w, int *h, int *t)
{
  unsigned char      *data, *ptr;
  int                 done, i, bpp, planes, comp, ncolors, line, column,
                      linesize, linepos, rshift = 0, gshift = 0, bshift = 0;
  unsigned char       byte;
  short int           word;
  long int            dbuf[4], dword, rmask = 0, gmask = 0, bmask = 0, offset,
                      size;
  signed char         bbuf[4];
  struct _cmap
    {
      unsigned char       r, g, b;
    }
                     *cmap = NULL;

#define BI_RGB       0
#define BI_RLE8      1
#define BI_RLE4      2
#define BI_BITFIELDS 3

  if (!file)
    return NULL;

  done = 0;
  /* 
   * Reading the bmp header 
   */

  fread(&bbuf, 1, 2, file);

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
   * REading the image data
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
		      unsigned char       first;

		      first = byte;
		      byte = getc(file);
		      if (first == 0)
			{
			  if (byte == 0)
			    {
/*                                    column = *w; */
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
		      linepos += size;
		    }
		}
	    }
	  else if (bpp == 24)
	    {
	      linepos += fread(&bbuf, 1, 3, file);
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

