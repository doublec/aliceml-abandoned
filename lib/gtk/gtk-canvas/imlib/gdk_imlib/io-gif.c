#include <config.h>
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

#ifndef HAVE_LIBGIF
unsigned char loader_gif (FILE *f, int *w, int *h, int *t)
{
	return NULL;
}
#else
#include <gif_lib.h>

unsigned char      *
loader_gif(FILE *f, int *w, int *h, int *t)
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

  fd = fileno(f);
  /* Apparently rewind(f) isn't sufficient */
  lseek(fd, (long) 0, 0);
  gif = DGifOpenFileHandle(fd);

  transp = -1;
  data = NULL;
  rows = NULL;

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
		  if (r == 255 && g == 0 && b == 255)
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

#endif
