#include <config.h>
#include <setjmp.h>
#define id _gdk_imlib_data
#include "gdk_imlib.h"
#include "gdk_imlib_private.h"

unsigned char      *
loader_ppm (FILE * f, int *w, int *h, int *t)
{
  int                 done;
  unsigned char      *ptr;
  unsigned char       chr;
  char                s[256];
  int                 a, b, i, j;
  int                 color, scale, ascii, bw;

  *t = 0;
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
	      fprintf(stderr, "gdk_imlib ERROR: Image width > 32767 pixels for file\n");
	      return NULL;
	    }
	  if (b > 32767)
	    {
	      fprintf(stderr, "gdk_imlib ERROR: Image height > 32767 pixels for file\n");
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
	      fprintf(stderr, "gdk_imlib ERROR: Cannot allocate RAM for RGB data in file");
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

gint
saver_ppm (GdkImlibImage *im, char *file, GdkImlibSaveInfo *info)
{
      char *ext = _gdk_imlib_GetExtension (file);
      int                 x, y;
      unsigned char      *ptr, val;
      int                 v;
      FILE *f;
      
      f = fopen(file, "wb");
      if (!f)
	      return 0;
      
      if (strcmp (ext, "pgm") == 0){
	      if (!fprintf(f, "P5\n# Created by Imlib\n%i %i\n255\n", im->rgb_width, im->rgb_height))
	      {
		      fclose(f);
		      return 0;
	      }
	      ptr = im->rgb_data;
	      for (y = 0; y < im->rgb_height; y++)
	      {
		      for (x = 0; x < im->rgb_width; x++)
		      {
			      v = (int)(*ptr++);
			      v += (int)(*ptr++);
				v += (int)(*ptr++);
				val = (unsigned char)(v / 3);
				if (!fwrite(&val, 1, 1, f))
				{
					fclose(f);
					return 0;
				}
		      }
	      }
	      fclose(f);
	      return 1;
      } else {
    	      if (!fprintf(f, "P6\n# Created by Imlib\n%i %i\n255\n", im->rgb_width, im->rgb_height))
	      {
		      fclose(f);
		      return 0;
	      }
    	      if (!fwrite(im->rgb_data, 1, (im->rgb_width * im->rgb_height * 3), f))
	      {
		      fclose(f);
		      return 0;
	      }
    	      fclose(f);
    	      return 1;
      }
}
