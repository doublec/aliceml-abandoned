#include <config.h>
#include <setjmp.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

unsigned char      *
loader_xpm(FILE *file, int *w, int *h, int *t)
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
  short               lookup[128 - 32][128 - 32];

  transp = 0;
  done = 0;

  if (!file)
    return NULL;

  *w = 10;
  *h = 10;

  ptr = NULL;
  end = NULL;
  data = NULL;
  c = ' ';
  comment = 0;
  quote = 0;
  context = 0;
  i = j = 0;
  cmap = NULL;

  line = malloc(lsz);
  while (!done)
    {
      pc = c;
      c = fgetc(file);
      if (c == EOF)
	break;
      if (!quote)
	{
	  if (pc == '/' && c == '*')
	    comment = 1;
	  else if (pc == '*' && c == '/' && comment)
	    comment = 0;
	}
      if (!comment)
	{
	  if (!quote && c == '"')
	    {
	      quote = 1;
	      i = 0;
	    }
	  else if (quote && c == '"')
	    {
	      line[i] = 0;
	      quote = 0;
	      if (context == 0)
		{
		  /* Header */
		  sscanf(line, "%i %i %i %i", w, h, &ncolors, &cpp);
		  if (ncolors > 32766)
		    {
		      fprintf(stderr, "gdk_imlib ERROR: XPM files wth colors > 32766 not supported\n");
		      free(line);
		      return NULL;
		    }
		  if (cpp > 5)
		    {
		      fprintf(stderr, "gdk_imlib ERROR: XPM files with characters per pixel > 5 not supported\n");
		      free(line);
		      return NULL;
		    }
		  if (*w > 32767)
		    {
		      fprintf(stderr, "gdk_imlib ERROR: Image width > 32767 pixels for file\n");
		      free(line);
		      return NULL;
		    }
		  if (*h > 32767)
		    {
		      fprintf(stderr, "gdk_imlib ERROR: Image height > 32767 pixels for file\n");
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
		      free(line);
		      free(cmap);
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

		      hascolor = 0;
		      iscolor = 0;
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
					  if (((cmap[j].r < 0) ||
					       (!strcmp(tok, "c"))) &&
					      (!hascolor))
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
		      else if (cpp == 2)
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
      /* Scan in line from XPM file*/
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

