#define _GNU_SOURCE
#include <config.h>
#include "gdk_imlib.h"
#define id _gdk_imlib_data
#include "gdk_imlib_private.h"

gint
gdk_imlib_save_image(GdkImlibImage * im, char *file, GdkImlibSaveInfo * info)
{
  char               *ext;
  char                cmd[10240];
  FILE               *f;
  GdkImlibSaveInfo    defaults;

  if (!im || !file)
    return 0;

  defaults.quality = 208;
  defaults.scaling = 1024;
  defaults.xjustification = 512;
  defaults.yjustification = 512;
  defaults.page_size = PAGE_SIZE_LETTER;
  defaults.color = 1;

  if (!info)
    info = &defaults;
  ext = _gdk_imlib_GetExtension(file);

  if ((!strcasecmp(ext, "ppm")) || (!strcasecmp(ext, "pnm")) || (!strcasecmp (ext, "pgm")))
    {
      return _gdk_imlib_SavePPM (im, file, info);
    }
  else if (!strcasecmp(ext, "ps"))
    {
	    return _gdk_imlib_SavePS (im, file, info);
    }
  else if ((!strcasecmp(ext, "jpeg")) || (!strcasecmp(ext, "jpg")))
    {
	    return _gdk_imlib_SaveJPEG (im, file, info);
    }
  else if (!strcasecmp(ext, "png"))
    {
	    return _gdk_imlib_SavePNG (im, file, info);
    }
  else if ((!strcasecmp(ext, "tiff")) || (!strcasecmp(ext, "tif")))
    {
	    return _gdk_imlib_SaveTIFF (im, file, info);
    }
  if (id->fallback)
    {
      f = _gdk_imlib_open_helper("%C/convert pnm:- %s", file, "wb");
      if (f)
	{
	  if (!fprintf(f, "P6\n# Created by Imlib\n%i %i\n255\n", im->rgb_width, im->rgb_height))
	    {
	      _gdk_imlib_close_helper(f);
	      return 0;
	    }
	  if (!fwrite(im->rgb_data, 1, (im->rgb_width * im->rgb_height * 3), f))
	    {
	      _gdk_imlib_close_helper(f);
	      return 0;
	    }
	  if (_gdk_imlib_close_helper(f))
	    return 0;
	  return 1;
	}

      if (!strcasecmp(ext, "jpeg"))
	g_snprintf(cmd, sizeof(cmd), "%%H -quality %i -progressive -outfile %%s", 100 * info->quality / 256);
      else if (!strcasecmp(ext, "jpg"))
	g_snprintf(cmd, sizeof(cmd), "%%H -quality %i -progressive -outfile %%s", 100 * info->quality / 256);
      else if (!strcasecmp(ext, "bmp"))
	strcpy(cmd, "%Q %N/ppmtobmp > %s");
      else if (!strcasecmp(ext, "gif"))
	strcpy(cmd, "%Q %N/ppmtogif -interlace > %s");
      else if (!strcasecmp(ext, "ilbm"))
	strcpy(cmd, "%N/ppmtoilbm -24if -hires -lace -compress > %s");
      else if (!strcasecmp(ext, "ilb"))
	strcpy(cmd, "%N/ppmtoilbm -24if -hires -lace -compress > %s");
      else if (!strcasecmp(ext, "iff"))
	strcpy(cmd, "%N/ppmtoilbm -24if -hires -lace -compress > %s");
      else if (!strcasecmp(ext, "icr"))
	strcpy(cmd, "%N/ppmtoicr > %s");
      else if (!strcasecmp(ext, "map"))
	strcpy(cmd, "%N/ppmtomap > %s");
      else if (!strcasecmp(ext, "mit"))
	strcpy(cmd, "%N/ppmtomitsu -sharpness 4 > %s");
      else if (!strcasecmp(ext, "mitsu"))
	strcpy(cmd, "%N/ppmtomitsu -sharpness 4 > %s");
      else if (!strcasecmp(ext, "pcx"))
	strcpy(cmd, "%N/ppmtopcx -24bit -packed > %s");
      else if (!strcasecmp(ext, "pgm"))
	strcpy(cmd, "%N/ppmtopgm > %s");
      else if (!strcasecmp(ext, "pi1"))
	strcpy(cmd, "%N/ppmtopi1 > %s");
      else if (!strcasecmp(ext, "pic"))
	strcpy(cmd, "%Q %N/ppmtopict > %s");
      else if (!strcasecmp(ext, "pict"))
	strcpy(cmd, "%Q %N/ppmtopict > %s");
      else if (!strcasecmp(ext, "pj"))
	strcpy(cmd, "%N/ppmtopj > %s");
      else if (!strcasecmp(ext, "pjxl"))
	strcpy(cmd, "%N/ppmtopjxl > %s");
      else if (!strcasecmp(ext, "puz"))
	strcpy(cmd, "%N/ppmtopuzz > %s");
      else if (!strcasecmp(ext, "puzz"))
	strcpy(cmd, "%N/ppmtopuzz > %s");
      else if (!strcasecmp(ext, "rgb3"))
	strcpy(cmd, "%N/ppmtorgb3 > %s");
      else if (!strcasecmp(ext, "six"))
	strcpy(cmd, "%N/ppmtosixel > %s");
      else if (!strcasecmp(ext, "sixel"))
	strcpy(cmd, "%N/ppmtosizel > %s");
      else if (!strcasecmp(ext, "tga"))
	strcpy(cmd, "%N/ppmtotga -rgb > %s");
      else if (!strcasecmp(ext, "targa"))
	strcpy(cmd, "%N/ppmtotga -rgb > %s");
      else if (!strcasecmp(ext, "uil"))
	strcpy(cmd, "%N/ppmtouil > %s");
      else if (!strcasecmp(ext, "xpm"))
	strcpy(cmd, "%Q %N/ppmtoxpm > %s");
      else if (!strcasecmp(ext, "yuv"))
	strcpy(cmd, "%N/ppmtoyuv > %s");
      else if (!strcasecmp(ext, "png"))
	strcpy(cmd, "%N/pnmtopng > %s");
      else if (!strcasecmp(ext, "ps"))
	strcpy(cmd, "%N/pnmtops -center -scale 100 > %s");
      else if (!strcasecmp(ext, "rast"))
	strcpy(cmd, "%N/pnmtorast -rle > %s");
      else if (!strcasecmp(ext, "ras"))
	strcpy(cmd, "%N/pnmtorast -rle > %s");
      else if (!strcasecmp(ext, "sgi"))
	strcpy(cmd, "%N/pnmtosgi > %s");
      else if (!strcasecmp(ext, "sir"))
	strcpy(cmd, "%N/pnmtosir > %s");
      else if (!strcasecmp(ext, "tif"))
	strcpy(cmd, "%N/pnmtotiff -lzw > %s");
      else if (!strcasecmp(ext, "tiff"))
	strcpy(cmd, "%N/pnmtotiff -lzw > %s");
      else if (!strcasecmp(ext, "xwd"))
	strcpy(cmd, "%N/pnmtoxwd > %s");
      else
	ext = "";
      if (ext[0])
	{
	  f = _gdk_imlib_open_helper(cmd, file, "wb");
	  if (f)
	    {
	      if (!fprintf(f, "P6\n# Created by Imlib\n%i %i\n255\n", im->rgb_width, im->rgb_height))
		{
		  _gdk_imlib_close_helper(f);
		  return 0;
		}
	      if (!fwrite(im->rgb_data, 1, (im->rgb_width * im->rgb_height * 3), f))
		{
		  _gdk_imlib_close_helper(f);
		  return 0;
		}
	      if (_gdk_imlib_close_helper(f))
		return 0;
	      return 1;
	    }
	}
    }
  return 0;
}
