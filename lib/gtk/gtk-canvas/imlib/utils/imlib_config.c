#include <config.h>
#ifndef GNOMELOCALEDIR
#define GNOMELOCALEDIR "/usr/share/locale"
#endif
#include <locale.h>
#ifdef ENABLE_NLS
#    include <libintl.h>
#    define _(String) gettext (String)
#    ifdef gettext_noop
#        define N_(String) gettext_noop (String)
#    else
#        define N_(String) (String)
#    endif
#else
/* Stubs that do something close enough.  */
#    define textdomain(String) (String)
#    define gettext(String) (String)
#    define dgettext(Domain,Message) (Message)
#    define dcgettext(Domain,Message,Type) (Message)
#    define bindtextdomain(Domain,Directory) (Domain)
#    define _(String) (String)
#    define N_(String) (String)
#endif

#include <gdk/gdk.h>
#include <gdk_imlib.h>
#include <gtk/gtk.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <gdk/gdkprivate.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

GtkWidget          *note = NULL;
GtkWidget          *pg1 = NULL;
GtkWidget          *pg2 = NULL;
GtkWidget          *pg3 = NULL;
GtkWidget          *pg4 = NULL;
GtkWidget          *win = NULL;
GdkPixmap          *brightness_pmap = NULL;
GdkPixmap          *brightness_mask = NULL;
GdkPixmap          *gamma_pmap = NULL;
GdkPixmap          *gamma_mask = NULL;
GdkPixmap          *contrast_pmap = NULL;
GdkPixmap          *contrast_mask = NULL;
GtkWidget          *list = NULL;
GtkWidget          *filesel = NULL;
GtkWidget          *pal_text = NULL;
GdkImlibImage      *test_im = NULL;
GtkWidget          *test_area = NULL;
int                 save;
int                 up;
int                 vis_num;

extern int          testimg_x;
extern int          testimg_y;
extern unsigned char testimg[];
extern unsigned char brightness_icon[];
extern unsigned char gamma_icon[];
extern unsigned char contrast_icon[];

struct _conf
  {
     char               *palettefile;
     char                paletteoverride;
     char                dither;
     char                remap;
     char                highquality;
     char                mitshm;
     char                shmmax;
     int                 shmmaxsize;
     char                sharedpixmap;
     char                fastrender;
     char                imagecache;
     int                 imagecachesize;
     char                pixmapcache;
     int                 pixmapcachesize;
     char                forcevisualid;
     int                 visualid;
     char                fallback;
     char                ordered;
     GdkImlibColorModifier mod, rmod, gmod, bmod;
  }
conf;

struct _val
  {
     int                *setting;
     int                 value;
     GtkWidget          *text;
  };

struct _vint
  {
     GtkObject          *adj;
     GtkWidget          *range;
     int                *value;
  };

struct _color
  {
     int                 number;
     int                 color[256][3];
     int                 cur_color;
     char                copy;
     char                spread;
     char                del;
     char                setlast;
     GdkImlibImage      *pal_im;
     GdkPixmap          *pmap;
     GtkWidget          *pal_widget;
     GtkWidget          *col_widget;
  }
color;

struct _vis
  {
     int                 id;
     int                 depth;
     char               *name;
  }
                   *vislist;

GdkImlibImage      *
make_pal_image()
{
   GdkImlibImage      *im;
   unsigned char      *dat;

   dat = malloc(256 * 3);
   im = gdk_imlib_create_image_from_data(dat, NULL, 16, 16);
   free(dat);
   return im;
}

void
make_test_image()
{
   test_im = gdk_imlib_create_image_from_data(testimg, NULL, testimg_x, testimg_y);
}

void
kill_win(GtkWidget * widget, gpointer * data)
{
   exit(0);
}

void
col_copy(GtkWidget * widget, gpointer * data)
{
   color.copy = 1;
}

void
col_spread(GtkWidget * widget, gpointer * data)
{
   color.spread = 1;
}

void
col_del(GtkWidget * widget, gpointer * data)
{
   color.del = 1;
}

void
col_setlast(GtkWidget * widget, gpointer * data)
{
   color.setlast = 1;
}

void
make_win()
{
   GtkWidget          *box1;

   win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
   gtk_signal_connect(GTK_OBJECT(win), "delete_event",
		      GTK_SIGNAL_FUNC(kill_win), NULL);
   gtk_container_set_border_width(GTK_CONTAINER(win), 4);
   gtk_window_set_policy(GTK_WINDOW(win), 0, 0, 1);
   gtk_window_set_title(GTK_WINDOW(win), _("Imlib Config Editor"));
   gtk_window_set_wmclass(GTK_WINDOW(win), "Imlib Config Editor", "Config");

   box1 = gtk_vbox_new(FALSE, 0);
   gtk_container_add(GTK_CONTAINER(win), box1);
   gtk_widget_show(box1);
   list = box1;
}

void
toggle_onoff(GtkWidget * widget, char *value)
{
   if (!up)
      return;
   if (*value)
      *value = 0;
   else
      *value = 1;
}

void
redraw_palette(void)
{
  int                 i, j;
  unsigned char      *ptr;
  GdkGC              *gc;
  GdkColor            clr;
  GdkColormap        *cmap;
  
  if (color.pmap)
    gdk_imlib_free_pixmap(color.pmap);
  ptr = color.pal_im->rgb_data;
  for (i = 0; i < 256; i++)
    {
      for (j = 0; j < 3; j++)
	{
	  *ptr++ = color.color[i][j];
	}
    }
  gdk_imlib_render(color.pal_im, 128, 128);
  color.pmap = gdk_imlib_move_image(color.pal_im);
  gdk_window_set_back_pixmap(color.pal_widget->window, color.pmap, FALSE);
  gdk_imlib_changed_image(color.pal_im);
  gdk_window_clear(color.pal_widget->window);
  gc = gdk_gc_new(color.pal_widget->window);
  cmap = gdk_colormap_get_system();
  gdk_color_white(cmap, &clr);
  gdk_gc_set_foreground(gc, &clr);
  gdk_gc_set_function(gc, GDK_XOR);
  gdk_draw_rectangle(color.pal_widget->window, gc, 0,
		     (color.cur_color % 16) * 8, (color.cur_color & 0xf0) / 2,
		     7, 7);
  gdk_gc_destroy(gc);
  gdk_flush();
}

void
change_col(GtkWidget * widget, GtkWidget * w)
{
   gdouble             col[4];
   int                 r, g, b;

   gtk_color_selection_get_color(GTK_COLOR_SELECTION(w), col);
   if (color.cur_color >= 0)
     {
	r = (int)(col[0] * (gdouble) 255);
	g = (int)(col[1] * (gdouble) 255);
	b = (int)(col[2] * (gdouble) 255);
	if ((r != color.color[color.cur_color][0]) ||
	    (g != color.color[color.cur_color][1]) ||
	    (b != color.color[color.cur_color][2]))
	  {
	     color.color[color.cur_color][0] = r;
	     color.color[color.cur_color][1] = g;
	     color.color[color.cur_color][2] = b;
	  }
     }
  redraw_palette();
}

void
update_test()
{
   GdkPixmap          *pmap;

   gdk_imlib_set_image_modifier(test_im, &conf.mod);
   gdk_imlib_set_image_red_modifier(test_im, &conf.rmod);
   gdk_imlib_set_image_green_modifier(test_im, &conf.gmod);
   gdk_imlib_set_image_blue_modifier(test_im, &conf.bmod);
   gdk_imlib_render(test_im, testimg_x, testimg_y);
   pmap = gdk_imlib_move_image(test_im);
   gdk_window_set_back_pixmap(test_area->window, pmap, FALSE);
   gdk_window_clear(test_area->window);
   gdk_imlib_free_pixmap(pmap);
}

void
change_val(GtkWidget * widget, struct _val *v)
{
   char                s[256];

   *(v->setting) = v->value;
   g_snprintf(s, 256, "%i", *(v->setting));
   gtk_entry_set_text(GTK_ENTRY(v->text), s);
}

void
enter_val(GtkWidget * widget, struct _val *v)
{
   char               *s;

   s = gtk_entry_get_text(GTK_ENTRY(v->text));
   *(v->setting) = atoi(s);
}

void
pal_press(GtkWidget * widget, GdkEventButton * event)
{
   int                 x, y, n, nn, i, ii;
   gdouble             col[4];
   unsigned char      *ptr;

   x = (event->x) / 8;
   y = (event->y) / 8;
   n = color.cur_color;
   color.cur_color = (y * 16) + x;
   if (color.copy)
     {
	color.color[color.cur_color][0] = color.color[n][0];
	color.color[color.cur_color][1] = color.color[n][1];
	color.color[color.cur_color][2] = color.color[n][2];
     }
   else if (color.spread)
     {
	if (n < color.cur_color)
	  {
	     nn = color.cur_color - n;
	     for (i = 0; i < nn; i++)
	       {
		  ii = n + i;
		  color.color[ii][0] = ((color.color[n][0] * (nn - i)) + (color.color[color.cur_color][0] * i)) / nn;
		  color.color[ii][1] = ((color.color[n][1] * (nn - i)) + (color.color[color.cur_color][1] * i)) / nn;
		  color.color[ii][2] = ((color.color[n][2] * (nn - i)) + (color.color[color.cur_color][2] * i)) / nn;
	       }
	  }
	else
	  {
	     nn = n - color.cur_color;
	     for (i = 0; i < nn; i++)
	       {
		  ii = color.cur_color + i;
		  color.color[ii][0] = ((color.color[n][0] * i) + (color.color[color.cur_color][0] * (nn - i))) / nn;
		  color.color[ii][1] = ((color.color[n][1] * i) + (color.color[color.cur_color][1] * (nn - i))) / nn;
		  color.color[ii][2] = ((color.color[n][2] * i) + (color.color[color.cur_color][2] * (nn - i))) / nn;
	       }
	  }
     }
   else if (color.del)
     {
	ptr = color.pal_im->rgb_data;
	for (i = color.cur_color; i < 255; i++)
	  {
	     color.color[i][0] = color.color[i + 1][0];
	     color.color[i][1] = color.color[i + 1][1];
	     color.color[i][2] = color.color[i + 1][2];
	  }
	color.color[255][0] = 0;
	color.color[255][1] = 0;
	color.color[255][2] = 0;
	color.number--;
	if (color.number < 0)
	   color.number = 0;
     }
   else if (color.setlast)
     {
	color.number = color.cur_color + 1;
	for (i = color.number; i < 256; i++)
	  {
	     color.color[i][0] = 0;
	     color.color[i][1] = 0;
	     color.color[i][2] = 0;
	  }
     }
   col[0] = ((gdouble) color.color[color.cur_color][0]) / 255;
   col[1] = ((gdouble) color.color[color.cur_color][1]) / 255;
   col[2] = ((gdouble) color.color[color.cur_color][2]) / 255;
   col[3] = 0;
   color.copy = 0;
   color.setlast = 0;
   color.spread = 0;
   color.del = 0;
   gtk_color_selection_set_color(GTK_COLOR_SELECTION(color.col_widget), col);
   change_col(NULL, color.col_widget);
}

void
showhide(GtkWidget * widget, GtkWidget * w)
{
   if (GTK_WIDGET_VISIBLE(w))
      gtk_widget_hide(w);
   else
     {
	gtk_window_set_position(GTK_WINDOW(w), GTK_WIN_POS_MOUSE);
	gtk_widget_show(w);
     }
}

void
change_str(GtkWidget * widget, GtkWidget * w)
{
   char               *s;

   s = gtk_entry_get_text(GTK_ENTRY(w));
   if (conf.palettefile)
      free(conf.palettefile);
   conf.palettefile = NULL;
   conf.palettefile = strdup(s);
}

void
pal_load(GtkWidget * widget, GtkWidget * w)
{
   if (conf.palettefile)
      gtk_file_selection_set_filename(GTK_FILE_SELECTION(w), conf.palettefile);
  showhide(NULL, w);
}

void
pal_save(GtkWidget * widget, GtkWidget * w)
{
   save = 1;
   if (conf.palettefile)
      gtk_file_selection_set_filename(GTK_FILE_SELECTION(w), conf.palettefile);
   showhide(NULL, w);
}

void
load_pal(char *f)
{
   FILE               *ff;
   int                 r, g, b, i;
   char                s[1024];

  setlocale(LC_ALL, "C");
  ff = fopen(f, "r");
   if (ff)
     {
	color.number = 0;
	while (fgets(s, 256, ff))
	  {
	     sscanf(s, "%x %x %x", &r, &g, &b);
	     if (r < 0)
		r = 0;
	     if (r > 255)
		r = 255;
	     if (g < 0)
		g = 0;
	     if (g > 255)
		g = 255;
	     if (b < 0)
		b = 0;
	     if (b > 255)
		b = 255;
	     color.color[color.number][0] = r;
	     color.color[color.number][1] = g;
	     color.color[color.number][2] = b;
	     color.number++;
	     if (color.number > 255)
		break;
	  }
	fclose(ff);
     }
  for (i = color.number; i < 256; i++)
    {
      color.color[i][0] = 0;
      color.color[i][1] = 0;
      color.color[i][2] = 0;
     }
  setlocale(LC_ALL, "");
}

void
save_pal(char *f)
{
   FILE               *ff;
   int                 i;

  setlocale(LC_ALL, "C");
  ff = fopen(f, "w");
   if (ff)
     {
	for (i = 0; i < color.number; i++)
	   fprintf(ff, "0x%x 0x%x 0x%x\n", color.color[i][0], color.color[i][1], color.color[i][2]);
	fclose(ff);
     }
  setlocale(LC_ALL, "");
}

void
read_imrc(FILE * f)
{
   char                s[2048];
   char                s1[1024], s2[1024];

   if (!f)
      return;
   if (conf.palettefile)
      free(conf.palettefile);
   conf.palettefile = NULL;
   conf.paletteoverride = 0;
   conf.dither = 1;
   conf.remap = 1;
   conf.highquality = 0;
   conf.mitshm = 1;
   conf.shmmax = 0;
   conf.shmmaxsize = 1000000;
   conf.sharedpixmap = 0;
   conf.fastrender = 1;
   conf.imagecache = 1;
   conf.imagecachesize = 4000000;
   conf.pixmapcache = 1;
   conf.pixmapcachesize = 40000000;
   conf.forcevisualid = 0;
   conf.visualid = -1;
   conf.fallback = 1;

   while (fgets(s, 2048, f))
     {
	if (s[0] != '#')
	  {
	     if (sscanf(s, "%1000s %1000s", s1, s2) != 2)
	       continue;
	     
	     if (!strcasecmp(s1, "PaletteFile"))
	       {
		  conf.palettefile = strdup(s2);
	       }
	     if (!strcasecmp(s1, "PaletteOverride"))
	       {
		  if (!strcasecmp(s2, "yes"))
		     conf.paletteoverride = 1;
		  else
		     conf.paletteoverride = 0;
	       }
	     if (!strcasecmp(s1, "Dither"))
	       {
		  if (!strcasecmp(s2, "yes"))
		     conf.dither = 1;
		  else
		     conf.dither = 0;
	       }
	     if (!strcasecmp(s1, "Remap"))
	       {
		  if (!strcasecmp(s2, "fast"))
		     conf.remap = 1;
		  else
		     conf.remap = 0;
	       }
	     if (!strcasecmp(s1, "HighQuality"))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.highquality = 1;
		  else
		     conf.highquality = 0;
	       }
	     if (!strcasecmp(s1, "Mit-Shm"))
	       {
#ifdef HAVE_SHM
		  if (!strcasecmp(s2, "on"))
		     conf.mitshm = 1;
		  else
#endif
		     conf.mitshm = 0;
	       }
	     if (!strcasecmp(s1, "SharedPixmaps"))
	       {
#ifdef HAVE_SHM
		  if (!strcasecmp(s2, "on"))
		     conf.sharedpixmap = 1;
		  else
#endif
		     conf.sharedpixmap = 0;
	       }
	     if (!strcasecmp(s1, "FastRender"))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.fastrender = 1;
		  else
		     conf.fastrender = 0;
	       }
	     if (!strcasecmp(s1, "Shm_Max_Size"))
	       {
		  conf.shmmax = 1;
		  conf.shmmaxsize = atoi(s2);
	       }
	     if (!strcasecmp(s1, "Image_Cache"))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.imagecache = 1;
		  else
		     conf.imagecache = 0;
	       }
	     if (!strcasecmp(s1, "Image_Cache_Size"))
	       {
		  conf.imagecachesize = atoi(s2);
	       }
	     if (!strcasecmp(s1, "Pixmap_Cache"))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.pixmapcache = 1;
		  else
		     conf.pixmapcache = 0;
	       }
	     if (!strcasecmp(s1, "Pixmap_Cache_Size"))
	       {
		  conf.pixmapcachesize = atoi(s2);
	       }
	     if (!strcasecmp(s1, "ForceVisualID"))
	       {
		  conf.forcevisualid = 1;
		  conf.visualid = atoi(s2);
	       }
	     if (!strcasecmp(s1, "Fallback"))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.fallback = 1;
		  else
		     conf.fallback = 0;
	       }
	     if (!strcasecmp("Gamma", s1))
	       {
		  conf.mod.gamma = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Brightness", s1))
	       {
		  conf.mod.brightness = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Contrast", s1))
	       {
		  conf.mod.contrast = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Red_Gamma", s1))
	       {
		  conf.rmod.gamma = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Red_Brightness", s1))
	       {
		  conf.rmod.brightness = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Red_Contrast", s1))
	       {
		  conf.rmod.contrast = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Green_Gamma", s1))
	       {
		  conf.gmod.gamma = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Green_Brightness", s1))
	       {
		  conf.gmod.brightness = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Green_Contrast", s1))
	       {
		  conf.gmod.contrast = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Blue_Gamma", s1))
	       {
		  conf.bmod.gamma = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Blue_Brightness", s1))
	       {
		  conf.bmod.brightness = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Blue_Contrast", s1))
	       {
		  conf.bmod.contrast = (int)(256.0 * atof(s2));
	       }
	     if (!strcasecmp("Ordered_Dither", s1))
	       {
		  if (!strcasecmp(s2, "on"))
		     conf.ordered = 1;
		  else
		     conf.ordered = 0;
	       }
	  }
     }
   if (conf.palettefile)
      load_pal(conf.palettefile);
   fclose(f);
}

void
write_imrc(FILE * f)
{
   if (!f)
      return;

   if (conf.palettefile)
      fprintf(f, "PaletteFile %s\n", conf.palettefile);
   if (conf.paletteoverride)
      fprintf(f, "PaletteOverride yes\n");
   else
      fprintf(f, "PaletteOverride no\n");
   if (conf.dither)
      fprintf(f, "Dither yes\n");
   else
      fprintf(f, "Dither no\n");
   if (conf.remap)
      fprintf(f, "Remap fast\n");
   else
      fprintf(f, "Remap slow\n");
   if (conf.highquality)
      fprintf(f, "HighQuality on\n");
   else
      fprintf(f, "HighQuality off\n");
   if (conf.mitshm)
      fprintf(f, "Mit-Shm on\n");
   else
      fprintf(f, "Mit-Shm off\n");
   if (conf.sharedpixmap)
      fprintf(f, "SharedPixmaps on\n");
   else
      fprintf(f, "SharedPixmaps off\n");
   if (conf.fastrender)
      fprintf(f, "FastRender on\n");
   else
      fprintf(f, "FastRender off\n");
   if (conf.shmmax)
      fprintf(f, "Shm_Max_Size %i\n", conf.shmmaxsize);
   if (conf.imagecache)
      fprintf(f, "Image_Cache on\n");
   else
      fprintf(f, "Image_Cache off\n");
   fprintf(f, "Image_Cache_Size %i\n", conf.imagecachesize);
   if (conf.pixmapcache)
      fprintf(f, "Pixmap_Cache on\n");
   else
      fprintf(f, "Pixmap_Cache off\n");
   fprintf(f, "Pixmap_Cache_Size %i\n", conf.pixmapcachesize);
   if (conf.forcevisualid)
      fprintf(f, "ForceVisualID %x\n", conf.visualid);
   if (conf.fallback)
      fprintf(f, "Fallback on\n");
   else
      fprintf(f, "Fallback off\n");
   fprintf(f, "Gamma %f\n", ((double)conf.mod.gamma) / 256.0);
   fprintf(f, "Brightness %f\n", ((double)conf.mod.brightness) / 256.0);
   fprintf(f, "Contrast %f\n", ((double)conf.mod.contrast) / 256.0);
   fprintf(f, "Red_Gamma %f\n", ((double)conf.rmod.gamma) / 256.0);
   fprintf(f, "Red_Brightness %f\n", ((double)conf.rmod.brightness) / 256.0);
   fprintf(f, "Red_Contrast %f\n", ((double)conf.rmod.contrast) / 256.0);
   fprintf(f, "Green_Gamma %f\n", ((double)conf.gmod.gamma) / 256.0);
   fprintf(f, "Green_Brightness %f\n", ((double)conf.gmod.brightness) / 256.0);
   fprintf(f, "Green_Contrast %f\n", ((double)conf.gmod.contrast) / 256.0);
   fprintf(f, "Blue_Gamma %f\n", ((double)conf.bmod.gamma) / 256.0);
   fprintf(f, "Blue_Brightness %f\n", ((double)conf.bmod.brightness) / 256.0);
   fprintf(f, "Blue_Contrast %f\n", ((double)conf.bmod.contrast) / 256.0);
   if (conf.ordered)
      fprintf(f, "Ordered_Dither on\n");
   else
      fprintf(f, "Ordered_Dither off\n");
   fclose(f);
   if (conf.palettefile)
      save_pal(conf.palettefile);
}

void
load_imrc()
{
   char               *h, s[2048];
   FILE               *f;

   h = getenv("HOME");
   g_snprintf(s, 2048, "%s/.imrc", h);
  setlocale(LC_ALL, "C");
  f = fopen(s, "r");
   if (!f)
     {
	h = gdk_imlib_get_sysconfig();
	f = fopen(h, "r");
	free(h);
     }
   read_imrc(f);
  setlocale(LC_ALL, "");
}

void
save_user(GtkWidget * widget, gpointer * data)
{
   char               *h, s[2048];
   FILE               *f;

   h = getenv("HOME");
   g_snprintf(s, 2048, "%s/.imrc", h);
   f = fopen(s, "w");
  setlocale(LC_ALL, "C");
  write_imrc(f);
  setlocale(LC_ALL, "");
}

void
save_sys(GtkWidget * widget, gpointer * data)
{
   char               *h;
   FILE               *f;

   h = gdk_imlib_get_sysconfig();
   f = fopen(h, "w");
   free(h);
  setlocale(LC_ALL, "C");
  write_imrc(f);
  setlocale(LC_ALL, "");
  
}

void
read_file(GtkWidget * widget, GtkWidget * w)
{
   char               *s;

   s = gtk_file_selection_get_filename(GTK_FILE_SELECTION(w));
   showhide(NULL, w);
   if (save)
     {
	save_pal(s);
	gtk_entry_set_text(GTK_ENTRY(pal_text), s);
     }
   else
    {
      load_pal(s);
      gtk_entry_set_text(GTK_ENTRY(pal_text), s);
      redraw_palette();
     }
}

void
mod_int(GtkWidget * widget, struct _vint *v)
{
  *(v->value) = (int)GTK_ADJUSTMENT(v->adj)->value;
  update_test();
}

void
reset_int(GtkWidget * widget, struct _vint *v)
{
   gtk_adjustment_set_value(GTK_ADJUSTMENT(v->adj), 256.0);
}

void
expose(GtkWidget * widget, gpointer * data)
{
   update_test();
}

void
add_test(GtkWidget * w)
{
   GtkWidget          *box, *area, *contain;

   make_test_image();

   box = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box, FALSE, FALSE, 0);
   gtk_widget_show(box);

   contain = gtk_viewport_new(NULL, NULL);
   gtk_widget_set_usize(contain, testimg_x + 4, testimg_y + 4);
   gtk_box_pack_start(GTK_BOX(box), contain, TRUE, FALSE, 0);
   gtk_widget_show(contain);

   area = gtk_drawing_area_new();
   gtk_drawing_area_size(GTK_DRAWING_AREA(area), testimg_x, testimg_y);
   gtk_widget_set_usize(area, testimg_x, testimg_y);
   gtk_container_add(GTK_CONTAINER(contain), area);
   gtk_widget_show(area);
   gtk_signal_connect(GTK_OBJECT(area), "expose_event", GTK_SIGNAL_FUNC(expose), NULL);
   test_area = area;
}

void
add_slider(GtkWidget * w, int *value, GtkWidget * ic)
{
   GtkObject          *adj;
   GtkWidget          *range, *button, *box;
   struct _vint       *v;

   box = gtk_hbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box, FALSE, FALSE, 0);
   gtk_widget_show(box);
   adj = gtk_adjustment_new((gfloat) * value, 0.0, 1024.0, 1.0, 8.0, 0.0);
   range = gtk_hscale_new(GTK_ADJUSTMENT(adj));
   gtk_widget_set_usize(range, 200, 12);
   gtk_range_set_update_policy(GTK_RANGE(range), GTK_UPDATE_CONTINUOUS);
   gtk_scale_set_draw_value(GTK_SCALE(range), FALSE);
   v = malloc(sizeof(struct _vint));

   v->adj = adj;
   v->range = range;
   v->value = value;
   gtk_signal_connect(GTK_OBJECT(adj), "value_changed", GTK_SIGNAL_FUNC(mod_int), v);
   gtk_box_pack_start(GTK_BOX(box), range, FALSE, FALSE, 0);
   gtk_widget_show(range);
   button = gtk_button_new();
   gtk_container_add(GTK_CONTAINER(button), ic);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(reset_int), v);
   gtk_widget_show(button);
   gtk_box_pack_start(GTK_BOX(box), button, TRUE, TRUE, 0);
}

void
make_icons()
{
   GdkImlibImage      *im;
   GdkImlibColor       icl;

   icl.r = 255;
   icl.g = 0;
   icl.b = 255;
   im = gdk_imlib_create_image_from_data(brightness_icon, NULL, 12, 12);
   gdk_imlib_set_image_shape(im, &icl);
   gdk_imlib_render(im, im->rgb_width, im->rgb_height);
   brightness_pmap = gdk_imlib_move_image(im);
   brightness_mask = gdk_imlib_move_mask(im);
   gdk_imlib_kill_image(im);
   im = gdk_imlib_create_image_from_data(contrast_icon, NULL, 12, 12);
   gdk_imlib_set_image_shape(im, &icl);
   gdk_imlib_render(im, im->rgb_width, im->rgb_height);
   contrast_pmap = gdk_imlib_move_image(im);
   contrast_mask = gdk_imlib_move_mask(im);
   gdk_imlib_kill_image(im);
   im = gdk_imlib_create_image_from_data(gamma_icon, NULL, 12, 12);
   gdk_imlib_set_image_shape(im, &icl);
   gdk_imlib_render(im, im->rgb_width, im->rgb_height);
   gamma_pmap = gdk_imlib_move_image(im);
   gamma_mask = gdk_imlib_move_mask(im);
   gdk_imlib_kill_image(im);
}

void
add_sliders(GtkWidget * w)
{
   GtkWidget          *frame, *box, *box0, *box1, *i1, *i2, *i3;

   make_icons();

   box0 = gtk_hbox_new(FALSE, 0);
   gtk_container_add(GTK_CONTAINER(w), box0);
   gtk_widget_show(box0);

   box1 = gtk_vbox_new(FALSE, 0);
   gtk_container_add(GTK_CONTAINER(box0), box1);
   gtk_widget_show(box1);

   i1 = gtk_pixmap_new(gamma_pmap, gamma_mask);
   gtk_widget_show(i1);
   i2 = gtk_pixmap_new(brightness_pmap, brightness_mask);
   gtk_widget_show(i2);
   i3 = gtk_pixmap_new(contrast_pmap, contrast_mask);
   gtk_widget_show(i3);

   frame = gtk_aspect_frame_new(_("Base Levels"), 0.5, 0.5, 0.0, TRUE);
   gtk_box_pack_start(GTK_BOX(box1), frame, FALSE, FALSE, 4);
   /* gtk_widget_show(frame); */
   box = gtk_vbox_new(TRUE, 0);
   gtk_container_add(GTK_CONTAINER(frame), box);
   gtk_widget_show(box);
   add_slider(box, &conf.mod.gamma, i1);
   add_slider(box, &conf.mod.brightness, i2);
   add_slider(box, &conf.mod.contrast, i3);

   i1 = gtk_pixmap_new(gamma_pmap, gamma_mask);
   gtk_widget_show(i1);
   i2 = gtk_pixmap_new(brightness_pmap, brightness_mask);
   gtk_widget_show(i2);
   i3 = gtk_pixmap_new(contrast_pmap, contrast_mask);
   gtk_widget_show(i3);
   frame = gtk_aspect_frame_new(_("Red Levels"), 0.5, 0.5, 0.0, TRUE);
   gtk_box_pack_start(GTK_BOX(box1), frame, FALSE, FALSE, 4);
   gtk_widget_show(frame);
   box = gtk_vbox_new(TRUE, 0);
   gtk_container_add(GTK_CONTAINER(frame), box);
   gtk_widget_show(box);
   add_slider(box, &conf.rmod.gamma, i1);
   add_slider(box, &conf.rmod.brightness, i2);
   add_slider(box, &conf.rmod.contrast, i3);

   i1 = gtk_pixmap_new(gamma_pmap, gamma_mask);
   gtk_widget_show(i1);
   i2 = gtk_pixmap_new(brightness_pmap, brightness_mask);
   gtk_widget_show(i2);
   i3 = gtk_pixmap_new(contrast_pmap, contrast_mask);
   gtk_widget_show(i3);
   frame = gtk_aspect_frame_new(_("Green Levels"), 0.5, 0.5, 0.0, TRUE);
   gtk_box_pack_start(GTK_BOX(box1), frame, FALSE, FALSE, 4);
   gtk_widget_show(frame);
   box = gtk_vbox_new(TRUE, 0);
   gtk_container_add(GTK_CONTAINER(frame), box);
   gtk_widget_show(box);
   add_slider(box, &conf.gmod.gamma, i1);
   add_slider(box, &conf.gmod.brightness, i2);
   add_slider(box, &conf.gmod.contrast, i3);

   i1 = gtk_pixmap_new(gamma_pmap, gamma_mask);
   gtk_widget_show(i1);
   i2 = gtk_pixmap_new(brightness_pmap, brightness_mask);
   gtk_widget_show(i2);
   i3 = gtk_pixmap_new(contrast_pmap, contrast_mask);
   gtk_widget_show(i3);
   frame = gtk_aspect_frame_new(_("Blue Levels"), 0.5, 0.5, 0.0, TRUE);
   gtk_box_pack_start(GTK_BOX(box1), frame, FALSE, FALSE, 4);
   gtk_widget_show(frame);
   box = gtk_vbox_new(TRUE, 0);
   gtk_container_add(GTK_CONTAINER(frame), box);
   gtk_widget_show(box);
   add_slider(box, &conf.bmod.gamma, i1);
   add_slider(box, &conf.bmod.brightness, i2);
   add_slider(box, &conf.bmod.contrast, i3);

   add_test(box0);
}

void
inbuilt_pal(GtkWidget *widget, char *p)
{
  char s[4096], *def;
  int i;
  
  def = gdk_imlib_get_sysconfig();
  for (i = strlen(def) - 1; ((i >= 0) && (def[i] != '/')); def[i--] = 0);
  g_snprintf(s, sizeof(s), "%s%s", def, p);
  free(def);
  load_pal(s);
  gtk_entry_set_text(GTK_ENTRY(pal_text), s);
  redraw_palette();
}

void
add_palsel(GtkWidget * w, char *title, char **value)
{
   GtkWidget          *button, *box, *box2, *box3, *text, *filesel, *colsel,
                      *pals, *contain;

   box = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box, TRUE, TRUE, 0);
   gtk_widget_show(box);

   box2 = gtk_hbox_new(TRUE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, TRUE, TRUE, 0);
   gtk_widget_show(box2);
  
   button = gtk_button_new_with_label(_("Small colour set"));
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(inbuilt_pal), "im_palette-tiny.pal");

   button = gtk_button_new_with_label(_("Medium colour set"));
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(inbuilt_pal), "im_palette-small.pal");

   button = gtk_button_new_with_label(_("Large colour set"));
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(inbuilt_pal), "im_palette.pal");
  
   box2 = gtk_hbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, TRUE, TRUE, 0);
   gtk_widget_show(box2);

   filesel = gtk_file_selection_new(_("Select Palette File"));
   gtk_signal_connect(GTK_OBJECT(filesel), "destroy",
		      GTK_SIGNAL_FUNC(showhide), filesel);
   gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(filesel)->cancel_button),
			     "clicked", GTK_SIGNAL_FUNC(showhide),
			     GTK_OBJECT(filesel));
   gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(filesel)->ok_button),
		      "clicked", GTK_SIGNAL_FUNC(read_file), filesel);

   button = gtk_button_new_with_label(_("Select"));
   gtk_box_pack_start(GTK_BOX(box2), button, FALSE, FALSE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(pal_load), filesel);

   text = gtk_entry_new_with_max_length(1024);
   gtk_box_pack_start(GTK_BOX(box2), text, TRUE, TRUE, 2);
   gtk_widget_show(text);
   if (*value)
      gtk_entry_set_text(GTK_ENTRY(text), *value);
   else
      gtk_entry_set_text(GTK_ENTRY(text), "");
   gtk_signal_connect(GTK_OBJECT(text), "changed", GTK_SIGNAL_FUNC(change_str), text);
   pal_text = text;

   button = gtk_button_new_with_label(_("Save"));
   gtk_box_pack_start(GTK_BOX(box2), button, FALSE, FALSE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(pal_save), filesel);

   box2 = gtk_hbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, TRUE, TRUE, 0);
   gtk_widget_show(box2);

   box = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box2), box, FALSE, FALSE, 0);
   gtk_widget_show(box);

   contain = gtk_viewport_new(NULL, NULL);
   gtk_widget_set_usize(contain, 132, 132);
   gtk_box_pack_start(GTK_BOX(box), contain, FALSE, FALSE, 0);
   gtk_widget_show(contain);

   pals = gtk_drawing_area_new();
   gtk_drawing_area_size(GTK_DRAWING_AREA(pals), 128, 128);
   gtk_widget_set_usize(pals, 128, 128);
   gtk_container_add(GTK_CONTAINER(contain), pals);
   gtk_widget_show(pals);
   color.pal_widget = pals;
   color.pal_im = make_pal_image();
   gtk_signal_connect(GTK_OBJECT(pals), "button_press_event", GTK_SIGNAL_FUNC(pal_press), NULL);
   gtk_widget_set_events(pals, GDK_BUTTON_PRESS_MASK);

   box3 = gtk_hbox_new(TRUE, 2);
   gtk_box_pack_start(GTK_BOX(box), box3, TRUE, TRUE, 2);
   gtk_widget_show(box3);

   button = gtk_button_new_with_label(_("Copy To"));
   gtk_box_pack_start(GTK_BOX(box3), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(col_copy), NULL);

   button = gtk_button_new_with_label(_("Spread To"));
   gtk_box_pack_start(GTK_BOX(box3), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(col_spread), NULL);

   box3 = gtk_hbox_new(TRUE, 2);
   gtk_box_pack_start(GTK_BOX(box), box3, TRUE, TRUE, 2);
   gtk_widget_show(box3);

   button = gtk_button_new_with_label(_("Set Last"));
   gtk_box_pack_start(GTK_BOX(box3), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(col_setlast), NULL);

   button = gtk_button_new_with_label(_("Delete"));
   gtk_box_pack_start(GTK_BOX(box3), button, TRUE, TRUE, 2);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(col_del), NULL);

   colsel = gtk_color_selection_new();
   gtk_box_pack_start(GTK_BOX(box2), colsel, FALSE, FALSE, 0);
   gtk_widget_show(colsel);
   gtk_signal_connect(GTK_OBJECT(colsel), "color_changed", GTK_SIGNAL_FUNC(change_col), colsel);
   color.col_widget = colsel;
}

void
add_note(char *p1, char *p2, char *p3, char *p4)
{
   GtkWidget          *label, *box, *box2;

   note = gtk_notebook_new();
   gtk_widget_show(note);

   box = gtk_vbox_new(FALSE, 0);
   box2 = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, FALSE, FALSE, 0);
   pg1 = box2;
   label = gtk_label_new(p1);
   gtk_widget_show(box);
   gtk_widget_show(box2);
   gtk_widget_show(label);
   gtk_notebook_append_page(GTK_NOTEBOOK(note), box, label);

   box = gtk_vbox_new(FALSE, 0);
   box2 = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, FALSE, FALSE, 0);
   pg2 = box2;
   label = gtk_label_new(p2);
   gtk_widget_show(box);
   gtk_widget_show(box2);
   gtk_widget_show(label);
   gtk_notebook_append_page(GTK_NOTEBOOK(note), box, label);

   box = gtk_vbox_new(FALSE, 0);
   box2 = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, TRUE, TRUE, 0);
   pg3 = box2;
   label = gtk_label_new(p3);
   gtk_widget_show(box);
   gtk_widget_show(box2);
   gtk_widget_show(label);
   gtk_notebook_append_page(GTK_NOTEBOOK(note), box, label);

   box = gtk_vbox_new(FALSE, 0);
   box2 = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, FALSE, FALSE, 0);
   pg4 = box2;
   label = gtk_label_new(p4);
   gtk_widget_show(box);
   gtk_widget_show(box2);
   gtk_widget_show(label);
   gtk_notebook_append_page(GTK_NOTEBOOK(note), box, label);

   gtk_box_pack_start(GTK_BOX(list), note, TRUE, TRUE, 0);
}

void
add_onoff(GtkWidget * w, char *title, char *value)
{
   GtkWidget          *check;

   check = gtk_check_button_new_with_label(title);
   gtk_box_pack_start(GTK_BOX(w), check, FALSE, FALSE, 2);
   gtk_widget_show(check);
   gtk_signal_connect(GTK_OBJECT(check), "clicked", GTK_SIGNAL_FUNC(toggle_onoff), value);
   gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(check), (gint) * value);
}

void
add_onoff_value(GtkWidget * w, char *title, char *value, int *setting,
		char *pre1str, int pre1,
		char *pre2str, int pre2,
		char *pre3str, int pre3,
		char *pre4str, int pre4,
		char *pre5str, int pre5,
		char *pre6str, int pre6)
{
   struct _val        *v;
   GtkWidget          *check, *button, *box, *box2, *text;
   char                s[256];

   box = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box, TRUE, TRUE, 0);
   gtk_widget_show(box);

   check = gtk_check_button_new_with_label(title);
   gtk_box_pack_start(GTK_BOX(box), check, FALSE, FALSE, 2);
   gtk_widget_show(check);
   gtk_signal_connect(GTK_OBJECT(check), "clicked", GTK_SIGNAL_FUNC(toggle_onoff), value);
   gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(check), (gint) * value);

   box2 = gtk_hbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(box), box2, TRUE, TRUE, 0);
   gtk_widget_show(box2);

   text = gtk_entry_new_with_max_length(11);
   gtk_box_pack_start(GTK_BOX(box2), text, FALSE, FALSE, 0);
   gtk_widget_show(text);
   v = malloc(sizeof(struct _val));

   v->value = pre1;
   v->setting = setting;
   v->text = text;
   g_snprintf(s, 256, "%i", *setting);
   gtk_entry_set_text(GTK_ENTRY(text), s);
   gtk_signal_connect(GTK_OBJECT(text), "changed", GTK_SIGNAL_FUNC(enter_val), v);

   button = gtk_button_new_with_label(pre1str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre1;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);

   button = gtk_button_new_with_label(pre2str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre2;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);

   button = gtk_button_new_with_label(pre3str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre3;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);

   button = gtk_button_new_with_label(pre4str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre4;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);

   button = gtk_button_new_with_label(pre5str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre5;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);

   button = gtk_button_new_with_label(pre6str);
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 0);
   gtk_widget_show(button);
   v = malloc(sizeof(struct _val));

   v->value = pre6;
   v->setting = setting;
   v->text = text;
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(change_val), v);
}

void
click_list(GtkWidget * widget, gint row)
{
   conf.visualid = vislist[row].id;
}

void
add_onoff_visual(GtkWidget * w, char *title)
{
   GtkWidget          *check, *box, *box2, *clist, *scrolled_win;
   char                s[256], *ss[3];
   const char         *titles[] =
   {
      "Visual Type",
      "Depth",
      "ID"
   };
   int                 i;

   box = gtk_vbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box, FALSE, FALSE, 0);
   gtk_widget_show(box);

   check = gtk_check_button_new_with_label(title);
   gtk_box_pack_start(GTK_BOX(box), check, FALSE, FALSE, 2);
   gtk_widget_show(check);
   gtk_signal_connect(GTK_OBJECT(check), "clicked", GTK_SIGNAL_FUNC(toggle_onoff), &conf.forcevisualid);
   gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(check), conf.forcevisualid);

   box2 = gtk_hbox_new(FALSE, 0);
   gtk_box_pack_start(GTK_BOX(w), box2, TRUE, TRUE, 0);
   gtk_widget_show(box2);

   scrolled_win = gtk_scrolled_window_new (NULL, NULL);
   clist = gtk_clist_new_with_titles(3, (char **)titles);
   gtk_box_pack_start(GTK_BOX(w), scrolled_win, TRUE, TRUE, 0);
   gtk_container_add (GTK_CONTAINER (scrolled_win), clist);
   gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win), GTK_POLICY_ALWAYS, GTK_POLICY_AUTOMATIC);
   gtk_signal_connect(GTK_OBJECT(clist), "select_row", GTK_SIGNAL_FUNC(click_list), NULL);
   gtk_clist_freeze(GTK_CLIST(clist));
   for (i = 0; i < vis_num; i++)
     {
	ss[0] = strdup(vislist[i].name);
	g_snprintf(s, 256, "%i\n", vislist[i].depth);
	ss[1] = strdup(s);
	g_snprintf(s, 256, "0x%x\n", vislist[i].id);
	ss[2] = strdup(s);
	gtk_clist_append(GTK_CLIST(clist), ss);
	free(ss[0]);
	free(ss[1]);
	free(ss[2]);
     }
   gtk_clist_thaw(GTK_CLIST(clist));
   gtk_widget_show(clist);
}

void
add_loadsave()
{
   GtkWidget          *button, *box2;

   box2 = gtk_hbox_new(TRUE, 0);
   gtk_box_pack_start(GTK_BOX(list), box2, FALSE, FALSE, 8);
   gtk_widget_show(box2);

   button = gtk_button_new_with_label(_("Save User Config"));
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 32);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(save_user), NULL);

  if (getuid() == 0)
    {
      button = gtk_button_new_with_label(_("Save System Config"));
      gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 32);
      gtk_widget_show(button);
      gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(save_sys), NULL);
    }

   button = gtk_button_new_with_label(_("Close"));
   gtk_box_pack_start(GTK_BOX(box2), button, TRUE, TRUE, 32);
   gtk_widget_show(button);
   gtk_signal_connect(GTK_OBJECT(button), "clicked", GTK_SIGNAL_FUNC(kill_win), NULL);
}

void
ready()
{
   gdouble             col[4];
   unsigned char      *ptr;
   int                 i, j;

   gtk_widget_show(win);
   col[0] = ((gdouble) color.color[color.cur_color][0]) / 255;
   col[1] = ((gdouble) color.color[color.cur_color][1]) / 255;
   col[2] = ((gdouble) color.color[color.cur_color][2]) / 255;
   col[3] = 0;
   if (color.pmap)
      gdk_imlib_free_pixmap(color.pmap);
   ptr = color.pal_im->rgb_data;
   for (i = 0; i < 256; i++)
     {
	for (j = 0; j < 3; j++)
	  {
	     *ptr++ = color.color[i][j];
	  }
     }
   gdk_imlib_render(color.pal_im, 128, 128);
   color.pmap = gdk_imlib_move_image(color.pal_im);
   gdk_window_set_back_pixmap(color.pal_widget->window, color.pmap, FALSE);
   gdk_imlib_changed_image(color.pal_im);
   gdk_window_clear(color.pal_widget->window);
/*   update_test(); */
}

void
init_conf()
{
   conf.palettefile = NULL;
   conf.paletteoverride = 0;
   conf.dither = 1;
   conf.remap = 1;
   conf.highquality = 0;
#ifdef HAVE_SHM
   conf.mitshm = 1;
#else
   conf.mitshm = 0;
#endif
   conf.shmmax = 0;
   conf.shmmaxsize = 1000000;
   conf.sharedpixmap = 0;
   conf.fastrender = 1;
   conf.imagecache = 1;
   conf.imagecachesize = 4000000;
   conf.pixmapcache = 1;
   conf.pixmapcachesize = 40000000;
   conf.forcevisualid = 0;
   conf.forcevisualid = 0;
   conf.visualid = -1;
   conf.fallback = 1;
   conf.mod.gamma = 256;
   conf.mod.brightness = 256;
   conf.mod.contrast = 256;
   conf.rmod.gamma = 256;
   conf.rmod.brightness = 256;
   conf.rmod.contrast = 256;
   conf.gmod.gamma = 256;
   conf.gmod.brightness = 256;
   conf.gmod.contrast = 256;
   conf.bmod.gamma = 256;
   conf.bmod.brightness = 256;
   conf.bmod.contrast = 256;
   conf.ordered = 1;
}

void
list_vis()
{
   int                 num, i;
   XVisualInfo         xvi, *xvir;

   xvir = XGetVisualInfo(gdk_display, 0, &xvi, &num);
   vislist = malloc(num * sizeof(struct _vis));

   for (i = 0; i < num; i++)
     {
	vislist[i].id = xvir[i].visualid;
	vislist[i].depth = xvir[i].depth;
	switch (xvir[i].class)
	  {
	  case StaticGray:
	     vislist[i].name = strdup("StaticGray");
	     break;
	  case GrayScale:
	     vislist[i].name = strdup("GrayScale");
	     break;
	  case StaticColor:
	     vislist[i].name = strdup("StaticColor");
	     break;
	  case PseudoColor:
	     vislist[i].name = strdup("PseudoColor");
	     break;
	  case TrueColor:
	     vislist[i].name = strdup("TrueColor");
	     break;
	  case DirectColor:
	     vislist[i].name = strdup("DirectColor");
	     break;
	  default:
	     break;
	  }
     }
   vis_num = num;
   XFree(xvir);
}

int
main(int argc, char **argv)
{
   gtk_init(&argc, &argv);
   gdk_imlib_init();
   list_vis();

   gtk_widget_push_visual(gdk_imlib_get_visual());
   gtk_widget_push_colormap(gdk_imlib_get_colormap());
   color.pmap = NULL;
   color.cur_color = 0;
   color.copy = 0;
   color.spread = 0;
   color.setlast = 0;
   color.del = 0;
   up = 0;

   init_conf();
   load_imrc();
   make_win();
   add_note(_("Colors"), _("Rendering"), _("Display"), _("Color Correction"));
   add_palsel(pg1, _("Select Palette"), &conf.palettefile);
   add_onoff(pg1, _("Force Palette"), &conf.paletteoverride);
   add_onoff(pg1, _("Dither in Palette Mode"), &conf.dither);
   add_onoff(pg1, _("Fast Remapping in Palette Mode"), &conf.remap);
   add_onoff(pg2, _("High Quality in 15/16bpp"), &conf.highquality);
   add_onoff_value(pg2, _("Limit Maximum Shared memory Size (bytes)"),
		   &conf.shmmax, &conf.shmmaxsize,
		   _("256 Kb"), 256 * 1024,
		   _("512 Kb"), 512 * 1024,
		   _("1   Mb"), 1 * 1024 * 1024,
		   _("2   Mb"), 2 * 1024 * 1024,
		   _("3   Mb"), 3 * 1024 * 1024,
		   _("4   Mb"), 4 * 1024 * 1024);
   add_onoff(pg2, _("Use Ordered Dithering (Faster)"), &conf.ordered);
   add_onoff(pg2, _("MIT-SHM Shared Memory Enabled"), &conf.mitshm);
   add_onoff(pg2, _("Shared Pixmaps"), &conf.sharedpixmap);
   add_onoff(pg2, _("Fast Render"), &conf.fastrender);
   add_onoff(pg2, _("Imagemagick & NETPBM Fallback"), &conf.fallback);
   add_onoff_value(pg2, _("Image Cache (bytes)"),
		   &conf.imagecache, &conf.imagecachesize,
		   _("512 Kb"), 512 * 1024,
		   _("1 Mb"), 1024 * 1024,
		   _("2 Mb"), 2 * 1024 * 1024,
		   _("4 Mb"), 4 * 1024 * 1024,
		   _("6 Mb"), 6 * 1024 * 1024,
		   _("10 Mb"), 10 * 1024 * 1024);
   add_onoff_value(pg2, _("Pixmap Cache (bits)"),
		   &conf.pixmapcache, &conf.pixmapcachesize,
		   _("5120 Kbits"), 5120 * 1024,
		   _("10 Mbits"), 10240 * 1024,
		   _("20 Mbits"), 20 * 1024 * 1024,
		   _("40 Mbits"), 40 * 1024 * 1024,
		   _("60 Mbits"), 60 * 1024 * 1024,
		   _("100 Mbits"), 100 * 1024 * 1024);
   add_onoff_visual(pg3, _("Force Visual ID"));
   add_sliders(pg4);
   add_loadsave();
   ready();
   up = 1;
   gtk_main();
   return 0;
}
