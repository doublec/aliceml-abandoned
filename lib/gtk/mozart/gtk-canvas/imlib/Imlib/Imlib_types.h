#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#ifdef HAVE_XSHM_H
#include <X11/extensions/XShm.h>
#endif
#include <X11/extensions/shape.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#include <netinet/in.h>
#ifdef HAVE_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SHM_H
#include <sys/shm.h>
#endif
#include <sys/time.h>
#include <sys/types.h>

#ifdef _HAVE_STRING_H
#include <string.h>
#elif _HAVE_STRINGS_H
#include <strings.h>
#endif

#ifndef SYSTEM_IMRC
#define SYSTEM_IMRC "/etc/imrc"
#endif /* endef SYSTEM_IMRC */

typedef struct _ImlibBorder ImlibBorder;
typedef struct _ImlibColor ImlibColor;
typedef struct _ImlibColorModifier ImlibColorModifier;
typedef struct _ImlibImage ImlibImage;
typedef struct _xdata Xdata;
typedef struct _ImlibData ImlibData;
typedef struct _ImlibSaveInfo ImlibSaveInfo;
typedef struct _ImlibInitParams ImlibInitParams;

struct _ImlibBorder
  {
    int                 left, right;
    int                 top, bottom;
  };

struct _ImlibColor
  {
    int                 r, g, b;
    int                 pixel;
  };

struct _ImlibColorModifier
  {
    int                 gamma;
    int                 brightness;
    int                 contrast;
  };

struct _ImlibImage
  {
    int                 rgb_width, rgb_height;
    unsigned char      *rgb_data;
    unsigned char      *alpha_data;
    char               *filename;
/* the below information is private */
    int                 width, height;
    ImlibColor          shape_color;
    ImlibBorder         border;
    Pixmap              pixmap;
    Pixmap              shape_mask;
    char                cache;
    ImlibColorModifier  mod, rmod, gmod, bmod;
    unsigned char       rmap[256], gmap[256], bmap[256];
  };

struct _xdata
  {
    Display            *disp;
    int                 screen;
    Window              root;
    Visual             *visual;
    int                 depth;
    int                 render_depth;
    Colormap            root_cmap;
    char                shm;
    char                shmp;
    int                 shm_event;
    XImage             *last_xim;
    XImage             *last_sxim;
#ifdef HAVE_XSHM_H
    XShmSegmentInfo     last_shminfo;
    XShmSegmentInfo     last_sshminfo;
#endif
    Window              base_window;
    int                 byte_order, bit_order;
  };

struct _ImlibData
  {
    int                 num_colors;
    ImlibColor         *palette;
    ImlibColor         *palette_orig;
    unsigned char      *fast_rgb;
    int                *fast_err;
    int                *fast_erg;
    int                *fast_erb;
    int                 render_type;
    int                 max_shm;
    Xdata               x;
    int                 byte_order;
    struct _cache
      {
	char                on_image;
	int                 size_image;
	int                 num_image;
	int                 used_image;
	struct image_cache *image;
	char                on_pixmap;
	int                 size_pixmap;
	int                 num_pixmap;
	int                 used_pixmap;
	struct pixmap_cache *pixmap;
      }
    cache;
    char                fastrend;
    char                hiq;
    ImlibColorModifier  mod, rmod, gmod, bmod;
    unsigned char       rmap[256], gmap[256], bmap[256];
    char                fallback;
    char                ordered_dither;
  };

struct _ImlibSaveInfo
  {
    int                 quality;
    int                 scaling;
    int                 xjustification;
    int                 yjustification;
    int                 page_size;
    char                color;
  };

struct _ImlibInitParams
  {
    int                 flags;
    int                 visualid;
    char               *palettefile;
    char                sharedmem;
    char                sharedpixmaps;
    char                paletteoverride;
    char                remap;
    char                fastrender;
    char                hiquality;
    char                dither;
    int                 imagecachesize;
    int                 pixmapcachesize;
    Colormap            cmap;
  };

#define PARAMS_VISUALID        1<<0
#define PARAMS_PALETTEFILE     1<<1
#define PARAMS_SHAREDMEM       1<<2
#define PARAMS_SHAREDPIXMAPS   1<<3
#define PARAMS_PALETTEOVERRIDE 1<<4
#define PARAMS_REMAP           1<<5
#define PARAMS_FASTRENDER      1<<6
#define PARAMS_HIQUALITY       1<<7
#define PARAMS_DITHER          1<<8
#define PARAMS_IMAGECACHESIZE  1<<9
#define PARAMS_PIXMAPCACHESIZE 1<<10
#define PARAMS_COLORMAP        1<<11

#define PAGE_SIZE_EXECUTIVE    0
#define PAGE_SIZE_LETTER       1
#define PAGE_SIZE_LEGAL        2
#define PAGE_SIZE_A4           3
#define PAGE_SIZE_A3           4
#define PAGE_SIZE_A5           5
#define PAGE_SIZE_FOLIO        6

#define RT_PLAIN_PALETTE       0
#define RT_PLAIN_PALETTE_FAST  1
#define RT_DITHER_PALETTE      2
#define RT_DITHER_PALETTE_FAST 3
#define RT_PLAIN_TRUECOL       4
/* a special high-quality renderer for people with 15 and 16bpp that dithers */
#define RT_DITHER_TRUECOL      5
