#ifndef CONVERT_PATH
#define CONVERT_PATH "/usr/local/bin"
#endif

#ifndef NETPBM_PATH
#define NETPBM_PATH  "/usr/local/bin"
#endif

#ifndef CJPEG_PROG
#define CJPEG_PROG "/usr/local/bin/cjpeg"
#endif

#ifndef DJPEG_PROG
#define DJPEG_PROG "/usr/local/bin/djpeg"
#endif

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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#ifdef HAVE_XSHM_H
#include <X11/extensions/XShm.h>
#endif
#include <X11/extensions/shape.h>
#include <X11/cursorfont.h>

#ifdef HAVE_LIBJPEG
#include <jpeglib.h>
#endif
#ifdef HAVE_LIBPNG
#include <png.h>
#endif
#ifdef HAVE_LIBTIFF
#include <tiffio.h>
#endif
#ifdef HAVE_LIBGIF
#include <gif_lib.h>
#endif

#define BYTE_ORD_24_RGB 0
#define BYTE_ORD_24_RBG 1
#define BYTE_ORD_24_BRG 2
#define BYTE_ORD_24_BGR 3
#define BYTE_ORD_24_GRB 4
#define BYTE_ORD_24_GBR 5

struct image_cache
  {
    char               *file;
    ImlibImage         *im;
    int                 refnum;
    char                dirty;
    struct image_cache *prev;
    struct image_cache *next;
  };

struct pixmap_cache
  {
    ImlibImage         *im;
    char               *file;
    char                dirty;
    int                 width, height;
    Pixmap              pmap;
    Pixmap              shape_mask;
    XImage             *xim;
    XImage             *sxim;
    int                 refnum;
    struct pixmap_cache *prev;
    struct pixmap_cache *next;
  };


int                 index_best_color_match(ImlibData * id, int *r, int *g, int *b);
void                dirty_pixmaps(ImlibData * id, ImlibImage * im);
void                dirty_images(ImlibData * id, ImlibImage * im);
void                find_pixmap(ImlibData * id, ImlibImage * im, int width, int height, Pixmap * pmap, Pixmap * mask);
ImlibImage         *find_image(ImlibData * id, char *file);
void                free_pixmappmap(ImlibData * id, Pixmap pmap);
void                free_image(ImlibData * id, ImlibImage * im);
void                flush_image(ImlibData * id, ImlibImage * im);
void                add_image(ImlibData * id, ImlibImage * im, char *file);
void                add_pixmap(ImlibData * id, ImlibImage * im, int width, int height, XImage * xim, XImage * sxim);
void                clean_caches(ImlibData * id);
void                nullify_image(ImlibData * id, ImlibImage * im);

char               *_GetExtension(char *file);

#ifdef HAVE_LIBJPEG
unsigned char      *_LoadJPEG(ImlibData * id, FILE * f, int *w, int *h);

#endif /* HAVE_LIBJPEG */
#ifdef HAVE_LIBPNG
unsigned char      *_LoadPNG(ImlibData * id, FILE * f, int *w, int *h, int *t);

#endif /* HAVE_LIBPNG */
#ifdef HAVE_LIBTIFF
unsigned char      *_LoadTIFF(ImlibData * id, FILE *f, char *file, int *w, int *h, int *t);

#endif /* HAVE_LIBTIFF */
#ifdef HAVE_LIBGIF
unsigned char      *_LoadGIF(ImlibData * id, FILE *f, int *w, int *h, int *t);

#endif /* HAVE_LIBGIF */
unsigned char      *_LoadBMP(ImlibData * id, FILE *f, int *w, int *h, int *t);
unsigned char      *_LoadXPM(ImlibData * id, FILE *f, int *w, int *h, int *t);
unsigned char      *_LoadPPM(ImlibData * id, FILE * f, int *w, int *h);
int                 ispnm(FILE *f);
int                 isjpeg(FILE *f);
int                 ispng(FILE *f);
int                 istiff(FILE *f);
int                 iseim(FILE *f);
int                 isgif(FILE *f);
int                 isxpm(FILE *f);
int                 isbmp(FILE *f);

void                calc_map_tables(ImlibData * id, ImlibImage * im);

void                _PaletteAlloc(ImlibData * id, int num, int *cols);

FILE               *open_helper(const char *, const char *, const char *);
int                 close_helper(FILE *);

#define INDEX_RGB(r,g,b)  id->fast_rgb[(r<<10)|(g<<5)|(b)]
#define COLOR_INDEX(i)    id->palette[i].pixel
#define COLOR_RGB(r,g,b)  id->palette[INDEX_RGB(r,g,b)].pixel
#define ERROR_RED(rr,i)   rr-id->palette[i].r;
#define ERROR_GRN(gg,i)   gg-id->palette[i].g;
#define ERROR_BLU(bb,i)   bb-id->palette[i].b;

#define DITHER_ERROR(Der1,Der2,Dex,Der,Deg,Deb) \
ter=&(Der1[Dex]);\
(*ter)+=(Der*7)>>4;ter++;\
(*ter)+=(Deg*7)>>4;ter++;\
(*ter)+=(Deb*7)>>4;\
ter=&(Der2[Dex-6]);\
(*ter)+=(Der*3)>>4;ter++;\
(*ter)+=(Deg*3)>>4;ter++;\
(*ter)+=(Deb*3)>>4;ter++;\
(*ter)+=(Der*5)>>4;ter++;\
(*ter)+=(Deg*5)>>4;ter++;\
(*ter)+=(Deb*5)>>4;ter++;\
(*ter)+=Der>>4;ter++;\
(*ter)+=Deg>>4;ter++;\
(*ter)+=Deb>>4;
