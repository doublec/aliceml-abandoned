#define _GNU_SOURCE
#include <config.h>
#include "Imlib.h"
#include "Imlib_private.h"

/* uncomment this to compile imlib's cahce with pixmap accounting output */
/*#define PIXMAP_ACCOUNTING*/
/* uncomment this to compile imlib's cahce with image accounting output */
/*#define IMAGE_ACCOUNTING*/

void
dirty_pixmaps(ImlibData * id, ImlibImage * im)
{
  struct pixmap_cache *ptr;

  ptr = id->cache.pixmap;
  while (ptr)
    {
      if ((ptr->im == im) && ((!ptr->file) || (!strcmp(im->filename, ptr->file))))
	ptr->dirty = 1;
      ptr = ptr->next;
    }
}

void
dirty_images(ImlibData * id, ImlibImage * im)
{
  struct image_cache *ptr;

  ptr = id->cache.image;
  while (ptr)
    {
      if ((!strcmp(im->filename, ptr->file)) && (im == ptr->im))
	{
	  ptr->dirty = 1;
	  return;
	}
      ptr = ptr->next;
    }
}

void
find_pixmap(ImlibData * id, ImlibImage * im, int width, int height, Pixmap * pmap, Pixmap * mask)
{
  struct pixmap_cache *ptr;

  ptr = id->cache.pixmap;
  while (ptr)
    {
      if ((ptr->im == im) && (ptr->width == width) && (ptr->height == height) &&
	  ((!ptr->file) || (!strcmp(im->filename, ptr->file))) &&
	  (!ptr->dirty))
	{
	  if (ptr->refnum > 0)
	    ptr->refnum++;
	  else
	    {
	      ptr->refnum++;
	      id->cache.num_pixmap++;
	      if (ptr->pmap)
		id->cache.used_pixmap -= width * height * id->x.depth;
	      if (ptr->shape_mask)
		id->cache.used_pixmap -= width * height;
	      if (id->cache.used_pixmap < 0)
		{
		  id->cache.used_pixmap = 0;
		  fprintf(stderr, "IMLIB: uhoh.. caching problems.... meep meep\n");
		}
	    }
	  if (ptr->prev)
	    {
	      ptr->prev->next = ptr->next;
	      if (ptr->next)
		ptr->next->prev = ptr->prev;
	      ptr->next = id->cache.pixmap;
	      ptr->next->prev = ptr;
	      id->cache.pixmap = ptr;
	      ptr->prev = NULL;
	    }
	  *pmap = ptr->pmap;
	  *mask = ptr->shape_mask;
	  return;
	}
      ptr = ptr->next;
    }
  *pmap = 0;
  *mask = 0;
}

ImlibImage         *
find_image(ImlibData * id, char *file)
{
  struct image_cache *ptr;

  ptr = id->cache.image;
  while (ptr)
    {
      if ((!strcmp(file, ptr->file)) && (!ptr->dirty))
	{
	  if (ptr->refnum)
	    ptr->refnum++;
	  else
	    {
	      ptr->refnum++;
	      id->cache.num_image++;
	      id->cache.used_image -= ptr->im->rgb_width * ptr->im->rgb_height * 3;
	      if (id->cache.used_image < 0)
		{
		  id->cache.used_image = 0;
		  fprintf(stderr, "IMLIB: uhoh.. caching problems.... meep meep\n");
		}
	    }
	  if (ptr->prev)
	    {
	      ptr->prev->next = ptr->next;
	      if (ptr->next)
		ptr->next->prev = ptr->prev;
	      ptr->next = id->cache.image;
	      ptr->next->prev = ptr;
	      id->cache.image = ptr;
	      ptr->prev = NULL;
	    }
	  return ptr->im;
	}
      ptr = ptr->next;
    }
  return NULL;
}

void
free_pixmappmap(ImlibData * id, Pixmap pmap)
{
  struct pixmap_cache *ptr;

  ptr = id->cache.pixmap;
  while (ptr)
    {
      if ((ptr->pmap == pmap) || (ptr->shape_mask == pmap))
	{
	  if (ptr->shape_mask == pmap)
	    return;
	  if (ptr->refnum > 0)
	    {
	      ptr->refnum--;
	      if (ptr->refnum == 0)
		{
		  id->cache.num_pixmap--;
		  if (ptr->pmap)
		    id->cache.used_pixmap += ptr->width * ptr->height * id->x.depth;
		  if (ptr->shape_mask)
		    id->cache.used_pixmap += ptr->width * ptr->height;
		}
	    }
	  return;
	}
      ptr = ptr->next;
    }
  XFreePixmap(id->x.disp, pmap);
}

void
free_image(ImlibData * id, ImlibImage * im)
{
  struct image_cache *ptr;

  ptr = id->cache.image;
  while (ptr)
    {
      if (im == ptr->im)
	{
	  if (ptr->refnum)
	    {
	      ptr->refnum--;
	      if (ptr->refnum == 0)
		{
		  id->cache.num_image--;
		  id->cache.used_image += ptr->im->rgb_width * ptr->im->rgb_height * 3;
		  if (im->pixmap)
		    {
		      free_pixmappmap(id, im->pixmap);
		      im->pixmap = 0;
		      im->shape_mask = 0;
		    }
		}
	    }
	  return;
	}
      ptr = ptr->next;
    }
  nullify_image(id, im);
}

void
flush_image(ImlibData * id, ImlibImage * im)
{
  if (im)
    im->cache = 0;
}

void
add_image(ImlibData * id, ImlibImage * im, char *file)
{
  struct image_cache *ptr;
  struct image_cache *n;

  if ((!im) || (!file))
    return;
  ptr = id->cache.image;
  n = malloc(sizeof(struct image_cache));

  if (!n)
    return;
  n->prev = NULL;
  n->next = ptr;
  n->file = malloc(strlen(file) + 1);
  if (!n->file)
    {
      free(n);
      return;
    }
  strcpy(n->file, file);
  n->im = im;
  n->refnum = 1;
  n->dirty = 0;
  if (n->next)
    n->next->prev = n;
  id->cache.image = n;
  id->cache.num_image++;
}

void
add_pixmap(ImlibData * id, ImlibImage * im, int width, int height, XImage * xim, XImage * sxim)
{
  struct pixmap_cache *ptr;
  struct pixmap_cache *n;

  if (!im)
    return;
  ptr = id->cache.pixmap;
  n = malloc(sizeof(struct pixmap_cache));

  if (!n)
    return;
  n->prev = NULL;
  n->next = ptr;
  n->im = im;
  if (im->filename)
    {
      n->file = malloc(strlen(im->filename) + 1);
      if (n->file)
	strcpy(n->file, im->filename);
    }
  else
    n->file = NULL;
  n->refnum = 1;
  n->dirty = 0;
  n->width = width;
  n->height = height;
  n->pmap = im->pixmap;
  n->shape_mask = im->shape_mask;
  n->xim = xim;
  n->sxim = sxim;
  if (n->next)
    n->next->prev = n;
  id->cache.pixmap = n;
  id->cache.num_pixmap++;
}

void
clean_caches(ImlibData * id)
{
  {
    struct image_cache *ptr = NULL;
    struct image_cache *last = NULL;
    int                 newlast;

#ifdef IMAGE_ACCOUNTING
    int                 total, total2, num, num2;

    printf("--------- Image cashe zise %i / %i with %i images referenced\n",
	   id->cache.used_image, id->cache.size_image,
	   id->cache.num_image);
    ptr = id->cache.image;
    total = 0;
    total2 = 0;
    num = 0;
    num2 = 0;
    while (ptr)
      {
	printf("Image for file %80s REFNUM %3i SIZE %4ix%4i\n",
	       ptr->file, ptr->refnum, ptr->im->rgb_width,
	       ptr->im->rgb_height);
	if (ptr->refnum > 0)
	  {
	    total += (ptr->im->rgb_width * ptr->im->rgb_height * 3);
	    num++;
	  }
	else
	  {
	    total2 += (ptr->im->rgb_width * ptr->im->height * 3);
	    num2++;
	  }
	ptr = ptr->next;
      }
    printf("Accounting Data:\n");
    printf("*** total image's in cache %i with %i images\n",
	   total, num);
    printf("*** total unreffed images's in cache %i with %i images\n\n",
	   total2, num2);
#endif
    /* find the back of the list */
    ptr = id->cache.image;
    while (ptr)
      {
	last = ptr;
	ptr = ptr->next;
      }
    newlast = 0;
    ptr = last;
    /* remove all images that are tagged non-cachable, and have 0 */
    /* references , even if the cache has spare room. */
    while (ptr)
      {
	if (ptr->refnum <= 0)
	  {
	    if (!ptr->im->cache)
	      {
		id->cache.used_image -= ptr->im->rgb_width * ptr->im->rgb_height * 3;
		nullify_image(id, ptr->im);
		if (ptr->prev)
		  ptr->prev->next = ptr->next;
		else
		  id->cache.image = ptr->next;
		if (ptr->next)
		  ptr->next->prev = ptr->prev;
		if (ptr->file)
		  free(ptr->file);
		last = ptr;
		ptr = ptr->prev;
		free(last);
	      }
	    else
	      ptr = ptr->prev;
	  }
	else
	  ptr = ptr->prev;
      }
    /* find the back of the list */
    ptr = id->cache.image;
    last = NULL;
    while (ptr)
      {
	last = ptr;
	ptr = ptr->next;
      }
    ptr = last;
    newlast = 0;
    /* while the amount of data in the cache is greater than the set */
    /* amount, delete the last entry (last used) from the unreferenced */
    /* cached 24-bit images */
    while (id->cache.used_image > id->cache.size_image)
      {
	if (newlast)
	  {
	    ptr = id->cache.image;
	    last = NULL;
	    while (ptr)
	      {
		last = ptr;
		ptr = ptr->next;
	      }
	    ptr = last;
	    newlast = 0;
	  }
	while (ptr)
	  {
	    if (ptr->refnum <= 0)
	      {
		id->cache.used_image -= ptr->im->rgb_width * ptr->im->rgb_height * 3;
		nullify_image(id, ptr->im);
		if (ptr->prev)
		  ptr->prev->next = ptr->next;
		else
		  id->cache.image = ptr->next;
		if (ptr->next)
		  ptr->next->prev = ptr->prev;
		if (ptr->file)
		  free(ptr->file);
		last = ptr;
		ptr = ptr->prev;
		free(last);
		newlast = 1;
	      }
	    else
	      ptr = ptr->prev;
	    if (id->cache.used_image <= id->cache.size_image)
	      ptr = NULL;
	  }
      }
  }
  {
    struct pixmap_cache *ptr;
    struct pixmap_cache *last;
    int                 newlast;

#ifdef PIXMAP_ACCOUNTING
    int                 total, total2, num, num2;

    printf("--------- Pixmap cashe zise %i / %i with %i pixmaps referenced\n",
	   id->cache.used_pixmap, id->cache.size_pixmap,
	   id->cache.num_pixmap);
    ptr = id->cache.pixmap;
    total = 0;
    total2 = 0;
    num = 0;
    num2 = 0;
    while (ptr)
      {
	printf("Pmap for file %80s REFNUM %3i SIZE %4ix%4i PMAP %8x MASK %8x\n",
	       ptr->file, ptr->refnum, ptr->width, ptr->height, ptr->pmap,
	       ptr->shape_mask);
	if (ptr->refnum > 0)
	  {
	    total += (ptr->width * ptr->height * id->x.depth);
	    if (ptr->shape_mask)
	      total += (ptr->width * ptr->height);
	    num++;
	  }
	else
	  {
	    total2 += (ptr->width * ptr->height * id->x.depth);
	    if (ptr->shape_mask)
	      total2 += (ptr->width * ptr->height);
	    num2++;
	  }
	ptr = ptr->next;
      }
    printf("Accounting Data:\n");
    printf("*** total pixmap's in cache %i with %i pixmaps\n",
	   total, num);
    printf("*** total unreffed pixmap's in cache %i with %i pixmaps\n\n",
	   total2, num2);
#endif
    /* find the back of the list */
    ptr = id->cache.pixmap;
    last = NULL;
    while (ptr)
      {
	last = ptr;
	ptr = ptr->next;
      }
    ptr = last;
    newlast = 0;
    /* while the amount of data in the cache is greater than the set */
    /* amount, delete the last entry (last used) from the unreferenced */
    /* cached pixmaps */
    while (id->cache.used_pixmap > id->cache.size_pixmap)
      {
	if (newlast)
	  {
	    ptr = id->cache.pixmap;
	    last = NULL;
	    while (ptr)
	      {
		last = ptr;
		ptr = ptr->next;
	      }
	    ptr = last;
	    newlast = 0;
	  }
	while (ptr)
	  {
	    if (ptr->refnum <= 0)
	      {
		if (ptr->pmap)
		  id->cache.used_pixmap -= ptr->width * ptr->height * id->x.depth;
		if (ptr->shape_mask)
		  id->cache.used_pixmap -= ptr->width * ptr->height;
		if (ptr->pmap)
		  XFreePixmap(id->x.disp, ptr->pmap);
		if (ptr->shape_mask)
		  XFreePixmap(id->x.disp, ptr->shape_mask);
		if (ptr->xim)
		  XDestroyImage(ptr->xim);
		if (ptr->sxim)
		  XDestroyImage(ptr->sxim);
		if (ptr->prev)
		  ptr->prev->next = ptr->next;
		else
		  id->cache.pixmap = ptr->next;
		if (ptr->next)
		  ptr->next->prev = ptr->prev;
		if (ptr->file)
		  free(ptr->file);
		last = ptr;
		ptr = ptr->prev;
		free(last);
		newlast = 1;
	      }
	    else
	      ptr = ptr->prev;
	    if (id->cache.used_pixmap <= id->cache.size_pixmap)
	      ptr = NULL;
	  }
      }
  }
}

void
nullify_image(ImlibData * id, ImlibImage * im)
{
  if (!im)
    return;
  if (im->rgb_data)
    free(im->rgb_data);
  if (im->alpha_data)
    free(im->alpha_data);
  if (im->pixmap)
    free_pixmappmap(id, im->pixmap);
  if (im->filename)
    free(im->filename);
  free(im);
}
