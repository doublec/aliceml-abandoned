#ifndef __IMLIB_H__
#define __IMLIB_H__

#include <Imlib_types.h>

#ifdef __cplusplus
extern              "C"
{
#endif				/* __cplusplus */

  ImlibData          *Imlib_init(Display * disp);
  ImlibData          *Imlib_init_with_params(Display * disp, ImlibInitParams * p);
  int                 Imlib_get_render_type(ImlibData * id);
  void                Imlib_set_render_type(ImlibData * id, int rend_type);
  int                 Imlib_load_colors(ImlibData * id, char *file);
  ImlibImage         *Imlib_load_image(ImlibData * id, char *file);
  int                 Imlib_best_color_match(ImlibData * id, int *r, int *g, int *b);
  int                 Imlib_render(ImlibData * id, ImlibImage * image, int width, int height);
  Pixmap              Imlib_copy_image(ImlibData * id, ImlibImage * image);
  Pixmap              Imlib_copy_mask(ImlibData * id, ImlibImage * image);
  Pixmap              Imlib_move_image(ImlibData * id, ImlibImage * image);
  Pixmap              Imlib_move_mask(ImlibData * id, ImlibImage * image);
  void                Imlib_destroy_image(ImlibData * id, ImlibImage * image);
  void                Imlib_kill_image(ImlibData * id, ImlibImage * image);
  void                Imlib_free_colors(ImlibData * id);
  void                Imlib_free_pixmap(ImlibData * id, Pixmap pixmap);
  void                Imlib_get_image_border(ImlibData * id, ImlibImage * image, ImlibBorder * border);
  void                Imlib_set_image_border(ImlibData * id, ImlibImage * image, ImlibBorder * border);
  void                Imlib_get_image_shape(ImlibData * id, ImlibImage * image, ImlibColor * color);
  void                Imlib_set_image_shape(ImlibData * id, ImlibImage * image, ImlibColor * color);
  int                 Imlib_save_image_to_eim(ImlibData * id, ImlibImage * image, char *file);
  int                 Imlib_add_image_to_eim(ImlibData * id, ImlibImage * image, char *file);
  int                 Imlib_save_image_to_ppm(ImlibData * id, ImlibImage * image, char *file);
  int                 Imlib_load_file_to_pixmap(ImlibData * id, char *filename, Pixmap * pmap, Pixmap * mask);
  void                Imlib_set_image_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_set_image_red_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_set_image_green_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_set_image_blue_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_get_image_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_get_image_red_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_get_image_green_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_get_image_blue_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod);
  void                Imlib_set_image_red_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_set_image_green_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_set_image_blue_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_get_image_red_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_get_image_green_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_get_image_blue_curve(ImlibData * id, ImlibImage * im, unsigned char *mod);
  void                Imlib_apply_modifiers_to_rgb(ImlibData * id, ImlibImage * im);
  void                Imlib_changed_image(ImlibData * id, ImlibImage * im);
  void                Imlib_apply_image(ImlibData * id, ImlibImage * im, Window p);
  void                Imlib_paste_image(ImlibData * id, ImlibImage * im, Window p, int x, int y, int w, int h);
  void                Imlib_paste_image_border(ImlibData * id, ImlibImage * im, Window p, int x, int y, int w, int h);
  void		      Imlib_bevel_image(ImlibData *id, ImlibImage *im, ImlibBorder *bord, unsigned char up);
  void		      Imlib_bevel_pixmap(ImlibData *id, Pixmap p, int w, int h, ImlibBorder *bord, unsigned char up);
  void                Imlib_flip_image_horizontal(ImlibData * id, ImlibImage * im);
  void                Imlib_flip_image_vertical(ImlibData * id, ImlibImage * im);
  void                Imlib_rotate_image(ImlibData * id, ImlibImage * im, int d);
  ImlibImage         *Imlib_create_image_from_data(ImlibData * id, unsigned char *data, unsigned char *alpha, int w, int h);
  ImlibImage         *Imlib_clone_image(ImlibData * id, ImlibImage * im);
  ImlibImage         *Imlib_clone_scaled_image(ImlibData * id, ImlibImage * im, int w, int h);
  int                 Imlib_get_fallback(ImlibData * id);
  void                Imlib_set_fallback(ImlibData * id, int fallback);
  Visual             *Imlib_get_visual(ImlibData * id);
  Colormap            Imlib_get_colormap(ImlibData * id);
  char               *Imlib_get_sysconfig(ImlibData * id);
  ImlibImage         *Imlib_create_image_from_xpm_data(ImlibData * id, char **data);
  int                 Imlib_data_to_pixmap(ImlibData * id, char **data, Pixmap * pmap, Pixmap * mask);
  void                Imlib_crop_image(ImlibData * id, ImlibImage * im, int x, int y, int w, int h);
  int                 Imlib_save_image(ImlibData * id, ImlibImage * im, char *file, ImlibSaveInfo * info);
  ImlibImage         *Imlib_crop_and_clone_image(ImlibData * id, ImlibImage * im, int x, int y, int w, int h);
  ImlibImage         *Imlib_create_image_from_drawable(ImlibData * id, Drawable win, Pixmap mask, int x, int y, int width, int height);
  ImlibImage         *Imlib_inlined_png_to_image(ImlibData *id, unsigned char *data, int data_size);
      
#ifdef __cplusplus
}
#endif				/* __cplusplus */

#endif
