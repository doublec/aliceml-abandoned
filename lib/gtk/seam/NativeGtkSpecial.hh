//
// Author:
//   Robert Grabowski <grabow@ps.uni-sb.de>
//
// Copyright:
//   Robert Grabowski, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef _NATIVE_GTK_SPECIAL_HH_
#define _NATIVE_GTK_SPECIAL_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>

DEFINE0(NativeGtk_textIterNew) {
  GtkTextIter *iter = new GtkTextIter;
  RETURN(OBJECT_TO_WORD(iter,TYPE_OWN));
} END

DEFINE1(NativeGtk_textIterCopy) {
  DECLARE_OBJECT(in0, x0);
  GtkTextIter* ret =
    (GtkTextIter*)gtk_text_iter_copy(((const GtkTextIter*)(in0)));
  RETURN1(OBJECT_TO_WORD(ret, TYPE_OWN));
} END

DEFINE0(NativeGtk_treeIterNew) {
  GtkTreeIter *iter = new GtkTreeIter;
  RETURN(OBJECT_TO_WORD(iter,TYPE_OWN));
} END

DEFINE0(NativeGtk_treeStoreNew) {
  GtkTreeStore *store = gtk_tree_store_new(1, G_TYPE_STRING);
  RETURN(OBJECT_TO_WORD(store,TYPE_G_OBJECT));
} END

DEFINE3(NativeGtk_treeModelGetStringAt) {
  DECLARE_OBJECT(mod, x0);
  DECLARE_OBJECT(path, x1);
  DECLARE_INT(col, x2);
  GtkTreeModel *model = static_cast<GtkTreeModel*>(mod);
  gchar *result = "";
  GValue *val;
  GtkTreeIter *iter;
  if (gtk_tree_model_get_iter(model, iter, static_cast<GtkTreePath*>(path))) {
    gtk_tree_model_get_value(model, iter, col, val);
    if (G_VALUE_TYPE(val) == G_TYPE_STRING)
      result = g_value_dup_string(val);
  }
  RETURN(STRING_TO_WORD(result));
} END

DEFINE1(NativeGtk_treeViewGetSelectedString) {
  DECLARE_OBJECT(t, x0);
  GtkTreeView *tree = static_cast<GtkTreeView*>(t);
  GtkTreeIter iter;
  GValue val;
  memset(&val, 0, sizeof(GValue));
  char *result = "";
  if (tree && 
      gtk_tree_selection_get_selected(gtk_tree_view_get_selection(tree), 
				      NULL, &iter)) {
    gtk_tree_model_get_value(gtk_tree_view_get_model(tree), &iter, 0, &val);
    if (G_VALUE_TYPE(&val) == G_TYPE_STRING)
      result = g_value_dup_string(&val);
      g_value_unset(&val);
  }
  RETURN(STRING_TO_WORD(result));
} END

DEFINE1(NativeGtk_radioButtonNew) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  
  GtkWidget* ret =
    gtk_radio_button_new(l);
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_radioButtonNewWithLabel) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  DECLARE_CSTRING(in1, x1);
  
  GtkWidget* ret =
    gtk_radio_button_new_with_label(l, reinterpret_cast<gchar*>(in1));
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_radioButtonNewWithMnemonic) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  DECLARE_CSTRING(in1, x1);

  GtkWidget* ret =
    gtk_radio_button_new_with_label(l, reinterpret_cast<gchar*>(in1));
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE1(NativeGtk_radioButtonGetGroup) {
  DECLARE_OBJECT(t, x0);
  GtkRadioButton *i = static_cast<GtkRadioButton *>(t);
  GSList* l = gtk_radio_button_get_group(i);
  RETURN1(OBJECT_TO_WORD(l));
} END

DEFINE2(NativeGtk_radioButtonSetGroup) {
  DECLARE_OBJECT(t, x0);
  DECLARE_OBJECT(g, x1);
  GtkRadioButton *i = static_cast<GtkRadioButton *>(t);
  GSList* l = static_cast<GSList *>(g);
  gtk_radio_button_set_group(i,l);
  RETURN_UNIT;
} END

DEFINE1(NativeGtk_radioMenuItemGetGroup) {
  DECLARE_OBJECT(t, x0);
  GtkRadioMenuItem *i = static_cast<GtkRadioMenuItem *>(t);
  GSList* l = gtk_radio_menu_item_get_group(i);
  RETURN1(OBJECT_TO_WORD(l));
} END

DEFINE2(NativeGtk_radioMenuItemSetGroup) {
  DECLARE_OBJECT(t, x0);
  DECLARE_OBJECT(g, x1);
  GtkRadioMenuItem *i = static_cast<GtkRadioMenuItem *>(t);
  GSList* l = static_cast<GSList *>(g);
  gtk_radio_menu_item_set_group(i,l);
  RETURN_UNIT;
} END

DEFINE1(NativeGtk_radioMenuItemNew) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  
  GtkWidget* ret =
    gtk_radio_menu_item_new(l);
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_radioMenuItemNewWithLabel) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  DECLARE_CSTRING(in1, x1);
  
  GtkWidget* ret =
    gtk_radio_menu_item_new_with_label(l, reinterpret_cast<gchar*>(in1));
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_radioMenuItemNewWithMnemonic) {
  DECLARE_OBJECT(g, x0);
  GSList *l = static_cast<GSList *>(g);
  DECLARE_CSTRING(in1, x1);
  
  GtkWidget* ret =
    gtk_radio_menu_item_new_with_mnemonic(l, reinterpret_cast<gchar*>(in1));
  RETURN1(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_widgetSetFlags) {
  DECLARE_OBJECT(t, x0);
  DECLARE_INT(flags, x1);
  GtkWidget *w = static_cast<GtkWidget *>(t);
  GTK_WIDGET_SET_FLAGS(w,flags);
  RETURN_UNIT;
} END

DEFINE2(NativeGtk_widgetUnsetFlags) {
  DECLARE_OBJECT(t, x0);
  DECLARE_INT(flags, x1);
  GtkWidget *w = static_cast<GtkWidget *>(t);
  GTK_WIDGET_UNSET_FLAGS(w,flags);
  RETURN_UNIT;
} END

DEFINE2(NativeGtk_objectGetObject) {
  DECLARE_OBJECT(obj, x0);
  DECLARE_STRING(property, x1);

  GtkObject* ret;
  g_object_get(obj, property->ExportC(),
               &ret,
               NULL);
  RETURN(OBJECT_TO_WORD(ret, TYPE_GTK_OBJECT));
} END

DEFINE2(NativeGtk_objectGetDouble) {
  DECLARE_OBJECT(obj, x0);
  DECLARE_STRING(property, x1);

  double ret;
  g_object_get(obj, property->ExportC(),
               &ret,
               NULL);
  RETURN_REAL(ret);
} END

DEFINE3(NativeGtk_objectSetObject) {
  DECLARE_OBJECT(obj, x0);
  DECLARE_STRING(property, x1);
  DECLARE_OBJECT(val, x2);

  g_object_set(obj, property->ExportC(), val, NULL);
  RETURN_UNIT;
} END

DEFINE3(NativeGtk_objectSetBool) {
  DECLARE_OBJECT(obj, x0);
  DECLARE_STRING(property, x1);
  DECLARE_BOOL(val, x2);

  g_object_set(obj, property->ExportC(), val, NULL);
  RETURN_UNIT;
} END

static void specialDeleteRangeEvent(GObject *obj,
                                    GtkTextIter *iter1,
                                    GtkTextIter *iter2, gpointer) {
  g_signal_emit_by_name(obj,
                        "delete-text",
                        gtk_text_iter_get_offset(iter1),
                        gtk_text_iter_get_offset(iter2),
                        gtk_text_iter_get_text(iter1, iter2));
}

DEFINE1(NativeGtk_textBufferConnectSpecialSignals) {
  DECLARE_OBJECT(obj, x0);

  g_signal_connect (obj, "delete-range",
                    (GCallback) specialDeleteRangeEvent,
                    NULL);
  RETURN_UNIT;
} END

DEFINE1(NativeGtk_pangoFontDescriptionFromString) {
  DECLARE_STRING(str, x0);
  PangoFontDescription *fontDesc =
    pango_font_description_from_string(str->ExportC());
  RETURN(OBJECT_TO_WORD(fontDesc, TYPE_UNKNOWN));
} END

// void gtk_widget_add_accelerator(GtkWidget*, const guchar*, GtkAccelGroup*, int, int, int)
DEFINE6(NativeGtk_widgetAddAccelerator) {
  DECLARE_OBJECT(in0, x0);
  DECLARE_CSTRING(in1, x1);
  DECLARE_OBJECT(in2, x2);
  DECLARE_INT(in3, x3);
  DECLARE_INT(in4, x4);
  DECLARE_INT(in5, x5);
  gtk_widget_add_accelerator(((GtkWidget*)(in0)), ((const gchar*)(in1)), ((GtkAccelGroup*)(in2)), ((int)(in3)), ((GdkModifierType)(in4)), ((GtkAccelFlags)(in5)));
  RETURN_UNIT;
} END

#endif
