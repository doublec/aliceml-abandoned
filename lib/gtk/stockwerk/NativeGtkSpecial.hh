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

#endif
