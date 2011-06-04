//
// Author:
//   Gareth Smith <garethdanielsmith@gmail.com>
//


#include <gtk/gtk.h>
#include "MyNativeAuthoring.hh"
#include "NativeUtils.hh"


#define DECLARE_BUILDER(b, x) \
  GtkBuilder *b; \
  { \
    DECLARE_OBJECT(b ## Void, x); \
    b = GTK_BUILDER(b ## Void); \
  }


// TODO: make work on windows


word alice_cons();


DEFINE0(GtkBuilder_new) {
  GtkBuilder* b = gtk_builder_new();
  RETURN(OBJECT_TO_WORD(b, TYPE_G_OBJECT, G_TYPE_OBJECT));
} END


DEFINE2(GtkBuilder_addFromFile) {
  DECLARE_BUILDER(b, x0);
  DECLARE_STRING(path, x1);
  
  GError *err = NULL;
  gtk_builder_add_from_file(b, path->ExportC(), &err);
  CHECK_GERROR(err);
  
  RETURN_UNIT;
} END


DEFINE2(GtkBuilder_addFromString) {
  DECLARE_BUILDER(b, x0);
  DECLARE_STRING(str, x1);
  
  GError *err = NULL;
  gtk_builder_add_from_string(b, str->ExportC(), str->GetSize(), &err);
  CHECK_GERROR(err);
  
  RETURN_UNIT;
} END


DEFINE2(GtkBuilder_getObject) {
  DECLARE_BUILDER(b, x0);
  DECLARE_STRING(name, x1);
  
  GObject *obj = gtk_builder_get_object(b, name->ExportC());
  if (obj == NULL) {
    GError *err = g_error_new(GTK_BUILDER_ERROR, 0, "no object with name: %s", name->ExportC());
    RAISE_GERROR(err);
  }
  
  RETURN(OBJECT_TO_WORD(obj, TYPE_G_OBJECT));
} END


DEFINE1(GtkBuilder_getObjects) {
  DECLARE_BUILDER(b, x0);
  
  GSList *objs = gtk_builder_get_objects(b);
  
  word ls = Store::IntToWord(Types::nil);
  GSList *cur = objs;
  while (cur != NULL) {
    GObject *obj = G_OBJECT(cur->data);
    
    const char *name;
    if (GTK_IS_BUILDABLE(obj)) {
      name = gtk_buildable_get_name(GTK_BUILDABLE(obj));
    }
    else {
      name = static_cast<const char*>(g_object_get_data(obj, "gtk-builder-name"));
    }
    
    Tuple *el = Tuple::New(2);
    el->Init(0, String::New(name)->ToWord());
    el->Init(1, OBJECT_TO_WORD(obj, TYPE_G_OBJECT));
    ls = alice_cons(el->ToWord(), ls);
    
    cur = cur->next;
  }
  g_slist_free(objs);
  
  RETURN(ls);
} END


static void GetSignalsConnectFunc(GtkBuilder *builder,
                                  GObject *object,
                                  const char *signal_name,
                                  const char *handler_name,
                                  GObject *connect_object,
                                  GConnectFlags flags,
                                  gpointer user_data) {
  
  GtkBuildable *obj = GTK_BUILDABLE(object);
      
  Tuple *el = Tuple::New(4);
  el->Init(0, String::New(gtk_buildable_get_name(obj))->ToWord());
  el->Init(1, String::New(signal_name)->ToWord());
  el->Init(2, String::New(handler_name)->ToWord());
  el->Init(3, Store::IntToWord(flags & G_CONNECT_AFTER ? Types::_true : Types::_false));
  
  Cell *ls = static_cast<Cell*>(user_data);
  ls->Assign(alice_cons(el->ToWord(), ls->Access()));
}


DEFINE1(GtkBuilder_getSignals) {
  DECLARE_BUILDER(b, x0);
  
  Cell *lsRef = Cell::New();
  lsRef->Init(Store::IntToWord(Types::nil));
  
  gtk_builder_connect_signals_full(b, GetSignalsConnectFunc, lsRef);
  
  RETURN(lsRef->Access());
} END

 
DEFINE2(GtkBuilder_setTranslationDomain) {
  DECLARE_BUILDER(b, x0);
  if (Store::WordToTransient(x1) != INVALID_POINTER) { REQUEST(x1); }
  
  TagVal *someDom = TagVal::FromWord(x1);
  if (someDom == INVALID_POINTER) { // x1 = NONE
    gtk_builder_set_translation_domain(b, NULL);
  }
  else {
    String *dom = String::FromWord(someDom->Sel(0));
    if (dom == INVALID_POINTER) {
      REQUEST(someDom->Sel(0));
    }
    gtk_builder_set_translation_domain(b, dom->ExportC());
  }
  
  RETURN_UNIT;
} END


DEFINE1(GtkBuilder_getTranslationDomain) {
  DECLARE_BUILDER(b, x0);
  
  const gchar *dom = gtk_builder_get_translation_domain(b);
  if (dom == NULL) {
    RETURN(Store::IntToWord(Types::NONE));
  }
  else {
    TagVal *someDom = TagVal::New(Types::SOME, 1);
    someDom->Init(0, String::New(dom)->ToWord());
    RETURN(someDom->ToWord());
  }
  
} END


word NativeGtkBuilder_CreateComponent () {
  Record *record = Record::New(8);
    
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "new", GtkBuilder_new, 0);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "addFromFile", GtkBuilder_addFromFile, 2);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "addFromString", GtkBuilder_addFromString, 2);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "getObject", GtkBuilder_getObject, 2);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "getObjects", GtkBuilder_getObjects, 1);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "getSignals", GtkBuilder_getSignals, 1);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "setTranslationDomain", GtkBuilder_setTranslationDomain, 2);
  INIT_STRUCTURE(record, "NativeLibs.NativeGtkBuilder", "getTranslationDomain", GtkBuilder_getTranslationDomain, 1);

  return record->ToWord();
}
