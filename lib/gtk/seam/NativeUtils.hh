#ifndef _UNSAFE_UTILS_HH_
#define _UNSAFE_UTILS_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>


inline Record *CreateRecord(int size) {
  return Record::New(size);
}

/*
TYPE CHECKING HAS BEEN COMPLETELY DISABLED (SLOW; AND IS DONE BY GTK ANYWAY)

static word TypeMismatchConstructor;

  TypeMismatchConstructor =
    UniqueConstructor::New(String::New("GtkTypes.TypeMismatch"))->ToWord();
  RootSet::Add(TypeMismatchConstructor);

word createExn(void *pointer, const gchar *tname, const gchar* funname, 
	       int argno) {
  char err[4000];
  g_snprintf(err, 4000, 
	     "%s: Type mismatch in argument %d: needed type %s, got type %s", 
	     funname, argno+1, tname, G_OBJECT_TYPE_NAME(pointer));            
  g_print("%s\n", err);                                       
  ConVal *conVal =
    ConVal::New(Constructor::FromWordDirect(TypeMismatchConstructor), 1);
  conVal->Init(0, String::New(err)->ToWord());     
  return conVal->ToWord();
}


#ifdef DEBUG
#define CHECK_TYPE(pointer, tname, funname, argno) {                         \
  if (G_IS_OBJECT(pointer) &&                                                \
      g_type_is_a(G_OBJECT_TYPE(pointer), g_type_from_name(tname)) == FALSE) \
      { RAISE(createExn(pointer,tname,funname,argno)); }                     \
  }
#else

#define CHECK_TYPE(pointer, tname, funname, argno) ;
#endif
*/

enum { TYPE_GTK_OBJECT, TYPE_G_OBJECT, TYPE_OWN, TYPE_UNKNOWN };

/***********************************************************************/

enum { BOOL, EVENT, INT, LIST, OBJECT, REAL, STRING };

#define VDATA_MAX_LEN 1024
#define ELLIP_MAX_ARGS 1

#define __PUT_VALUE(vtype, F, x, pos) {    \
  F(value, x);                             \
  memcpy(pos, &value, sizeof(vtype));      \
  pos += sizeof(vtype);                    \
}

#define __PUT_VALIST_ITEM(pos, end, tvw) {          \
  TagVal *tv = TagVal::FromWord(tvw);               \
  if (pos < end) {                                  \
    switch (tv->GetTag()) {                         \
    case BOOL: __PUT_VALUE(bool, DECLARE_BOOL, tv->Sel(0), pos); break;       \
    case EVENT:  break; \
    case INT:  __PUT_VALUE(int,  DECLARE_INT, tv->Sel(0), pos);  break;       \
    case LIST:   break; \
    case OBJECT: __PUT_VALUE(void*,DECLARE_OBJECT,tv->Sel(0),pos);  break; \
    case REAL: __PUT_VALUE(double, DECLARE_CDOUBLE, tv->Sel(0), pos); break;  \
    case STRING: __PUT_VALUE(char*, DECLARE_CSTRING, tv->Sel(0), pos); break; \
    }                                                                         \
  }                                                                           \
}

#define DECLARE_VALIST(l, x)                                \
  gint8 l[VDATA_MAX_LEN+20];                                \
  memset(l, 0, sizeof(l));                                  \
  {                                                         \
    gint8 *l##__pos = l, *l##__end = &(l[VDATA_MAX_LEN]);   \
    DECLARE_LIST_ELEMS(l##__tagval, l##__length, x,         \
      __PUT_VALIST_ITEM(l##__pos, l##__end, l##__tagval->Sel(0)) ); \
  }                                                       

//#define DECLARE_ELLIPSES(l, x)                              \
//  int l[ELLIP_MAX_ARGS+1];                                  \
//  memset(l, 0, sizeof(l));                                  \
//  {                                                         \
//    gint8 *l##__pos = (gint8*)l, *l##__end = (gint8*)&(l[ELLIP_MAX_ARGS]);  \
//    DECLARE_LIST_ELEMS(l##__tagval, l##__length, x,         \
//      __PUT_VALIST_ITEM(l##__pos, l##__end, l##__tagval) ); \
//  }                                                         

#define DECLARE_ELLIPSES(l, x)            \
  double l[2];                            \
  memset(l, 0, sizeof(l));                \
  {                                       \
    gint8 *l##__pos = (gint8*)l, *l##__end = (gint8*)&(l[1]); \
    __PUT_VALIST_ITEM(l##__pos, l##__end, x);                 \
  }                                                         

/***********************************************************************/

// macros for extracting C pointer from an object tuple;
// the pointer itself cannot be a transient, as the object tuples are
// always created by the native components
#define DECLARE_OBJECT(pointer, x)                               \
  DECLARE_TUPLE(pointer##__tup,x);                               \
  void *pointer = Store::WordToUnmanagedPointer(pointer##__tup->Sel(0));

#define DECLARE_OBJECT_WITH_TYPE(pointer, type, x)               \
  DECLARE_TUPLE(pointer##__tup,x);                               \
  int type = Store::WordToInt(pointer##__tup->Sel(1));           \
  void *pointer = Store::WordToUnmanagedPointer(pointer##__tup->Sel(0));


inline void print_type(char *s, void *obj) {
  GObject *p = reinterpret_cast<GObject*>(obj);
  GTypeQuery q;
  memset(&q, 0, sizeof(q));
  g_type_query(G_OBJECT_TYPE(p), &q);
  g_message("%s: %p (type %s)", s, p, q.type_name);
}

inline void __refObject(void *p, int type) {
  if (!p) return;
  //g_message("reffing: %p (type %d)", p, type);
  switch (type) {
  case TYPE_GTK_OBJECT: 
    g_object_ref(G_OBJECT(p));
    gtk_object_sink(GTK_OBJECT(p));
    //print_type("gtk-object-reffed", p);
    break;
  case TYPE_G_OBJECT: 
    g_object_ref(p);
    //print_type("gobject-reffed", p);
    break;
  }
}

inline void __unrefObject(void *p, int type) {
  if (!p) return;
  switch (type) {
  case TYPE_GTK_OBJECT: 
  case TYPE_G_OBJECT: 
    //print_type("about to unref", p);
    //g_message("refcnt: %d", G_OBJECT(p)->ref_count);
    g_object_unref(G_OBJECT(p));
    // print_type("object-unreffed", p); // crashes if ref_count == 0
    break;
  case TYPE_OWN:
    delete (int*)p;
    //g_message("deleted: %p", p);
    break;
  }
}

inline word PointerToObject(void *p, int type) {
  __refObject(p, type);
  Tuple *t = Tuple::New(2);
  t->Init(0,Store::UnmanagedPointerToWord(p));
  t->Init(1,Store::IntToWord(type));
  return t->ToWord();
}

#define DECLARE_GLIST(l, x, ltype, ltype2, F)               \
  ltype *l = NULL;                                          \
  {                                                         \
    DECLARE_LIST_ELEMS(l##__tagval, l##__length, x,         \
      { F(l##__value,l##__tagval->Sel(0));                  \
        l = ltype2##_append(l, l##__value);                 \
      } );                                                  \
  }

#define __RETURN_LIST_HELP(lname,ltype,convertfun)          \
  word tail = Store::IntToWord(Types::nil);                 \
  for (guint i = ltype##_length(lname); i > 0; i--) {       \
    TagVal *cons = TagVal::New(0,2);                        \
    cons->Init(0,convertfun(ltype##_nth_data(lname,i-1)));  \
    cons->Init(1,tail);                                     \
    tail = cons->ToWord();                                  \
  }                                                         \
  return tail;

#define __OBJECT_TO_WORD(p) \
  PointerToObject(p, TYPE_UNKNOWN)

inline word GListToObjectList(GList *list) {
  __RETURN_LIST_HELP(list, g_list, __OBJECT_TO_WORD);
}

inline word GSListToObjectList(GSList *list) {
  __RETURN_LIST_HELP(list, g_slist, __OBJECT_TO_WORD);
}

inline word GListToStringList(GList *list) {
  __RETURN_LIST_HELP(list, g_list, STRING_TO_WORD);
}

inline word GSListToStringList(GSList *list) {
  __RETURN_LIST_HELP(list, g_slist, STRING_TO_WORD);
}


#endif
