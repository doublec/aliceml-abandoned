#ifndef _UNSAFE_UTILS_HH_
#define _UNSAFE_UTILS_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>


Record *CreateRecord(int size) {
  Record *record = Record::New(size);
  return record;
}

/*
static word wDict;

static void AddObject(GtkObject *object) {
  static s_int key     = 0;
  WeakDictionary *dict = WeakDictionary::FromWordDirect(wDict);
  dict->InsertItem(key++, Store::UnmanagedPointerToWord(object));
}
*/
//  RootSet::Add(wDict);
//  wDict = WeakDictionary::New(100, new GtkFinalize());


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

enum { TYPE_GTK_OBJECT, TYPE_GTK_WIDGET, TYPE_G_OBJECT, 
       TYPE_OWN, TYPE_UNKNOWN };

/***********************************************************************/

enum { BOOL, EVENT, INT, LIST, OBJECT, REAL, STRING };

#define VDATA_MAX_LEN 1024
#define ELLIP_MAX_ARGS 10

#define __PUT_VALUE(vtype, F, x, pos) {    \
  F(value, x);                             \
  memcpy(pos, &value, sizeof(vtype));      \
  pos += sizeof(vtype);                    \
}

#define __PUT_VALIST_ITEM(pos, end, listitem) {     \
  TagVal *tv = TagVal::FromWord(listitem->Sel(0));  \
  if (pos < end) {                                  \
    switch (tv->GetTag()) {                         \
    case BOOL: __PUT_VALUE(bool, DECLARE_BOOL, tv->Sel(0), pos); break;       \
    case EVENT:  break; \
    case INT:  __PUT_VALUE(int,  DECLARE_INT, tv->Sel(0), pos);  break;       \
    case LIST:   break; \
    case OBJECT: __PUT_VALUE(void*,DECLARE_UNMANAGED_POINTER,tv->Sel(0),pos); \
                 break; \
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
      __PUT_VALIST_ITEM(l##__pos, l##__end, l##__tagval) ); \
  }                                                       

#define DECLARE_ELLIPSES(l, x)                              \
  int l[ELLIP_MAX_ARGS+1];                                  \
  memset(l, 0, sizeof(l));                                  \
  {                                                         \
    gint8 *l##__pos = (gint8*)l, *l##__end = (gint8*)&(l[ELLIP_MAX_ARGS]);  \
    DECLARE_LIST_ELEMS(l##__tagval, l##__length, x,         \
      __PUT_VALIST_ITEM(l##__pos, l##__end, l##__tagval) ); \
  }                                                         

/***********************************************************************/


#define DECLARE_GLIST(l, x, ltype, ltype2, F)               \
  ltype *l = NULL;                                          \
  {                                                         \
    DECLARE_LIST_ELEMS(l##__tagval, l##__length, x,         \
      { F(l##__value,l##__tagval->Sel(0));                  \
        l = ltype2##_append(l, l##__value);                 \
      } );                                                  \
  }

#define __RETURN_STRING(s) \
  String::New(reinterpret_cast<const char *>(s))->ToWord()

#define __RETURN_LIST_HELP(lname,ltype,convertfun)          \
  word tail = Store::IntToWord(Types::nil);                 \
  for (guint i = ltype##_length(lname); i > 0; i--) {       \
    TagVal *cons = TagVal::New(0,2);                        \
    cons->Init(0,convertfun(ltype##_nth_data(lname,i-1)));  \
    cons->Init(1,tail);                                     \
    tail = cons->ToWord();                                  \
  }                                                         \
  return tail;

inline word GListToObjectList(GList *list) {
  __RETURN_LIST_HELP(list, g_list, Store::UnmanagedPointerToWord);
}

inline word GSListToObjectList(GSList *list) {
  __RETURN_LIST_HELP(list, g_slist, Store::UnmanagedPointerToWord);
}

inline word GListToStringList(GList *list) {
  __RETURN_LIST_HELP(list, g_list, __RETURN_STRING);
}

inline word GSListToStringList(GSList *list) {
  __RETURN_LIST_HELP(list, g_slist, __RETURN_STRING);
}


#endif
