#ifndef _NATIVE_GTK_SPECIAL_HH_
#define _NATIVE_GTK_SPECIAL_HH_

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include <gtk/gtk.h>

DEFINE1(NativeGtk_TypeMismatch) {
  Constructor *ccVal = Constructor::FromWord(TypeMismatchConstructor);
  ConVal *conVal     = ConVal::New(ccVal, 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

void NativeGtkSpecialInit(Record *record) {
  record->Init("'TypeMismatch", TypeMismatchConstructor);
  INIT_STRUCTURE(record, "NativeGtk", "TypeMismatch",
		 NativeGtk_TypeMismatch, 1, true);
}

DEFINE0(NativeGtk_init) {
  gtk_init(NULL,NULL);
  RETURN_UNIT;
} END

DEFINE0(NativeGtk_null) {
  RETURN(Store::UnmanagedPointerToWord(NULL));
} END

DEFINE0(NativeGtk_gtkTrue) {
  RETURN(Store::IntToWord(TRUE));
} END

DEFINE0(NativeGtk_gtkFalse) {
  RETURN(Store::IntToWord(FALSE));
} END

DEFINE0(NativeGtk_textIterNew) {
  GtkTextIter *iter = new GtkTextIter;
  RETURN(Store::UnmanagedPointerToWord(iter));
} END

DEFINE0(NativeGtk_treeIterNew) {
  GtkTreeIter *iter = new GtkTreeIter;
  RETURN(Store::UnmanagedPointerToWord(iter));
} END

DEFINE0(NativeGtk_treeStoreNew) {
  GtkTreeStore *store = gtk_tree_store_new(1, G_TYPE_STRING);
  RETURN(Store::UnmanagedPointerToWord(store));
} END

DEFINE3(NativeGtk_treeModelGetStringAt) {
  DECLARE_UNMANAGED_POINTER(mod, x0);
  DECLARE_UNMANAGED_POINTER(path, x1);
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
  RETURN(String::New(reinterpret_cast<const char *>(result))->ToWord());
} END

DEFINE1(NativeGtk_treeViewGetSelectedString) {
  DECLARE_UNMANAGED_POINTER(t, x0);
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
  RETURN(String::New(reinterpret_cast<const char*>(result))->ToWord());
} END

////////////////////////////////////////////////////////////////////////

static word tail = 0;

DEFINE0(NativeGtk_getEventStream) {
  if (tail) {
    ConVal *conVal =
      ConVal::New(Constructor::FromWordDirect(TypeMismatchConstructor), 1);
    conVal->Init(0, String::New("stream exists")->ToWord());     
    RAISE(conVal->ToWord());
  }
  else {
    tail = (Future::New())->ToWord();
    RootSet::Add(tail);
    RETURN(tail);
  }
} END

void push_front(word *list, word value) {
  TagVal *cons = TagVal::New(0,2);
  cons->Init(0,value);
  cons->Init(1,*list);
  *list = cons->ToWord();
}

word create_param(int tag, word value) {
  TagVal *param = TagVal::New(tag,1);
  param->Init(0, value);
  return param->ToWord();
}

word GdkScrollDirectionToDatatype(GdkScrollDirection dir) {
  enum { SCROLL_DOWN, SCROLL_LEFT, SCROLL_RIGHT, SCROLL_UP };
  switch (dir) {
  case GDK_SCROLL_UP: return Store::IntToWord(SCROLL_UP);
  case GDK_SCROLL_DOWN: return Store::IntToWord(SCROLL_DOWN);
  case GDK_SCROLL_LEFT: return Store::IntToWord(SCROLL_LEFT);
  }
  return Store::IntToWord(SCROLL_RIGHT);
}

word GdkCrossingModeToDatatype(GdkCrossingMode mode) {
  enum { CROSSING_GRAB, CROSSING_NORMAL, CROSSING_UNGRAB };
  switch (mode) {
  case GDK_CROSSING_NORMAL: return Store::IntToWord(CROSSING_NORMAL);
  case GDK_CROSSING_GRAB: return Store::IntToWord(CROSSING_GRAB);
  }
  return Store::IntToWord(CROSSING_UNGRAB);
}

word GdkNotifyTypeToDatatype(GdkNotifyType type) {
  enum { NOTIFY_ANCESTOR, NOTIFY_INFERIOR, NOTIFY_NONLINEAR, 
	 NOTIFY_NONLINEAR_VIRTUAL, NOTIFY_UNKNOWN, NOTIFY_VIRTUAL };
  switch (type) {
  case GDK_NOTIFY_ANCESTOR: return Store::IntToWord(NOTIFY_ANCESTOR);
  case GDK_NOTIFY_VIRTUAL: return Store::IntToWord(NOTIFY_VIRTUAL);
  case GDK_NOTIFY_INFERIOR: return Store::IntToWord(NOTIFY_INFERIOR);
  case GDK_NOTIFY_NONLINEAR: return Store::IntToWord(NOTIFY_NONLINEAR);
  case GDK_NOTIFY_NONLINEAR_VIRTUAL: 
    return Store::IntToWord(NOTIFY_NONLINEAR_VIRTUAL);
  }
  return Store::IntToWord(NOTIFY_UNKNOWN);
}

word GdkVisibilityStateToDatatype(GdkVisibilityState state) {
  enum {  VISIBILITY_FULLY_OBSCURED, VISIBILITY_PARTIAL, 
	  VISIBILITY_UNOBSCURED };
  switch (state) {
  case GDK_VISIBILITY_FULLY_OBSCURED: 
    return Store::IntToWord(VISIBILITY_FULLY_OBSCURED);
  case GDK_VISIBILITY_PARTIAL: return Store::IntToWord(VISIBILITY_PARTIAL);
  }
  return Store::IntToWord(VISIBILITY_UNOBSCURED);
}

word GdkEventToDatatype(GdkEvent *event) {
  enum { _2BUTTON_PRESS, _3BUTTON_PRESS, BUTTON_PRESS, BUTTON_RELEASE, 
	 CLIENT_EVENT,
	 CONFIGURE,
	 DELETE, DESTROY,
	 DRAG_ENTER, DRAG_LEAVE, DRAG_MOTION, DRAG_STATUS,
	 DROP_FINISHED, DROP_START, 
	 ENTER_NOTIFY, 
	 EXPOSE,
	 FOCUS_CHANGE, 
	 KEY_PRESS, KEY_RELEASE,
	 LEAVE_NOTIFY, 
	 MAP,
	 MOTION_NOTIFY,
	 NO_EXPOSE,
	 NOTHING,
	 PROPERTY_NOTIFY,
	 PROXIMITY_IN, PROXIMITY_OUT,
	 SCROLL, 
	 SELECTION_CLEAR, SELECTION_NOTIFY, SELECTION_REQUEST, 
	 SETTING, 
	 UNMAP,
	 UNSUPPORTED, 
	 VISIBILITY_NOTIFY, 
	 WINDOW_STATE };
  int evtype = UNSUPPORTED;
  Record *r = NULL;

  switch (event->type) {
  case GDK_NOTHING: evtype = NOTHING; break;
  case GDK_DELETE: evtype = DELETE; break;
  case GDK_DESTROY: evtype = DESTROY; break;
  case GDK_EXPOSE: evtype = EXPOSE; break;
  case GDK_MOTION_NOTIFY: evtype = MOTION_NOTIFY; break;
  case GDK_BUTTON_PRESS:   evtype = BUTTON_PRESS; break;
  case GDK_2BUTTON_PRESS:  evtype = _2BUTTON_PRESS; break;
  case GDK_3BUTTON_PRESS:  evtype = _3BUTTON_PRESS; break;
  case GDK_BUTTON_RELEASE: evtype = BUTTON_RELEASE; break;
  case GDK_KEY_PRESS: evtype = KEY_PRESS; break;
  case GDK_KEY_RELEASE: evtype = KEY_RELEASE; break;
  case GDK_ENTER_NOTIFY: evtype = ENTER_NOTIFY; break;
  case GDK_LEAVE_NOTIFY: evtype = LEAVE_NOTIFY; break;
  case GDK_FOCUS_CHANGE: evtype = FOCUS_CHANGE; break;
  case GDK_CONFIGURE: evtype = CONFIGURE; break;
  case GDK_MAP: evtype = MAP; break;
  case GDK_UNMAP: evtype = UNMAP; break;
  case GDK_PROPERTY_NOTIFY: evtype = PROPERTY_NOTIFY; break;
  case GDK_SELECTION_CLEAR: evtype = SELECTION_CLEAR; break;
  case GDK_SELECTION_REQUEST: evtype = SELECTION_REQUEST; break;
  case GDK_SELECTION_NOTIFY: evtype = SELECTION_NOTIFY; break;
  case GDK_PROXIMITY_IN: evtype = PROXIMITY_IN; break;
  case GDK_PROXIMITY_OUT: evtype = PROXIMITY_OUT; break;
  case GDK_DRAG_ENTER: evtype = DRAG_ENTER; break;
  case GDK_DRAG_LEAVE: evtype = DRAG_LEAVE; break;
  case GDK_DRAG_MOTION: evtype = DRAG_MOTION; break;
  case GDK_DRAG_STATUS: evtype = DRAG_STATUS; break;
  case GDK_DROP_START: evtype = DROP_START; break;
  case GDK_DROP_FINISHED: evtype = DROP_FINISHED; break;
  case GDK_CLIENT_EVENT: evtype = CLIENT_EVENT; break;
  case GDK_VISIBILITY_NOTIFY: evtype = VISIBILITY_NOTIFY; break;
  case GDK_NO_EXPOSE: evtype = NO_EXPOSE; break;
  case GDK_SCROLL: evtype = SCROLL; break;
  case GDK_WINDOW_STATE: evtype = WINDOW_STATE; break;
  case GDK_SETTING: evtype = SETTING; break;
}

  g_print("event: Gdk: %d, Alice: %d\n", event->type, evtype);

  switch (event->type) {    

  case GDK_EXPOSE: {
    GdkEventExpose *ev = reinterpret_cast<GdkEventExpose*>(event);
    r = Record::New(8);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("area_x", Store::IntToWord((ev->area).x));
    r->Init("area_y", Store::IntToWord((ev->area).y));
    r->Init("area_width", Store::IntToWord((ev->area).width));
    r->Init("area_height", Store::IntToWord((ev->area).height));
    r->Init("region", Store::UnmanagedPointerToWord(ev->region));
    r->Init("count", Store::IntToWord(ev->count));
    break;
  }

  case GDK_MOTION_NOTIFY: {
    GdkEventMotion *ev = reinterpret_cast<GdkEventMotion*>(event);
    r = Record::New(10);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("time", Store::IntToWord(ev->time));
    r->Init("x", Real::New(ev->x)->ToWord());
    r->Init("y", Real::New(ev->y)->ToWord());
    r->Init("state", Store::IntToWord(ev->state));
    r->Init("is_hint", Store::IntToWord(ev->is_hint));
    r->Init("device", Store::UnmanagedPointerToWord(ev->device));
    r->Init("x_root", Real::New(ev->x_root)->ToWord());
    r->Init("y_root", Real::New(ev->y_root)->ToWord());
    break;
  }

  case GDK_BUTTON_PRESS:
  case GDK_2BUTTON_PRESS:
  case GDK_3BUTTON_PRESS:
  case GDK_BUTTON_RELEASE: {
    GdkEventButton *ev = reinterpret_cast<GdkEventButton*>(event);
    r = Record::New(10);    
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("time", Store::IntToWord(ev->time));
    r->Init("x", Real::New(ev->x)->ToWord());
    r->Init("y", Real::New(ev->y)->ToWord());
    r->Init("state", Store::IntToWord(ev->state));
    r->Init("button", Store::IntToWord(ev->button));
    r->Init("device", Store::UnmanagedPointerToWord(ev->device));
    r->Init("x_root", Real::New(ev->x_root)->ToWord());
    r->Init("y_root", Real::New(ev->y_root)->ToWord());
    break;
  }
 
  case GDK_KEY_PRESS:
  case GDK_KEY_RELEASE: {
    GdkEventKey *ev = reinterpret_cast<GdkEventKey*>(event);
    r = Record::New(9);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("time", Store::IntToWord(ev->time));
    r->Init("state", Store::IntToWord(ev->state));
    r->Init("keyval", Store::IntToWord(ev->keyval));
    r->Init("length", Store::IntToWord(ev->length));
    r->Init("string", String::New(ev->string)->ToWord());
    r->Init("hardware_keycode", Store::IntToWord(ev->hardware_keycode));
    r->Init("group", Store::IntToWord(ev->group));
    break;
  }

  case GDK_ENTER_NOTIFY: 
  case GDK_LEAVE_NOTIFY: {
    GdkEventCrossing *ev = reinterpret_cast<GdkEventCrossing*>(event);    
    r = Record::New(12);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("subwindow", Store::UnmanagedPointerToWord(ev->subwindow));
    r->Init("time", Store::IntToWord(ev->time));
    r->Init("x", Real::New(ev->x)->ToWord());
    r->Init("y", Real::New(ev->y)->ToWord());
    r->Init("x_root", Real::New(ev->x_root)->ToWord());
    r->Init("y_root", Real::New(ev->y_root)->ToWord());
    r->Init("mode", GdkCrossingModeToDatatype(ev->mode));
    r->Init("detail", GdkNotifyTypeToDatatype(ev->detail));
    r->Init("focus", BOOL_TO_WORD(ev->focus));
    r->Init("state", Store::IntToWord(ev->state));
    break;
  }

  case GDK_FOCUS_CHANGE: {
    GdkEventFocus *ev = reinterpret_cast<GdkEventFocus*>(event);
    r = Record::New(3);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("hasFocus", BOOL_TO_WORD(ev->in));
    break;
  }

  case GDK_CONFIGURE: {
    GdkEventConfigure *ev = reinterpret_cast<GdkEventConfigure*>(event);
    r = Record::New(6);    
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("x", Store::IntToWord(ev->x));
    r->Init("y", Store::IntToWord(ev->y));
    r->Init("width", Store::IntToWord(ev->width));
    r->Init("height", Store::IntToWord(ev->height));
    break;
  }
    
  case GDK_VISIBILITY_NOTIFY: {
    GdkEventVisibility *ev = reinterpret_cast<GdkEventVisibility*>(event);
    r = Record::New(3);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("state", GdkVisibilityStateToDatatype(ev->state));
    break;
  }

  case GDK_NO_EXPOSE: {
    GdkEventNoExpose *ev = reinterpret_cast<GdkEventNoExpose*>(event);
    r = Record::New(2);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    break;
  }

  case GDK_SCROLL: {
    GdkEventScroll *ev = reinterpret_cast<GdkEventScroll*>(event);    
    r = Record::New(10);
    r->Init("window", Store::UnmanagedPointerToWord(ev->window));
    r->Init("send", BOOL_TO_WORD(ev->send_event));
    r->Init("time", Store::IntToWord(ev->time));
    r->Init("x", Real::New(ev->x)->ToWord());
    r->Init("y", Real::New(ev->y)->ToWord());
    r->Init("state", Store::IntToWord(ev->state));
    r->Init("direction", GdkScrollDirectionToDatatype(ev->direction));
    r->Init("device", Store::UnmanagedPointerToWord(ev->device));
    r->Init("x_root", Real::New(ev->x_root)->ToWord());
    r->Init("y_root", Real::New(ev->y_root)->ToWord());
    break;
  }
  }

  /*  TagVal *tv = TagVal::New(evtype, (r ? 1 : 0));
  if (r)
    tv->Init(0, r->ToWord());
  */

  // *** workaround
  evtype = UNSUPPORTED;
  TagVal *tv = TagVal::New(evtype, 1);
  tv->Init(0, Store::UnmanagedPointerToWord(event));

  return tv->ToWord();
}

word create_object(GType t, gpointer p) {
  static const GType G_LIST_TYPE = g_type_from_name("GList");
  static const GType G_SLIST_TYPE = g_type_from_name("GSList");
  static const GType GDK_EVENT_TYPE = gdk_event_get_type();

  int tag = OBJECT;
  word value;
  if (g_type_is_a(t, G_LIST_TYPE)) {
    tag = LIST;
    value = GListToObjectList(static_cast<GList*>(p));
  }
  else
    if (g_type_is_a(t, G_SLIST_TYPE)) {
      tag = LIST;
      value = GSListToObjectList(static_cast<GSList*>(p));
    }
    else
      if (t == GDK_EVENT_TYPE) {
	tag = EVENT;
	value = GdkEventToDatatype(static_cast<GdkEvent*>(p));
      }
      else
	value = Store::UnmanagedPointerToWord(p);
  return create_param(tag, value);
}

void sendArgsToStream(gint connid, guint n_param_values, 
		      const GValue *param_values) {
  Future *oldfuture = static_cast<Future*>(Store::WordToTransient(tail));
  tail = (Future::New())->ToWord();
  word stream = tail;
  word paramlist = Store::IntToWord(Types::nil);
  gpointer widget = NULL;

  for (int i = n_param_values-1; i >= 0; i--) {
    word value;

    const GValue *val = param_values + i;
    /*
    GTypeQuery q;
    memset(&q, 0, sizeof(q));
    g_type_query(G_VALUE_TYPE(val), &q);
    g_print("Param #%d, Type: %ld, Name: %s\n", i, G_VALUE_TYPE(val), 
	    q.type_name);
    */
    switch(G_VALUE_TYPE(val)) {
    case G_TYPE_CHAR:   
      value = create_param(INT, 
		   Store::IntToWord(static_cast<int>(g_value_get_char(val))));
      break;
    case G_TYPE_UCHAR:  
      value = create_param(INT, 
                 Store::IntToWord(static_cast<int>(g_value_get_uchar(val))));
      break;
    case G_TYPE_BOOLEAN:
      value = create_param(BOOL, BOOL_TO_WORD(g_value_get_boolean(val)));
      break;
    case G_TYPE_INT:    
      value = create_param(INT, Store::IntToWord(g_value_get_int(val)));
      break;
    case G_TYPE_UINT:   
      value = create_param(INT, Store::IntToWord(g_value_get_uint(val)));
      break;
    case G_TYPE_LONG:   
      value = create_param(INT, Store::IntToWord(g_value_get_long(val)));
      break;
    case G_TYPE_ULONG:  
      value = create_param(INT, Store::IntToWord(g_value_get_ulong(val)));
      break;
    case G_TYPE_INT64:  
      value = create_param(INT, Store::IntToWord(g_value_get_int64(val)));
      break;
    case G_TYPE_UINT64: 
      value = create_param(INT, Store::IntToWord(g_value_get_uint64(val)));
      break;
    case G_TYPE_ENUM:   
      value = create_param(INT, Store::IntToWord(g_value_get_enum(val)));
      break;
    case G_TYPE_FLAGS:  
      value = create_param(INT, Store::IntToWord(g_value_get_flags(val)));
      break;
    case G_TYPE_FLOAT:  
      value = create_param(REAL, Real::New(g_value_get_float(val))->ToWord());
      break;
    case G_TYPE_DOUBLE: 
      value = create_param(REAL, Real::New(g_value_get_double(val))->ToWord());
      break;
    case G_TYPE_STRING: 
     value = create_param(STRING, 
			  String::New(g_value_get_string(val))->ToWord());
      break;
    default:
      //g_print("NAFT AS POINTER: %p\n", g_value_peek_pointer(val));
      if (i==0)
	widget = g_value_peek_pointer(val);
      else {
	//g_print("** %d\n", ((GdkEvent*)g_value_peek_pointer(val))->type); 
	value = create_object(G_VALUE_TYPE(val), g_value_peek_pointer(val));
      }
    }
    if (!widget) push_front(&paramlist,value);
  }
  
  Tuple *tup = Tuple::New(3);
  tup->Init(0,Store::IntToWord(connid));
  tup->Init(1,Store::UnmanagedPointerToWord(widget));
  tup->Init(2,paramlist);
  
  push_front(&stream,tup->ToWord());

  oldfuture->ScheduleWaitingThreads();
  oldfuture->Become(REF_LABEL, stream);
}

void generic_marshaller(GClosure *closure, GValue *return_value, 
			guint n_param_values, const GValue *param_values, 
			gpointer, gpointer marshal_data) {

  gint connid = GPOINTER_TO_INT(marshal_data);
  gint isDelete = GPOINTER_TO_INT(closure->data);

  //  g_print("event occured: %d\n", connid);
  //  g_print("delete?: %d\n", isDelete);

  sendArgsToStream(connid,n_param_values,param_values);

  if (G_VALUE_HOLDS(return_value, G_TYPE_BOOLEAN))
    g_value_set_boolean(return_value, (isDelete != 0) ? TRUE : FALSE);
}

DEFINE3(NativeGtk_signalConnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_CSTRING(signalname,x1);
  DECLARE_BOOL(after,x2);

  gint isDelete = !strcmp(signalname, "delete_event");

  GClosure *closure = g_cclosure_new(G_CALLBACK(generic_marshaller),
  				     GINT_TO_POINTER(isDelete), NULL);
  gulong connid = g_signal_connect_closure(G_OBJECT(obj), signalname, 
					closure, after ? TRUE : FALSE); 
  g_closure_set_meta_marshal(closure,GINT_TO_POINTER(connid),
			     generic_marshaller);
  
  RETURN(Store::IntToWord(static_cast<int>(connid)));
} END

DEFINE2(NativeGtk_signalDisconnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_INT(handler_id,x1);
  g_signal_handler_disconnect(G_OBJECT(obj), static_cast<gulong>(handler_id));
  RETURN_UNIT;
} END

DEFINE1(NativeGtk_gObjectRef) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  RETURN(Store::UnmanagedPointerToWord(g_object_ref(obj)));
} END

DEFINE1(NativeGtk_gObjectUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  g_object_unref(obj);
  RETURN_UNIT;
} END

DEFINE1(NativeGtk_deleteUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  delete (int*)obj;
  RETURN_UNIT; 
} END

#endif
