#include "NativeUtils.hh"

static word eventStream = 0;
static word weakDict = 0;
static word signalMap = 0;
static word signalMap2 = 0;

word push_front(word list, word value) {
  TagVal *cons = TagVal::New(0,2);
  cons->Init(0,value);
  cons->Init(1,list);
  return cons->ToWord();
}

///////////////////////////////////////////////////////////////////////

DEFINE0(NativeGtkCore_null) {
  word w = PointerToObject(NULL,TYPE_UNKNOWN);
  RETURN(w);
} END

DEFINE0(NativeGtkCore_gtkTrue) {
  RETURN(Store::IntToWord(TRUE));
} END

DEFINE0(NativeGtkCore_gtkFalse) {
  RETURN(Store::IntToWord(FALSE));
} END

inline void print_type(char *s, void *obj) {
  GObject *p = reinterpret_cast<GObject*>(obj);
  GTypeQuery q;
  memset(&q, 0, sizeof(q));
  g_type_query(G_OBJECT_TYPE(p), &q);
  g_message("%s: %p (type %s)", s, p, q.type_name);
}

inline void refObject(void *p, int type) {
  switch (type) {
  case TYPE_GTK_OBJECT: 
    g_object_ref(G_OBJECT(p));
    gtk_object_sink(GTK_OBJECT(p));
    print_type("gtk-object-reffed", p);
    break;
  case TYPE_G_OBJECT: 
    g_object_ref(p);
    print_type("gobject-reffed", p);
    break;
  }
}

DEFINE1(NativeGtkCore_refObject) {
  DECLARE_UNMANAGED_POINTER_TYPE(p,type,x0);
  g_message("reffing: Tuple %d = (Pointer: %p, Type: %d)", x0, p, type);
  refObject(p,type);  
  RETURN(x0);
} END

inline void unrefObject(word o) {
  Tuple *t = Tuple::FromWord(o);
  void *obj = Store::WordToUnmanagedPointer(t->Sel(0));
  int type = Store::WordToInt(t->Sel(1));
  // g_message("unreffing: Tuple %d = (Pointer: %p, Type: %d)", o, obj, type);
  switch (type) {
  case TYPE_GTK_OBJECT: 
  case TYPE_G_OBJECT: 
    print_type("about to unref", obj);
    //g_message("refcnt: %d", G_OBJECT(obj)->ref_count);
    g_object_unref(G_OBJECT(obj));
    // print_type("object-unreffed", obj); // crashes if ref_count == 0
    break;
  case TYPE_OWN:
    delete (int*)obj;
    g_message("deleted: %p", obj);
    break;
  }
}

DEFINE1(NativeGtkCore_hasSignals) {
  DECLARE_UNMANAGED_POINTER_TYPE(obj,type,x0);
  RETURN(BOOL_TO_WORD(type == TYPE_GTK_OBJECT));
} END

DEFINE1(NativeGtkCore_printObject) {
  DECLARE_UNMANAGED_POINTER_TYPE(obj,type,x0);
  g_message("Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  RETURN_UNIT;
} END


inline word PointerToObjectRegister(void *p, int type) {
  word obj = PointerToObject(p,type);
  WeakMap *wd = WeakMap::FromWord(weakDict);
  word w = Store::UnmanagedPointerToWord(p);
  if (!wd->IsMember(w)) {
    refObject(p,type);
    wd->Put(w,obj);
  }
  return obj;
}



///////////////////////////////////////////////////////////////////////

static inline word GdkScrollDirectionToDatatype(GdkScrollDirection dir) {
  enum { SCROLL_DOWN, SCROLL_LEFT, SCROLL_RIGHT, SCROLL_UP };
  switch (dir) {
  case GDK_SCROLL_UP: return Store::IntToWord(SCROLL_UP);
  case GDK_SCROLL_DOWN: return Store::IntToWord(SCROLL_DOWN);
  case GDK_SCROLL_LEFT: return Store::IntToWord(SCROLL_LEFT);
  }
  return Store::IntToWord(SCROLL_RIGHT);
}

static inline word GdkCrossingModeToDatatype(GdkCrossingMode mode) {
  enum { CROSSING_GRAB, CROSSING_NORMAL, CROSSING_UNGRAB };
  switch (mode) {
  case GDK_CROSSING_NORMAL: return Store::IntToWord(CROSSING_NORMAL);
  case GDK_CROSSING_GRAB: return Store::IntToWord(CROSSING_GRAB);
  }
  return Store::IntToWord(CROSSING_UNGRAB);
}

static inline word GdkNotifyTypeToDatatype(GdkNotifyType type) {
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

static inline word GdkVisibilityStateToDatatype(GdkVisibilityState state) {
  enum {  VISIBILITY_FULLY_OBSCURED, VISIBILITY_PARTIAL, 
	  VISIBILITY_UNOBSCURED };
  switch (state) {
  case GDK_VISIBILITY_FULLY_OBSCURED: 
    return Store::IntToWord(VISIBILITY_FULLY_OBSCURED);
  case GDK_VISIBILITY_PARTIAL: return Store::IntToWord(VISIBILITY_PARTIAL);
  }
  return Store::IntToWord(VISIBILITY_UNOBSCURED);
}

static inline word ExposeEvent(GdkEvent* event, int label) {
  GdkEventExpose *ev = reinterpret_cast<GdkEventExpose*>(event);
  TagVal *t = TagVal::New(label, 8);
  t->Init(0, Store::IntToWord((ev->area).height));
  t->Init(1, Store::IntToWord((ev->area).width));
  t->Init(2, Store::IntToWord((ev->area).x));
  t->Init(3, Store::IntToWord((ev->area).y));
  t->Init(4, Store::IntToWord(ev->count));
  t->Init(5, PointerToObjectRegister(ev->region,TYPE_UNKNOWN));
  t->Init(6, BOOL_TO_WORD(ev->send_event));
  t->Init(7, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  return t->ToWord();
}

static inline word MotionEvent(GdkEvent* event, int label) {
  GdkEventMotion *ev = reinterpret_cast<GdkEventMotion*>(event);
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(1, Store::IntToWord(ev->is_hint));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  t->Init(6, Real::New(ev->x)->ToWord());
  t->Init(7, Real::New(ev->x_root)->ToWord());
  t->Init(8, Real::New(ev->y)->ToWord());
  t->Init(9, Real::New(ev->y_root)->ToWord());
  return t->ToWord();
}

static inline word ButtonEvent(GdkEvent* event, int label) {
  GdkEventButton *ev = reinterpret_cast<GdkEventButton*>(event);
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, Store::IntToWord(ev->button));
  t->Init(1, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  //  g_message("%d", ev->time);
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  t->Init(6, Real::New(ev->x)->ToWord());
  t->Init(7, Real::New(ev->x_root)->ToWord());
  t->Init(8, Real::New(ev->y)->ToWord());
  t->Init(9, Real::New(ev->y_root)->ToWord());
  return t->ToWord();
}

static inline word KeyEvent(GdkEvent* event, int label) {
  GdkEventKey *ev = reinterpret_cast<GdkEventKey*>(event);
  TagVal *t = TagVal::New(label, 9);
  t->Init(0, Store::IntToWord(ev->group));
  t->Init(1, Store::IntToWord(ev->hardware_keycode));
  t->Init(2, Store::IntToWord(ev->keyval));
  t->Init(3, Store::IntToWord(ev->length));
  t->Init(4, BOOL_TO_WORD(ev->send_event));
  t->Init(5, Store::IntToWord(ev->state));
  t->Init(6, String::New(ev->string)->ToWord());
  t->Init(7, Store::IntToWord(ev->time));
  t->Init(8, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  return t->ToWord();
}

static inline word CrossingEvent(GdkEvent* event, int label) {
  GdkEventCrossing *ev = reinterpret_cast<GdkEventCrossing*>(event);    
  TagVal *t = TagVal::New(label, 12);
  t->Init(0, GdkNotifyTypeToDatatype(ev->detail));
  t->Init(1, BOOL_TO_WORD(ev->focus));
  t->Init(2, GdkCrossingModeToDatatype(ev->mode));
  t->Init(3, BOOL_TO_WORD(ev->send_event));
  t->Init(4, Store::IntToWord(ev->state));
  t->Init(5, PointerToObjectRegister(ev->subwindow,TYPE_GTK_OBJECT));
  //  g_message("%d", ev->time);
  t->Init(6, Store::IntToWord(ev->time));
  t->Init(7, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  t->Init(8, Real::New(ev->x)->ToWord());
  t->Init(9, Real::New(ev->x_root)->ToWord());
  t->Init(10, Real::New(ev->y)->ToWord());
  t->Init(11, Real::New(ev->y_root)->ToWord());
  return t->ToWord();
}

static inline word FocusEvent(GdkEvent* event, int label) {
  GdkEventFocus *ev = reinterpret_cast<GdkEventFocus*>(event);
  TagVal *t = TagVal::New(label, 3);
  t->Init(0, BOOL_TO_WORD(ev->in));
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  t->Init(2, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  return t->ToWord();
}

static inline word ConfigureEvent(GdkEvent* event, int label) {
  GdkEventConfigure *ev = reinterpret_cast<GdkEventConfigure*>(event);
  TagVal *t = TagVal::New(label, 6);
  t->Init(0, Store::IntToWord(ev->height));
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  t->Init(2, Store::IntToWord(ev->width));
  t->Init(3, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  t->Init(4, Store::IntToWord(ev->x));
  t->Init(5, Store::IntToWord(ev->y));
  return t->ToWord();
}

static inline word VisibilityEvent(GdkEvent* event, int label) {
  GdkEventVisibility *ev = reinterpret_cast<GdkEventVisibility*>(event);
  TagVal *t = TagVal::New(label, 3);
  t->Init(0, BOOL_TO_WORD(ev->send_event));
  t->Init(1, GdkVisibilityStateToDatatype(ev->state));
  t->Init(2, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  return t->ToWord();
}

static inline word NoExposeEvent(GdkEvent* event, int label) {
  GdkEventNoExpose *ev = reinterpret_cast<GdkEventNoExpose*>(event);
  TagVal *t = TagVal::New(label, 2);
  t->Init(0, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));  
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  return t->ToWord();
}

static inline word ScrollEvent(GdkEvent* event, int label) {
  GdkEventScroll *ev = reinterpret_cast<GdkEventScroll*>(event);    
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(1, GdkScrollDirectionToDatatype(ev->direction));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_GTK_OBJECT));
  t->Init(6, Real::New(ev->x)->ToWord());
  t->Init(7, Real::New(ev->x_root)->ToWord());
  t->Init(8, Real::New(ev->y)->ToWord());
  t->Init(9, Real::New(ev->y_root)->ToWord());
  return t->ToWord();
}

static inline word SimpleEvent(int label) {
  TagVal *t = TagVal::New(label, 0);
  return t->ToWord();
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
	 NOTHING,
	 NO_EXPOSE,
	 PROPERTY_NOTIFY,
	 PROXIMITY_IN, PROXIMITY_OUT,
	 SCROLL, 
	 SELECTION_CLEAR, SELECTION_NOTIFY, SELECTION_REQUEST, 
	 SETTING, 
	 UNMAP,
	 UNSUPPORTED, 
	 VISIBILITY_NOTIFY, 
	 WINDOW_STATE };

  switch (event->type) {
  case GDK_NOTHING: return SimpleEvent(NOTHING);
  case GDK_DELETE: return SimpleEvent(DELETE);
  case GDK_DESTROY: return SimpleEvent(DESTROY);
  case GDK_EXPOSE: return ExposeEvent(event, EXPOSE);
  case GDK_MOTION_NOTIFY: return MotionEvent(event, MOTION_NOTIFY);
  case GDK_BUTTON_PRESS: return ButtonEvent(event, BUTTON_PRESS);
  case GDK_2BUTTON_PRESS:return ButtonEvent(event, _2BUTTON_PRESS);
  case GDK_3BUTTON_PRESS:return ButtonEvent(event, _3BUTTON_PRESS);
  case GDK_BUTTON_RELEASE: return ButtonEvent(event, BUTTON_RELEASE);
  case GDK_KEY_PRESS: return KeyEvent(event, KEY_PRESS);
  case GDK_KEY_RELEASE: return KeyEvent(event, KEY_RELEASE);
  case GDK_ENTER_NOTIFY: return CrossingEvent(event, ENTER_NOTIFY);
  case GDK_LEAVE_NOTIFY: return CrossingEvent(event, LEAVE_NOTIFY);
  case GDK_FOCUS_CHANGE: return FocusEvent(event, FOCUS_CHANGE);
  case GDK_CONFIGURE: return ConfigureEvent(event, CONFIGURE);
  case GDK_MAP: return SimpleEvent(MAP);
  case GDK_UNMAP: return SimpleEvent(UNMAP);
  case GDK_PROPERTY_NOTIFY: return SimpleEvent(PROPERTY_NOTIFY);
  case GDK_SELECTION_CLEAR: return SimpleEvent(SELECTION_CLEAR);
  case GDK_SELECTION_REQUEST: return SimpleEvent(SELECTION_REQUEST);
  case GDK_SELECTION_NOTIFY: return SimpleEvent(SELECTION_NOTIFY);
  case GDK_PROXIMITY_IN: return SimpleEvent(PROXIMITY_IN);
  case GDK_PROXIMITY_OUT: return SimpleEvent(PROXIMITY_OUT);
  case GDK_DRAG_ENTER: return SimpleEvent(DRAG_ENTER);
  case GDK_DRAG_LEAVE: return SimpleEvent(DRAG_LEAVE);
  case GDK_DRAG_MOTION: return SimpleEvent(DRAG_MOTION);
  case GDK_DRAG_STATUS: return SimpleEvent(DRAG_STATUS);
  case GDK_DROP_START: return SimpleEvent(DROP_START);
  case GDK_DROP_FINISHED: return SimpleEvent(DROP_FINISHED);
  case GDK_CLIENT_EVENT: return SimpleEvent(CLIENT_EVENT);
  case GDK_VISIBILITY_NOTIFY: 
    return VisibilityEvent(event, VISIBILITY_NOTIFY);
  case GDK_NO_EXPOSE: return NoExposeEvent(event, NO_EXPOSE);
  case GDK_SCROLL: return ScrollEvent(event, SCROLL);
  case GDK_WINDOW_STATE: return SimpleEvent(WINDOW_STATE);
  case GDK_SETTING: return SimpleEvent(SETTING);
  default:
    TagVal *tv = TagVal::New(UNSUPPORTED, 1);
    tv->Init(0, PointerToObjectRegister(event,TYPE_UNKNOWN));
    return tv->ToWord();  
  }
}

inline void put_on_stream(word *stream, word value) {
  Future *f = static_cast<Future*>(Store::WordToTransient(*stream));
  *stream = (Future::New())->ToWord();  
  f->ScheduleWaitingThreads();
  f->Become(REF_LABEL, push_front(*stream, value));
}

inline word create_param(int tag, word value) {
  TagVal *param = TagVal::New(tag,1);
  param->Init(0, value);
  return param->ToWord();
}

word create_object(GType t, gpointer p) {
  static const GType G_LIST_TYPE = g_type_from_name("GList");
  static const GType G_SLIST_TYPE = g_type_from_name("GSList");
  static const GType GDK_EVENT_TYPE = gdk_event_get_type();
  static const GType GTK_OBJECT_TYPE = g_type_from_name("GtkObject");

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
	value = GdkEventToDatatype(gdk_event_copy(static_cast<GdkEvent*>(p)));
      }
      else
	if (g_type_is_a(t, GTK_OBJECT_TYPE))
	  value = PointerToObjectRegister(p, TYPE_GTK_OBJECT);
        else
	  value = PointerToObjectRegister(p, (G_IS_OBJECT(p) ? TYPE_G_OBJECT 
					                     : TYPE_UNKNOWN));
  return create_param(tag, value);
}

void sendArgsToStream(gint connid, guint n_param_values, 
		      const GValue *param_values) {

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
    if (!widget) paramlist = push_front(paramlist,value);
  }
  
  Tuple *tup = Tuple::New(3);
  tup->Init(0,Store::IntToWord(connid));
  tup->Init(1,PointerToObjectRegister(widget,TYPE_GTK_OBJECT));
  tup->Init(2,paramlist);
  
  put_on_stream(&eventStream, tup->ToWord());

  //  g_message("event has been put on stream");
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

DEFINE3(NativeGtkCore_signalConnect) {
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

DEFINE2(NativeGtkCore_signalDisconnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_INT(handler_id,x1);
  g_signal_handler_disconnect(G_OBJECT(obj), static_cast<gulong>(handler_id));
  RETURN_UNIT;
} END

DEFINE0(NativeGtkCore_getEventStream) {
  if (!eventStream) {
    eventStream = (Future::New())->ToWord();
    RootSet::Add(eventStream);
  }
  RETURN(eventStream);
} END

//////////////////////////////////////////////////////////////////////

class MyFinalization: public Finalization {
public:
  void Finalize(word value) {
    void *p = Store::WordToUnmanagedPointer((Tuple::FromWord(value))->Sel(0));
    //g_message("finalizing %p (%d)", p, value);
    unrefObject(value);
    g_message("finalized %p (%d)", p, value);
  }
};

DEFINE1(NativeGtkCore_weakMapAdd) {
  // x0 = (pointer, type)
  DECLARE_UNMANAGED_POINTER_TYPE(obj,type,x0);
  //g_message("adding Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  WeakMap::FromWord(weakDict)->Put(Tuple::FromWord(x0)->Sel(0),x0);  
  g_message("added Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_weakMapIsMember) {
  // x0 = (pointer, type)
  DECLARE_UNMANAGED_POINTER_TYPE(obj,type,x0);
  //g_message("is member? Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  RETURN(BOOL_TO_WORD
	 (WeakMap::FromWord(weakDict)->IsMember(Tuple::FromWord(x0)->Sel(0))));
} END

DEFINE2(NativeGtkCore_weakMapCondGet) {
  // x0 = (pointer, type), x1 = alternative  
  WeakMap::FromWord(weakDict)->CondGet(Tuple::FromWord(x0)->Sel(0),x1);
  g_message("condget");
  RETURN(WeakMap::FromWord(weakDict)->CondGet(Tuple::FromWord(x0)->Sel(0),x1));
} END

////////////////////////////////////////////////////////////////////////

DEFINE3(NativeGtkCore_signalMapAdd) {
  // x0 = connid to add, x1 = callback-fn, x2 = object
  Map::FromWord(signalMap)->Put(x0,x1);

  DECLARE_UNMANAGED_POINTER(p,x2);
  word key = Store::UnmanagedPointerToWord(p);
  Map* sm2 = Map::FromWord(signalMap2);
  word ids = sm2->CondGet(key, Store::IntToWord(Types::nil));
  sm2->Put(key, push_front(ids,x0));
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_signalMapRemove) {
  // x0 = connid to remove 
  Map::FromWord(signalMap)->Remove(x0);
  RETURN_UNIT;
} END

DEFINE2(NativeGtkCore_signalMapCondGet) {
  // x0 = connid to get, x1 = alternative
  RETURN(Map::FromWord(signalMap)->CondGet(x0,x1));
} END

DEFINE1(NativeGtkCore_signalMapGetConnIds) {
  // x0 = object
  DECLARE_UNMANAGED_POINTER(p,x0);
  word key = Store::UnmanagedPointerToWord(p);
  Map* sm2 = Map::FromWord(signalMap2);
  word ids = sm2->CondGet(key, Store::IntToWord(Types::nil));
  sm2->Remove(key);
  RETURN(ids);
} END

//////////////////////////////////////////////////////////////////////

DEFINE0(NativeGtkCore_isLoaded) {
  RETURN(BOOL_TO_WORD(signalMap != 0));
} END

DEFINE0(NativeGtkCore_init) {
  if (!signalMap) {
    signalMap = Map::New(256)->ToWord();
    RootSet::Add(signalMap);
  }
  if (!signalMap2) {
    signalMap2 = Map::New(256)->ToWord();
    RootSet::Add(signalMap2);
  }
  if (!weakDict) {
    weakDict = WeakMap::New(256, new MyFinalization())->ToWord();
    RootSet::Add(weakDict);
  }
  gtk_init(NULL,NULL);
  RETURN_UNIT;
} END

DEFINE0(NativeGtkCore_eventsPending) {
  RETURN(BOOL_TO_WORD(gtk_events_pending()));
} END

DEFINE0(NativeGtkCore_mainIteration) {
  gtk_main_iteration();
  RETURN_UNIT;
} END

////////////////////////////////////////////////////////////////////////

word InitComponent() {
  Record *record = CreateRecord(20);
  INIT_STRUCTURE(record, "NativeGtkCore", "isLoaded",
		 NativeGtkCore_isLoaded, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "init", 
		 NativeGtkCore_init, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "eventsPending", 
		 NativeGtkCore_eventsPending, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "mainIteration", 
		 NativeGtkCore_mainIteration, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "null", 
		 NativeGtkCore_null, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "gtkTrue", 
		 NativeGtkCore_gtkTrue, 0);
  INIT_STRUCTURE(record, "NativeGtkCore", "gtkFalse", 
		 NativeGtkCore_gtkFalse, 0);

  INIT_STRUCTURE(record, "NativeGtkCore", "refObject", 
		 NativeGtkCore_refObject, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "hasSignals", 
		 NativeGtkCore_hasSignals, 1);

  INIT_STRUCTURE(record, "NativeGtkCore", "printObject", 
		 NativeGtkCore_printObject, 1);

  INIT_STRUCTURE(record, "NativeGtkCore", "signalConnect", 
		 NativeGtkCore_signalConnect, 3);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalDisconnect", 
		 NativeGtkCore_signalDisconnect, 3);
  INIT_STRUCTURE(record, "NativeGtkCore", "getEventStream", 
		 NativeGtkCore_getEventStream, 0);

  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapAdd", 
		 NativeGtkCore_weakMapAdd, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapIsMember", 
		 NativeGtkCore_weakMapIsMember, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapCondGet", 
		 NativeGtkCore_weakMapCondGet, 2);

  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapAdd",
		 NativeGtkCore_signalMapAdd, 3);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapRemove",
		 NativeGtkCore_signalMapRemove, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapCondGet",
		 NativeGtkCore_signalMapCondGet, 2);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapGetConnIds",
		 NativeGtkCore_signalMapGetConnIds, 1);

  RETURN_STRUCTURE("NativeGtkCore$", record);
}
