#include "NativeUtils.hh"

static word eventStream = 0;
static word finalStream = 0;
static word finalStreamRef = 0;
static word weakDict = 0;
static word signalMap = 0;

word push_front(word list, word value) {
  TagVal *cons = TagVal::New(0,2);
  cons->Init(0,value);
  cons->Init(1,list);
  return cons->ToWord();
}

void put_on_stream(word *stream, word value) {
  Future *f = static_cast<Future*>(Store::WordToTransient(*stream));
  *stream = (Future::New())->ToWord();  
  f->ScheduleWaitingThreads();
  f->Become(REF_LABEL, push_front(*stream, value));
}

///////////////////////////////////////////////////////////////////////

DEFINE0(NativeGtkCore_isLoaded) {
  RETURN(BOOL_TO_WORD(signalMap != 0));
} END

DEFINE0(NativeGtkCore_init) {
  if (!signalMap) {
    signalMap = Map::New(256)->ToWord();
    RootSet::Add(signalMap);
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

DEFINE0(NativeGtkCore_null) {
  RETURN(Store::UnmanagedPointerToWord(NULL));
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

DEFINE1(NativeGtkCore_widgetRef) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  print_type("widget-reffed", obj);
  RETURN(Store::UnmanagedPointerToWord(
		gtk_widget_ref(reinterpret_cast<GtkWidget*>(obj))));
} END

DEFINE1(NativeGtkCore_widgetUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  print_type("widget-unreffed", obj);
  gtk_widget_unref(reinterpret_cast<GtkWidget*>(obj));
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_objectRef) {
  DECLARE_UNMANAGED_POINTER(obj,x0);  
  print_type("object-reffed", obj);
  RETURN(Store::UnmanagedPointerToWord(
         gtk_object_ref(reinterpret_cast<GtkObject*>(obj))));
} END

DEFINE1(NativeGtkCore_objectUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  print_type("object-unreffed", obj);
  gtk_object_unref(reinterpret_cast<GtkObject*>(obj));
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_gObjectRef) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  print_type("gobject-reffed", obj);
  RETURN(Store::UnmanagedPointerToWord(g_object_ref(obj)));
} END

DEFINE1(NativeGtkCore_gObjectUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  print_type("gobject-unreffed", obj);
  g_object_unref(obj);
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_deleteUnref) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  g_message("delete-unreffed: %p", obj);
  delete (int*)obj;
  RETURN_UNIT; 
} END

DEFINE1(NativeGtkCore_printObject) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  g_message("Pointer: %p, Word: %d", obj, x0);
  RETURN_UNIT;
} END

////////////////////////////////////////////////////////////////////////

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
  t->Init(5, Store::UnmanagedPointerToWord(ev->region));
  t->Init(6, BOOL_TO_WORD(ev->send_event));
  t->Init(7, Store::UnmanagedPointerToWord(ev->window));
  return t->ToWord();
}

static inline word MotionEvent(GdkEvent* event, int label) {
  GdkEventMotion *ev = reinterpret_cast<GdkEventMotion*>(event);
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, Store::UnmanagedPointerToWord(ev->device));
  t->Init(1, Store::IntToWord(ev->is_hint));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, Store::UnmanagedPointerToWord(ev->window));
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
  t->Init(1, Store::UnmanagedPointerToWord(ev->device));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  //  g_message("%d", ev->time);
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, Store::UnmanagedPointerToWord(ev->window));
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
  t->Init(8, Store::UnmanagedPointerToWord(ev->window));
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
  t->Init(5, Store::UnmanagedPointerToWord(ev->subwindow));
  //  g_message("%d", ev->time);
  t->Init(6, Store::IntToWord(ev->time));
  t->Init(7, Store::UnmanagedPointerToWord(ev->window));
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
  t->Init(2, Store::UnmanagedPointerToWord(ev->window));
  return t->ToWord();
}

static inline word ConfigureEvent(GdkEvent* event, int label) {
  GdkEventConfigure *ev = reinterpret_cast<GdkEventConfigure*>(event);
  TagVal *t = TagVal::New(label, 6);
  t->Init(0, Store::IntToWord(ev->height));
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  t->Init(2, Store::IntToWord(ev->width));
  t->Init(3, Store::UnmanagedPointerToWord(ev->window));
  t->Init(4, Store::IntToWord(ev->x));
  t->Init(5, Store::IntToWord(ev->y));
  return t->ToWord();
}

static inline word VisibilityEvent(GdkEvent* event, int label) {
  GdkEventVisibility *ev = reinterpret_cast<GdkEventVisibility*>(event);
  TagVal *t = TagVal::New(label, 3);
  t->Init(0, BOOL_TO_WORD(ev->send_event));
  t->Init(1, GdkVisibilityStateToDatatype(ev->state));
  t->Init(2, Store::UnmanagedPointerToWord(ev->window));
  return t->ToWord();
}

static inline word NoExposeEvent(GdkEvent* event, int label) {
  GdkEventNoExpose *ev = reinterpret_cast<GdkEventNoExpose*>(event);
  TagVal *t = TagVal::New(label, 2);
  t->Init(0, Store::UnmanagedPointerToWord(ev->window));  
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  return t->ToWord();
}

static inline word ScrollEvent(GdkEvent* event, int label) {
  GdkEventScroll *ev = reinterpret_cast<GdkEventScroll*>(event);    
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, Store::UnmanagedPointerToWord(ev->device));
  t->Init(1, GdkScrollDirectionToDatatype(ev->direction));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, Store::IntToWord(ev->state));
  t->Init(4, Store::IntToWord(ev->time));
  t->Init(5, Store::UnmanagedPointerToWord(ev->window));
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
    tv->Init(0, Store::UnmanagedPointerToWord(event));
    return tv->ToWord();  
  }
}

word create_param(int tag, word value) {
  TagVal *param = TagVal::New(tag,1);
  param->Init(0, value);
  return param->ToWord();
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
	value = GdkEventToDatatype(gdk_event_copy(static_cast<GdkEvent*>(p)));
      }
      else
	value = Store::UnmanagedPointerToWord(p);      
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
  tup->Init(1,Store::UnmanagedPointerToWord(widget));
  tup->Init(2,paramlist);
  
  put_on_stream(&eventStream, tup->ToWord());

  //  g_message("event has been put put on stream");
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
  /*
  // Hack alert
  u_int AlreadyMoved(Block *p) {
    return (!((((u_int *) p)[0] & GEN_GC_MASK) >> GEN_GC_SHIFT));
  }
  Block *GetForwardPtr(Block *p) {
    return ((Block **) p)[0];
  }

  inline word ForwardWord(word p) {
    Block *sp = PointerOp::RemoveTag(p);
    // order is important because moving ptr overwrites gen assignment
    if (AlreadyMoved(sp)) {
      sp = GetForwardPtr(sp);
      p  = PointerOp::EncodeTag(sp, PointerOp::DecodeTag(p));
    }
    return p;
  }
  */

  void Finalize(word value) {
    void *p = Store::WordToUnmanagedPointer((Tuple::FromWord(value))->Sel(0));

    // g_message("finalizing %p", p);
    /*
    finalStreamRef = Tuple::FromWord(value)->Sel(2);
    Cell *c = Cell::FromWord(finalStreamRef);
    finalStream = c->Access();
    //finalStream = ForwardWord(finalStream);

    put_on_stream(&finalStream, value);

    c->Assign(finalStream);
    */
    //    g_message("stream: %d, ref: %d", finalStream, finalStreamRef);

    g_message("finalized %p (%d)", p, value);
  }
};

DEFINE2(NativeGtkCore_weakMapAdd) {
  // x0 = pointer/key, x1 = (pointer, unref-fun, ids ref)

  // g_message("adding %p", Store::WordToUnmanagedPointer(x0));
  //  Tuple* t = Tuple::FromWord(x1);
  //  t->Init(2, finalStreamRef);

  //  WeakMap::FromWord(weakDict)->Put(x0, t->ToWord());
  WeakMap::FromWord(weakDict)->Put(x0, x1);
  
  g_message("added %p (%d)", Store::WordToUnmanagedPointer(x0), x1);
	    //	                     Store::WordToUnmanagedPointer(t->Sel(0)));
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_weakMapIsMember) {
  // x0 = pointer/key
  RETURN(BOOL_TO_WORD(WeakMap::FromWord(weakDict)->IsMember(x0)));
} END

DEFINE2(NativeGtkCore_weakMapCondGet) {
  // x0 = pointer/key, x1 = alternative
  RETURN(WeakMap::FromWord(weakDict)->CondGet(x0,x1));
} END

DEFINE0(NativeGtkCore_getFinalStream) {
  if (!finalStream) {
    finalStream = (Future::New())->ToWord();
    RootSet::Add(finalStream);
    finalStreamRef = (Cell::New(finalStream))->ToWord();
    RootSet::Add(finalStreamRef);
    weakDict = WeakMap::New(256, new MyFinalization())->ToWord();
    RootSet::Add(weakDict);
  }
  RETURN(finalStream);
} END


////////////////////////////////////////////////////////////////////////

DEFINE2(NativeGtkCore_signalMapAdd) {
  Map::FromWord(signalMap)->Put(x0,x1);
  RETURN_UNIT;
} END

DEFINE1(NativeGtkCore_signalMapRemove) {
  Map::FromWord(signalMap)->Remove(x0);
  RETURN_UNIT;
} END

DEFINE2(NativeGtkCore_signalMapCondGet) {
  RETURN(Map::FromWord(signalMap)->CondGet(x0,x1));
} END

//////////////////////////////////////////////////////////////////////

word InitComponent() {
  Record *record = CreateRecord(25);
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

  INIT_STRUCTURE(record, "NativeGtkCore", "objectRef", 
		 NativeGtkCore_objectRef, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "objectUnref", 
		 NativeGtkCore_objectUnref, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "widgetRef", 
		 NativeGtkCore_widgetRef, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "widgetUnref", 
		 NativeGtkCore_widgetUnref, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "gObjectRef", 
		 NativeGtkCore_gObjectRef, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "gObjectUnref", 
		 NativeGtkCore_gObjectUnref, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "deleteUnref", 
		 NativeGtkCore_deleteUnref, 1);

  INIT_STRUCTURE(record, "NativeGtkCore", "printObject", 
		 NativeGtkCore_printObject, 1);

  INIT_STRUCTURE(record, "NativeGtkCore", "signalConnect", 
		 NativeGtkCore_signalConnect, 3);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalDisconnect", 
		 NativeGtkCore_signalDisconnect, 3);
  INIT_STRUCTURE(record, "NativeGtkCore", "getEventStream", 
		 NativeGtkCore_getEventStream, 0);

  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapAdd", 
		 NativeGtkCore_weakMapAdd, 2);
  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapIsMember", 
		 NativeGtkCore_weakMapIsMember, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "weakMapCondGet", 
		 NativeGtkCore_weakMapCondGet, 2);
  INIT_STRUCTURE(record, "NativeGtkCore", "getFinalStream", 
		 NativeGtkCore_getFinalStream, 0);

  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapAdd",
		 NativeGtkCore_signalMapAdd, 2);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapRemove",
		 NativeGtkCore_signalMapRemove, 1);
  INIT_STRUCTURE(record, "NativeGtkCore", "signalMapCondGet",
		 NativeGtkCore_signalMapCondGet, 2);

  RETURN_STRUCTURE("NativeGtkCore$", record);
}
