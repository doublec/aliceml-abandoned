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

/*
  The native core component. See Core.aml for the purpose of the core.
*/

#include "Alice.hh"
#include "MyNativeAuthoring.hh"
#include "NativeUtils.hh"

static word eventStream = 0;
static word weakDict = 0;
static word signalMap = 0;
static word signalMap2 = 0;

///////////////////////////////////////////////////////////////////////

// push a word to the front of a list and return the new list
word push_front(word list, word value) {
  TagVal *cons = TagVal::New(0,2);
  cons->Init(0,value);
  cons->Init(1,list);
  return cons->ToWord();
}

// convert a pointer to an object, and add it to the weak map if necessary
inline word PointerToObjectRegister(void *p, int type) {
  if (!p)
    return OBJECT_TO_WORD(p);
  WeakMap *wd = WeakMap::FromWord(weakDict);
  word w = Store::UnmanagedPointerToWord(p);
  if (wd->IsMember(w))
    return wd->Get(w);
  //g_message("ptor adding %p (type %d)", p, type);
  word obj = OBJECT_TO_WORD(p,type);
  wd->Put(w,obj);
  return obj;
}

///////////////////////////////////////////////////////////////////////
// GENERAL CONSTANTS

DEFINE0(NativeCore_null) {
  RETURN(OBJECT_TO_WORD(NULL));
} END

DEFINE0(NativeCore_gtkTrue) {
  RETURN(INT_TO_WORD(TRUE));
} END

DEFINE0(NativeCore_gtkFalse) {
  RETURN(INT_TO_WORD(FALSE));
} END

///////////////////////////////////////////////////////////////////////
// EVENT HANDLING FUNCTIONS

static inline word ExposeEvent(GdkEvent* event, int label) {
  GdkEventExpose *ev = reinterpret_cast<GdkEventExpose*>(event);
  TagVal *t = TagVal::New(label, 8);
  t->Init(0, INT_TO_WORD((ev->area).height));
  t->Init(1, INT_TO_WORD((ev->area).width));
  t->Init(2, INT_TO_WORD((ev->area).x));
  t->Init(3, INT_TO_WORD((ev->area).y));
  t->Init(4, INT_TO_WORD(ev->count));
  t->Init(5, PointerToObjectRegister(ev->region,TYPE_UNKNOWN));
  t->Init(6, BOOL_TO_WORD(ev->send_event));
  t->Init(7, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  return t->ToWord();
}

static inline word MotionEvent(GdkEvent* event, int label) {
  GdkEventMotion *ev = reinterpret_cast<GdkEventMotion*>(event);
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(1, INT_TO_WORD(ev->is_hint));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, INT_TO_WORD(ev->state));
  t->Init(4, INT_TO_WORD(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  t->Init(6, REAL_TO_WORD(ev->x));
  t->Init(7, REAL_TO_WORD(ev->x_root));
  t->Init(8, REAL_TO_WORD(ev->y));
  t->Init(9, REAL_TO_WORD(ev->y_root));
  return t->ToWord();
}

static inline word ButtonEvent(GdkEvent* event, int label) {
  GdkEventButton *ev = reinterpret_cast<GdkEventButton*>(event);
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, INT_TO_WORD(ev->button));
  t->Init(1, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, INT_TO_WORD(ev->state));
  t->Init(4, INT_TO_WORD(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  t->Init(6, REAL_TO_WORD(ev->x));
  t->Init(7, REAL_TO_WORD(ev->x_root));
  t->Init(8, REAL_TO_WORD(ev->y));
  t->Init(9, REAL_TO_WORD(ev->y_root));
  return t->ToWord();
}

static inline word KeyEvent(GdkEvent* event, int label) {
  GdkEventKey *ev = reinterpret_cast<GdkEventKey*>(event);
  TagVal *t = TagVal::New(label, 9);
  t->Init(0, INT_TO_WORD(ev->group));
  t->Init(1, INT_TO_WORD(ev->hardware_keycode));
  t->Init(2, INT_TO_WORD(ev->keyval));
  t->Init(3, INT_TO_WORD(ev->length));
  t->Init(4, BOOL_TO_WORD(ev->send_event));
  t->Init(5, INT_TO_WORD(ev->state));
  t->Init(6, STRING_TO_WORD(ev->string));
  t->Init(7, INT_TO_WORD(ev->time));
  t->Init(8, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  return t->ToWord();
}

static inline word CrossingEvent(GdkEvent* event, int label) {
  GdkEventCrossing *ev = reinterpret_cast<GdkEventCrossing*>(event);    
  TagVal *t = TagVal::New(label, 12);
  t->Init(0, INT_TO_WORD(ev->detail));
  t->Init(1, BOOL_TO_WORD(ev->focus));
  t->Init(2, INT_TO_WORD(ev->mode));
  t->Init(3, BOOL_TO_WORD(ev->send_event));
  t->Init(4, INT_TO_WORD(ev->state));
  t->Init(5, PointerToObjectRegister(ev->subwindow,TYPE_G_OBJECT));
  t->Init(6, INT_TO_WORD(ev->time));
  t->Init(7, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  t->Init(8, REAL_TO_WORD(ev->x));
  t->Init(9, REAL_TO_WORD(ev->x_root));
  t->Init(10, REAL_TO_WORD(ev->y));
  t->Init(11, REAL_TO_WORD(ev->y_root));
  return t->ToWord();
}

static inline word FocusEvent(GdkEvent* event, int label) {
  GdkEventFocus *ev = reinterpret_cast<GdkEventFocus*>(event);
  TagVal *t = TagVal::New(label, 3);
  t->Init(0, BOOL_TO_WORD(ev->in));
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  t->Init(2, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  return t->ToWord();
}

static inline word ConfigureEvent(GdkEvent* event, int label) {
  GdkEventConfigure *ev = reinterpret_cast<GdkEventConfigure*>(event);
  TagVal *t = TagVal::New(label, 6);
  t->Init(0, INT_TO_WORD(ev->height));
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  t->Init(2, INT_TO_WORD(ev->width));
  t->Init(3, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  t->Init(4, INT_TO_WORD(ev->x));
  t->Init(5, INT_TO_WORD(ev->y));
  return t->ToWord();
}

static inline word VisibilityEvent(GdkEvent* event, int label) {
  GdkEventVisibility *ev = reinterpret_cast<GdkEventVisibility*>(event);
  TagVal *t = TagVal::New(label, 3);
  t->Init(0, BOOL_TO_WORD(ev->send_event));
  t->Init(1, INT_TO_WORD(ev->state));
  t->Init(2, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  return t->ToWord();
}

static inline word NoExposeEvent(GdkEvent* event, int label) {
  GdkEventNoExpose *ev = reinterpret_cast<GdkEventNoExpose*>(event);
  TagVal *t = TagVal::New(label, 2);
  t->Init(0, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));  
  t->Init(1, BOOL_TO_WORD(ev->send_event));
  return t->ToWord();
}

static inline word ScrollEvent(GdkEvent* event, int label) {
  GdkEventScroll *ev = reinterpret_cast<GdkEventScroll*>(event);    
  TagVal *t = TagVal::New(label, 10);
  t->Init(0, PointerToObjectRegister(ev->device,TYPE_G_OBJECT));
  t->Init(1, INT_TO_WORD(ev->direction));
  t->Init(2, BOOL_TO_WORD(ev->send_event));
  t->Init(3, INT_TO_WORD(ev->state));
  t->Init(4, INT_TO_WORD(ev->time));
  t->Init(5, PointerToObjectRegister(ev->window,TYPE_G_OBJECT));
  t->Init(6, REAL_TO_WORD(ev->x));
  t->Init(7, REAL_TO_WORD(ev->x_root));
  t->Init(8, REAL_TO_WORD(ev->y));
  t->Init(9, REAL_TO_WORD(ev->y_root));
  return t->ToWord();
}

static inline word SimpleEvent(int label) {
  TagVal *t = TagVal::New(label, 0);
  return t->ToWord();
}

word GdkEventToDatatype(GdkEvent *event) {
  enum { EVENT_2BUTTON_PRESS, EVENT_3BUTTON_PRESS, 
	   EVENT_BUTTON_PRESS, EVENT_BUTTON_RELEASE, 
	 EVENT_CLIENT_EVENT,
	 EVENT_CONFIGURE,
	 EVENT_DELETE, EVENT_DESTROY,
	 EVENT_DRAG_ENTER, EVENT_DRAG_LEAVE, 
	   EVENT_DRAG_MOTION, EVENT_DRAG_STATUS,
	 EVENT_DROP_FINISHED, EVENT_DROP_START, 
	 EVENT_ENTER_NOTIFY, 
	 EVENT_EXPOSE,
	 EVENT_FOCUS_CHANGE, 
	 EVENT_KEY_PRESS, EVENT_KEY_RELEASE,
	 EVENT_LEAVE_NOTIFY, 
	 EVENT_MAP,
	 EVENT_MOTION_NOTIFY,
	 EVENT_NOTHING,
	 EVENT_NO_EXPOSE,
	 EVENT_PROPERTY_NOTIFY,
	 EVENT_PROXIMITY_IN, EVENT_PROXIMITY_OUT,
	 EVENT_SCROLL, 
	 EVENT_SELECTION_CLEAR, EVENT_SELECTION_NOTIFY,EVENT_SELECTION_REQUEST,
	 EVENT_SETTING, 
	 EVENT_UNMAP,
	 EVENT_UNSUPPORTED, 
	 EVENT_VISIBILITY_NOTIFY, 
	 EVENT_WINDOW_STATE };

  switch (event->type) {
  case GDK_NOTHING: return SimpleEvent(EVENT_NOTHING);
  case GDK_DELETE: return SimpleEvent(EVENT_DELETE);
  case GDK_DESTROY: return SimpleEvent(EVENT_DESTROY);
  case GDK_EXPOSE: return ExposeEvent(event, EVENT_EXPOSE);
  case GDK_MOTION_NOTIFY: return MotionEvent(event, EVENT_MOTION_NOTIFY);
  case GDK_BUTTON_PRESS: return ButtonEvent(event, EVENT_BUTTON_PRESS);
  case GDK_2BUTTON_PRESS:return ButtonEvent(event, EVENT_2BUTTON_PRESS);
  case GDK_3BUTTON_PRESS:return ButtonEvent(event, EVENT_3BUTTON_PRESS);
  case GDK_BUTTON_RELEASE: return ButtonEvent(event, EVENT_BUTTON_RELEASE);
  case GDK_KEY_PRESS: return KeyEvent(event, EVENT_KEY_PRESS);
  case GDK_KEY_RELEASE: return KeyEvent(event, EVENT_KEY_RELEASE);
  case GDK_ENTER_NOTIFY: return CrossingEvent(event, EVENT_ENTER_NOTIFY);
  case GDK_LEAVE_NOTIFY: return CrossingEvent(event, EVENT_LEAVE_NOTIFY);
  case GDK_FOCUS_CHANGE: return FocusEvent(event, EVENT_FOCUS_CHANGE);
  case GDK_CONFIGURE: return ConfigureEvent(event, EVENT_CONFIGURE);
  case GDK_MAP: return SimpleEvent(EVENT_MAP);
  case GDK_UNMAP: return SimpleEvent(EVENT_UNMAP);
  case GDK_PROPERTY_NOTIFY: return SimpleEvent(EVENT_PROPERTY_NOTIFY);
  case GDK_SELECTION_CLEAR: return SimpleEvent(EVENT_SELECTION_CLEAR);
  case GDK_SELECTION_REQUEST: return SimpleEvent(EVENT_SELECTION_REQUEST);
  case GDK_SELECTION_NOTIFY: return SimpleEvent(EVENT_SELECTION_NOTIFY);
  case GDK_PROXIMITY_IN: return SimpleEvent(EVENT_PROXIMITY_IN);
  case GDK_PROXIMITY_OUT: return SimpleEvent(EVENT_PROXIMITY_OUT);
  case GDK_DRAG_ENTER: return SimpleEvent(EVENT_DRAG_ENTER);
  case GDK_DRAG_LEAVE: return SimpleEvent(EVENT_DRAG_LEAVE);
  case GDK_DRAG_MOTION: return SimpleEvent(EVENT_DRAG_MOTION);
  case GDK_DRAG_STATUS: return SimpleEvent(EVENT_DRAG_STATUS);
  case GDK_DROP_START: return SimpleEvent(EVENT_DROP_START);
  case GDK_DROP_FINISHED: return SimpleEvent(EVENT_DROP_FINISHED);
  case GDK_CLIENT_EVENT: return SimpleEvent(EVENT_CLIENT_EVENT);
  case GDK_VISIBILITY_NOTIFY: 
    return VisibilityEvent(event, EVENT_VISIBILITY_NOTIFY);
  case GDK_NO_EXPOSE: return NoExposeEvent(event, EVENT_NO_EXPOSE);
  case GDK_SCROLL: return ScrollEvent(event, EVENT_SCROLL);
  case GDK_WINDOW_STATE: return SimpleEvent(EVENT_WINDOW_STATE);
  case GDK_SETTING: return SimpleEvent(EVENT_SETTING);
  default:
    TagVal *tv = TagVal::New(EVENT_UNSUPPORTED, 1);
    tv->Init(0, PointerToObjectRegister(event,TYPE_UNKNOWN));
    return tv->ToWord();  
  }
}

// put a word on a stream
inline void put_on_stream(word *stream, word value) {
  Future *f = static_cast<Future*>(Store::WordToTransient(*stream));
  *stream = (Future::New())->ToWord();  
  f->ScheduleWaitingThreads();
  f->Become(REF_LABEL, push_front(*stream, value));
}

// construct an arg value
inline word create_param(int tag, word value) {
  TagVal *param = TagVal::New(tag,1);
  param->Init(0, value);
  return param->ToWord();
}

// convert a pointer to an object with the correct type information
word create_object(GType t, gpointer p) {
  static const GType G_LIST_TYPE = g_type_from_name("GList");
  static const GType G_SLIST_TYPE = g_type_from_name("GSList");
  static const GType GDK_EVENT_TYPE = gdk_event_get_type();
  static const GType GTK_OBJECT_TYPE = g_type_from_name("GtkObject");

  int tag = gtkOBJECT;
  word value;
  if (g_type_is_a(t, G_LIST_TYPE)) {
    tag = gtkLIST;
    value = GLIST_OBJECT_TO_WORD(static_cast<GList*>(p));
  }
  else
    if (g_type_is_a(t, G_SLIST_TYPE)) {
      tag = gtkLIST;
      value = GSLIST_OBJECT_TO_WORD(static_cast<GSList*>(p));
    }
    else
      if (t == GDK_EVENT_TYPE) {
	tag = gtkEVENT;
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

// main function that puts arguments on event stream
void sendArgsToStream(gint connid, guint n_param_values, 
		      const GValue *param_values) {

  word paramlist = INT_TO_WORD(Types::nil);
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
      value = create_param(gtkINT, 
		   INT_TO_WORD(static_cast<int>(g_value_get_char(val))));
      break;
    case G_TYPE_UCHAR:  
      value = create_param(gtkINT, 
                 INT_TO_WORD(static_cast<int>(g_value_get_uchar(val))));
      break;
    case G_TYPE_BOOLEAN:
      value = create_param(gtkBOOL, BOOL_TO_WORD(g_value_get_boolean(val)));
      break;
    case G_TYPE_INT:    
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_int(val)));
      break;
    case G_TYPE_UINT:   
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_uint(val)));
      break;
    case G_TYPE_LONG:   
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_long(val)));
      break;
    case G_TYPE_ULONG:  
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_ulong(val)));
      break;
    case G_TYPE_INT64:  
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_int64(val)));
      break;
    case G_TYPE_UINT64: 
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_uint64(val)));
      break;
    case G_TYPE_ENUM:   
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_enum(val)));
      break;
    case G_TYPE_FLAGS:  
      value = create_param(gtkINT, INT_TO_WORD(g_value_get_flags(val)));
      break;
    case G_TYPE_FLOAT:  
      value = create_param(gtkREAL, REAL_TO_WORD(g_value_get_float(val)));
      break;
    case G_TYPE_DOUBLE: 
      value = create_param(gtkREAL, REAL_TO_WORD(g_value_get_double(val)));
      break;
    case G_TYPE_STRING: 
     value = create_param(gtkSTRING, STRING_TO_WORD(g_value_get_string(val)));
      break;
    default:
      if (i==0)
	widget = g_value_peek_pointer(val);
      else
	value = create_object(G_VALUE_TYPE(val), g_value_peek_pointer(val));
    }
    if (!widget) paramlist = push_front(paramlist,value);
  }
  
  Tuple *tup = Tuple::New(3);
  tup->Init(0,INT_TO_WORD(connid));
  tup->Init(1,PointerToObjectRegister(widget,TYPE_GTK_OBJECT));
  tup->Init(2,paramlist);
  
  put_on_stream(&eventStream, tup->ToWord());

  //  g_message("event has been put on stream");
}

// the generic_marshaller is attached to every GObject (instead of the
// the alice callback function, which cannot be invoked in C)
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

DEFINE3(NativeCore_signalConnect) {
  DECLARE_OBJECT(obj,x0);
  DECLARE_CSTRING(signalname,x1);
  DECLARE_BOOL(after,x2);

  gint isDelete = !strcmp(signalname, "delete_event");

  GClosure *closure = g_cclosure_new(G_CALLBACK(generic_marshaller),
  				     GINT_TO_POINTER(isDelete), NULL);
  gulong connid = g_signal_connect_closure(G_OBJECT(obj), signalname, 
					closure, after ? TRUE : FALSE); 
  g_closure_set_meta_marshal(closure,GINT_TO_POINTER(connid),
			     generic_marshaller);
  
  RETURN(INT_TO_WORD(static_cast<int>(connid)));
} END

DEFINE2(NativeCore_signalDisconnect) {
  DECLARE_OBJECT(obj,x0);
  DECLARE_INT(handler_id,x1);
  g_signal_handler_disconnect(G_OBJECT(obj), static_cast<gulong>(handler_id));
  RETURN_UNIT;
} END

DEFINE0(NativeCore_getEventStream) {
  if (!eventStream) {
    eventStream = (Future::New())->ToWord();
    RootSet::Add(eventStream);
  }
  RETURN(eventStream);
} END

////////////////////////////////////////////////////////////////////////
// SIGNAL MAP FUNCTIONS

DEFINE3(NativeCore_signalMapAdd) {
  // x0 = connid to add, x1 = callback-fn, x2 = object
  //g_message("adding signal #%d", Store::WordToInt(x0));
  Map::FromWord(signalMap)->Put(x0,x1);

  DECLARE_OBJECT(p,x2);
  word key = Store::UnmanagedPointerToWord(p);
  Map* sm2 = Map::FromWord(signalMap2);
  word ids = sm2->CondGet(key, INT_TO_WORD(Types::nil));
  sm2->Put(key, push_front(ids,x0));
  RETURN_UNIT;
} END

DEFINE1(NativeCore_signalMapRemove) {
  // x0 = connid to remove 
  //g_message("removing signal #%d", Store::WordToInt(x0));
  Map::FromWord(signalMap)->Remove(x0);
  RETURN_UNIT;
} END

DEFINE2(NativeCore_signalMapCondGet) {
  // x0 = connid to get, x1 = alternative
  RETURN(Map::FromWord(signalMap)->CondGet(x0,x1));
} END

DEFINE1(NativeCore_signalMapGetConnIds) {
  // x0 = object
  DECLARE_OBJECT(p,x0);
  word key = Store::UnmanagedPointerToWord(p);
  Map* sm2 = Map::FromWord(signalMap2);
  word ids = sm2->CondGet(key, INT_TO_WORD(Types::nil));
  sm2->Remove(key);
  RETURN(ids);
} END

//////////////////////////////////////////////////////////////////////
// WEAK MAP FUNCTIONS

class MyFinalization: public Finalization {
public:
  void Finalize(word value) {
    //g_message("finalizing %p (%d)", p, value);
    Tuple *t = Tuple::FromWord(value);
    void *p = Store::WordToUnmanagedPointer(t->Sel(0));
    int type = Store::WordToInt(t->Sel(1));
    __unrefObject(Store::WordToUnmanagedPointer(t->Sel(0)),
                  Store::WordToInt(t->Sel(1)));
    //g_message("finalized %p (type %d)", p, type);
  }
};

DEFINE1(NativeCore_weakMapAdd) {
  // x0 = (pointer, type)
  WeakMap::FromWord(weakDict)->Put(Tuple::FromWord(x0)->Sel(0),x0);  
  //DECLARE_OBJECT_WITH_TYPE(obj,type,x0);
  //g_message("added Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  RETURN_UNIT;
} END

DEFINE1(NativeCore_weakMapIsMember) {
  // x0 = (pointer, type)
  //DECLARE_OBJECT_WITH_TYPE(obj,type,x0);
  //g_message("is member? Tuple %d = (Pointer: %p, Type: %d)", x0, obj, type);
  RETURN(BOOL_TO_WORD
	 (WeakMap::FromWord(weakDict)->IsMember(Tuple::FromWord(x0)->Sel(0))));
} END

DEFINE2(NativeCore_weakMapCondGet) {
  // x0 = (pointer, type), x1 = alternative  
  RETURN(WeakMap::FromWord(weakDict)->CondGet(Tuple::FromWord(x0)->Sel(0),x1));
} END

///////////////////////////////////////////////////////////////////////
// OBJECT HANDLING

DEFINE1(NativeCore_unrefObject) {
  DECLARE_OBJECT_WITH_TYPE(p,type,x0);
  __unrefObject(p,type);  
  RETURN(x0);
} END

DEFINE1(NativeCore_hasSignals) {
  DECLARE_OBJECT_WITH_TYPE(obj,type,x0);
  RETURN(BOOL_TO_WORD(type == TYPE_GTK_OBJECT));
} END

//////////////////////////////////////////////////////////////////////
// MAIN LOOP FUNCTIONS

DEFINE0(NativeCore_isLoaded) {
  RETURN(BOOL_TO_WORD(signalMap != 0));
} END

DEFINE0(NativeCore_init) {
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

DEFINE0(NativeCore_eventsPending) {
  RETURN(BOOL_TO_WORD(gtk_events_pending()));
} END

DEFINE0(NativeCore_mainIteration) {
  gtk_main_iteration();
  RETURN_UNIT;
} END

///////////////////////////////////////////////////////////////////////
// DEBUG FUNCTIONS

DEFINE1(NativeCore_printObject) {
  DECLARE_OBJECT_WITH_TYPE(obj,type,x0);
  g_print("printObject: Tuple %d = (Pointer: %p, Type: %d)\n", x0, obj, type);
  RETURN_UNIT;
} END

DEFINE0(NativeCore_forceGC) {
  StatusWord::SetStatus(Store::GCStatus());
  RETURN_UNIT;
} END

////////////////////////////////////////////////////////////////////////

word InitComponent() {
  Record *record = Record::New(21);
  INIT_STRUCTURE(record, "NativeCore", "null", 
		 NativeCore_null, 0);
  INIT_STRUCTURE(record, "NativeCore", "gtkTrue", 
		 NativeCore_gtkTrue, 0);
  INIT_STRUCTURE(record, "NativeCore", "gtkFalse", 
		 NativeCore_gtkFalse, 0);

  INIT_STRUCTURE(record, "NativeCore", "signalConnect", 
		 NativeCore_signalConnect, 3);
  INIT_STRUCTURE(record, "NativeCore", "signalDisconnect", 
		 NativeCore_signalDisconnect, 3);
  INIT_STRUCTURE(record, "NativeCore", "getEventStream", 
		 NativeCore_getEventStream, 0);

  INIT_STRUCTURE(record, "NativeCore", "signalMapAdd",
		 NativeCore_signalMapAdd, 3);
  INIT_STRUCTURE(record, "NativeCore", "signalMapRemove",
		 NativeCore_signalMapRemove, 1);
  INIT_STRUCTURE(record, "NativeCore", "signalMapCondGet",
		 NativeCore_signalMapCondGet, 2);
  INIT_STRUCTURE(record, "NativeCore", "signalMapGetConnIds",
		 NativeCore_signalMapGetConnIds, 1);

  INIT_STRUCTURE(record, "NativeCore", "weakMapAdd", 
		 NativeCore_weakMapAdd, 1);
  INIT_STRUCTURE(record, "NativeCore", "weakMapIsMember", 
		 NativeCore_weakMapIsMember, 1);
  INIT_STRUCTURE(record, "NativeCore", "weakMapCondGet", 
		 NativeCore_weakMapCondGet, 2);

  INIT_STRUCTURE(record, "NativeCore", "unrefObject", 
		 NativeCore_unrefObject, 1);
  INIT_STRUCTURE(record, "NativeCore", "hasSignals", 
		 NativeCore_hasSignals, 1);

  INIT_STRUCTURE(record, "NativeCore", "isLoaded",
		 NativeCore_isLoaded, 0);
  INIT_STRUCTURE(record, "NativeCore", "init", 
		 NativeCore_init, 0);
  INIT_STRUCTURE(record, "NativeCore", "eventsPending", 
		 NativeCore_eventsPending, 0);
  INIT_STRUCTURE(record, "NativeCore", "mainIteration", 
		 NativeCore_mainIteration, 0);

  INIT_STRUCTURE(record, "NativeCore", "printObject", 
		 NativeCore_printObject, 1);
  INIT_STRUCTURE(record, "NativeCore", "forceGC",
		 NativeCore_forceGC, 0);

  RETURN_STRUCTURE("NativeCore$", record);
}
