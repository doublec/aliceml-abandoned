/*
 * Author:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2001
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 */

#include <mozart.h>
#include <gtk/gtk.h>
#include <string.h>
#include <stdio.h>

#define GOZ_DECLARE_GTKOBJECT(i, val)  OZ_declareForeignType (i, val, GtkObject*)

#define GOZ_(name) name ## _

#define GOZ_DECLARE_VIRTUAL_STRING(i, val) \
  gchar *val; \
  OZ_declareTerm(i, GOZ_(val)) \
  OZ_isUnit (GOZ_(val)) ? val = NULL : val = strdup(OZ_virtualStringToC(GOZ_(val), NULL));

/*
 * Signal Handling/Marshalling from Alice <-> G(D|T)K
 */

static OZ_Term signal_port = 0;

OZ_BI_define (alice_initialize_signal_port, 1, 0) {
  OZ_declareTerm (0, port);
  if (signal_port == 0) {
    OZ_protect(&signal_port); /* prevent GC of port anchor */
  }
  signal_port = port;
  return OZ_ENTAILED;
} OZ_BI_end


/*
 * Process all events in the queue and tell the oz side whether there were events or not
 */

OZ_BI_define (alice_handle_pending_events, 0, 1) {
  int had_events = 0;

  while (gtk_events_pending()) {
    had_events = 1;
    gtk_main_iteration();
  }
  OZ_out(0) = (had_events ? OZ_true() : OZ_false());
  return OZ_ENTAILED;
} OZ_BI_end

/*
 * Gdk Event Transformation
 */

static OZ_Term createExposeEvent(char *type, GdkEventExpose *event) {
  GdkRectangle *rect = (GdkRectangle *) malloc(sizeof(GdkRectangle));
  
  memcpy(rect, &(event->area), sizeof(GdkRectangle));

  return OZ_mkTuple(OZ_atom(type), 4,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_makeForeignPointer(rect),
		    OZ_int(event->count));
}

static OZ_Term computeSource(GdkInputSource source) {
  switch (source) {
  case GDK_SOURCE_MOUSE:
    return OZ_atom("GDK_SOURCE_MOUSE");
  case GDK_SOURCE_PEN:
    return OZ_atom("GDK_SOURCE_PEN");
  case GDK_SOURCE_ERASER:
    return OZ_atom("GDK_SOURCE_ERASER");
  case GDK_SOURCE_CURSOR:
    return OZ_atom("GDK_SOURCE_CURSOR");
  default:
    return OZ_atom("GDK_SOURCE_ALICEDUMMY");
  }
}

static OZ_Term computeCrossing(GdkCrossingMode mode) {
  switch (mode) {
  case GDK_CROSSING_NORMAL:
    return OZ_atom("GDK_CROSSING_NORMAL");
  case GDK_CROSSING_GRAB:
    return OZ_atom("GDK_CROSSING_GRAB");
  case GDK_CROSSING_UNGRAB:
    return OZ_atom("GDK_CROSSING_UNGRAB");
  default:
    return OZ_atom("GDK_CROSSING_ALICEDUMMY");
  }
}

static OZ_Term computeNotify(GdkNotifyType type) {
  switch (type) {
  case GDK_NOTIFY_ANCESTOR:
    return OZ_atom("GDK_NOTIFY_ANCESTOR");
  case GDK_NOTIFY_VIRTUAL:
    return OZ_atom("GDK_NOTIFY_VIRTUAL");
  case GDK_NOTIFY_INFERIOR:
    return OZ_atom("GDK_NOTIFY_INFERIOR");
  case GDK_NOTIFY_NONLINEAR:
    return OZ_atom("GDK_NOTIFY_NONLINEAR");
  case GDK_NOTIFY_NONLINEAR_VIRTUAL:
    return OZ_atom("GDK_NOTIFY_NONLINEAR_VIRTUAL");
  case GDK_NOTIFY_UNKNOWN:
    return OZ_atom("GDK_NOTIFY_UNKNOWN");
  default:
    return OZ_atom("GDK_NOTIFY_ALICEDUMMY");
  }
}

static OZ_Term computeVisibility(GdkVisibilityState state) {
  switch (state) {
  case GDK_VISIBILITY_UNOBSCURED:
    return OZ_atom("GDK_VISIBILITY_UNOBSCURED");
  case GDK_VISIBILITY_PARTIAL:
    return OZ_atom("GDK_VISIBILITY_PARTIAL");
  case GDK_VISIBILITY_FULLY_OBSCURED:
    return OZ_atom("GDK_VISIBILITY_FULLY_OBSCURED");
  default:
    return OZ_atom("GDK_VISIBILITY_ALICEDUMMY");
  }
}

static OZ_Term createMotionEvent(char *type, GdkEventMotion *event) {
  return OZ_mkTuple(OZ_atom(type), 14,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_int(event->time),
		    OZ_float(event->x),
		    OZ_float(event->y),
		    OZ_float(event->pressure),
		    OZ_float(event->xtilt),
		    OZ_float(event->ytilt),
		    OZ_int(event->state),
		    OZ_int(event->is_hint),
		    computeSource(event->source),
		    OZ_int(event->deviceid),
		    OZ_float(event->x_root),
		    OZ_float(event->y_root));
}

static OZ_Term createKeyEvent(char *type, GdkEventKey *event) {
  return OZ_mkTuple(OZ_atom(type), 7,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_int(event->time),
		    OZ_int(event->state),
		    OZ_int(event->keyval),
		    OZ_int(event->length),
		    OZ_mkByteString(event->string, event->length));
}

static OZ_Term createCrossingEvent(char *type, GdkEventCrossing *event) {
  return OZ_mkTuple(OZ_atom(type), 12,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_makeForeignPointer(event->subwindow),
		    OZ_int(event->time),
		    OZ_float(event->x),
		    OZ_float(event->y),
		    OZ_float(event->x_root),
		    OZ_float(event->y_root),
		    computeCrossing(event->mode),
		    computeNotify(event->detail),
		    OZ_int((int) event->focus),
		    OZ_int((int) event->state));
}

static OZ_Term createFocusEvent(char *type, GdkEventFocus *event) {
  return OZ_mkTuple(OZ_atom(type), 3,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_int((int) event->in));
}

static OZ_Term createConfigureEvent(char *type, GdkEventConfigure *event) {
  return OZ_mkTuple(OZ_atom(type), 6,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_int(event->x),
		    OZ_int(event->y),
		    OZ_int(event->width),
		    OZ_int(event->height));
}

static OZ_Term createButtonEvent(char *type, GdkEventButton *event) {
  return OZ_mkTuple(OZ_atom(type), 14,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    OZ_int(event->time),
		    OZ_float(event->x),
		    OZ_float(event->y),
		    OZ_float(event->pressure),
		    OZ_float(event->xtilt),
		    OZ_float(event->ytilt),
		    OZ_int(event->state),
		    OZ_int(event->button),
		    computeSource(event->source),
		    OZ_int(event->deviceid),
		    OZ_float(event->x_root),
		    OZ_float(event->y_root));
}

static OZ_Term createVisibilityEvent(char *type, GdkEventVisibility *event) {
  return OZ_mkTuple(OZ_atom(type), 3,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event),
		    computeVisibility(event->state));
}

static OZ_Term createNoExposeEvent(char *type, GdkEventNoExpose *event) {
  return OZ_mkTuple(OZ_atom(type), 2,
		    OZ_makeForeignPointer(event->window),
		    OZ_int(event->send_event));
}

static OZ_Term createGdkEvent(GdkEvent *event) {
  switch (event->type) {
  case GDK_NOTHING:
    return OZ_atom("GDK_NOTHING");
  case GDK_DELETE:
    return OZ_atom("GDK_DELETE");
  case GDK_DESTROY:
    return OZ_atom("GDK_DESTROY");
  case GDK_EXPOSE:
    return createExposeEvent("GDK_EPOSE", (GdkEventExpose *) event); 
  case GDK_MOTION_NOTIFY:
    return createMotionEvent("GDK_MOTION_NOTIFY", (GdkEventMotion *) event);
  case GDK_BUTTON_PRESS:
    return createButtonEvent("GDK_BUTTON_PRESS", (GdkEventButton *) event);
  case GDK_2BUTTON_PRESS:
    return createButtonEvent("GDK_2BUTTON_PRESS", (GdkEventButton *) event);
  case GDK_3BUTTON_PRESS:
    return createButtonEvent("GDK_3BUTTON_PRESS", (GdkEventButton *) event);
  case GDK_BUTTON_RELEASE:
    return createButtonEvent("GDK_BUTTON_RELEASE", (GdkEventButton *) event);
  case GDK_KEY_PRESS:
    return createKeyEvent("GDK_KEY_PRESS", (GdkEventKey *) event);
  case GDK_KEY_RELEASE:
    return createKeyEvent("GDK_KEY_RELEASE", (GdkEventKey *) event);
    break;
  case GDK_ENTER_NOTIFY:
    return createCrossingEvent("GDK_ENTER_NOTIFY", (GdkEventCrossing *) event);
  case GDK_LEAVE_NOTIFY:
    return createCrossingEvent("GDK_LEAVE_NOTIFY", (GdkEventCrossing *) event);
  case GDK_FOCUS_CHANGE:
    return createFocusEvent("GDK_FOCUS_CHANGE", (GdkEventFocus *) event);
  case GDK_CONFIGURE:
    return createConfigureEvent("GDK_CONFIGURE", (GdkEventConfigure *) event);
  case GDK_MAP:
    return OZ_atom("GDK_MAP");
  case GDK_UNMAP:
    return OZ_atom("GDK_UNMAP");
  case GDK_PROPERTY_NOTIFY:
    return OZ_atom("GDK_PROPERTY_NOTIFY");
  case GDK_SELECTION_CLEAR:
    return OZ_atom("GDK_SELECTION_CLEAR");
  case GDK_SELECTION_REQUEST:
    return OZ_atom("GDK_SELECTION_REQUEST");
  case GDK_SELECTION_NOTIFY:
    return OZ_atom("GDK_SELECTION_NOTIFY");
  case GDK_PROXIMITY_IN:
    return OZ_atom("GDK_PROXIMITY_IN");
  case GDK_PROXIMITY_OUT:
    return OZ_atom("GDK_PROXIMITY_OUT");
  case GDK_DRAG_ENTER:
    return OZ_atom("GDK_DRAG_ENTER");
  case GDK_DRAG_LEAVE:
    return OZ_atom("GDK_DRAG_LEAVE");
  case GDK_DRAG_MOTION:
    return OZ_atom("GDK_DRAG_MOTION");
  case GDK_DRAG_STATUS:
    return OZ_atom("GDK_DRAG_STATUS");
  case GDK_DROP_START:
    return OZ_atom("GDK_DROP_START");
    break;
  case GDK_DROP_FINISHED:
    return OZ_atom("GDK_DROP_FINISHED");
  case GDK_CLIENT_EVENT:
    return OZ_atom("GDK_CLIENT_EVENT");
  case GDK_VISIBILITY_NOTIFY:
    return createVisibilityEvent("GDK_VISIBILITY_NOTIFY", (GdkEventVisibility *) event);
  case GDK_NO_EXPOSE:
    return createNoExposeEvent("GDK_NO_EXPOSE", (GdkEventNoExpose *) event);
  default:
    return OZ_atom("UNSUPPORTED");
  }
}

/*
 * User Data is transmitted using the GtkArg Array.
 * Usually it consists of zero or one Argument, the GdkEvent Pointer.
 * The event pointer will be transformed to a tuple.
 * Additional Data will be ignored and should not be used.
 */

static void signal_marshal(GtkObject *object, gpointer oz_id, guint n_args, GtkArg *args) {
  switch (n_args) {
  case 0: /* GtkWidget Event */
    OZ_send(signal_port, OZ_mkTuple(OZ_atom("event"), 2, OZ_int((guint) oz_id),
				    OZ_atom("UNSUPPORTED")));
    break;
  case 1: /* GdkEvent Type is stored as object */
    OZ_send(signal_port, OZ_mkTuple(OZ_atom("event"), 2, OZ_int((guint) oz_id),
				    createGdkEvent((GdkEvent *) GTK_VALUE_OBJECT(args[0]))));
    break;
  default:
    fprintf(stderr, "signal_marshal: unable to handle event. IGNORED.\n");
    break;
  }

  /* Assign Result Type; this is fake because it ALWAYS indicates non-handling.
   * This should be changed later on but will work fine (but slowly) for now.
   */
  GtkArg result      = args[n_args + 1];
  result.type        = GTK_TYPE_BOOL;
  result.d.bool_data = FALSE;
}

/*  static char *eventTypeToString(GdkEvent *event) { */
/*    switch (event->type) { */
/*    case GDK_NOTHING: */
/*      return "GDK_NOTHING"; */
/*    case GDK_DELETE: */
/*      return "GDK_DELETE"; */
/*    case GDK_DESTROY: */
/*      return "GDK_DESTROY"; */
/*    case GDK_EXPOSE: */
/*      return "GDK_EXPOSE"; */
/*    case GDK_MOTION_NOTIFY: */
/*      return "GDK_MOTION_NOTIFY"; */
/*    case GDK_BUTTON_PRESS: */
/*      return "GDK_BUTTON_PRESS"; */
/*    case GDK_2BUTTON_PRESS: */
/*      return "GDK_2BUTTON_PRESS"; */
/*    case GDK_3BUTTON_PRESS: */
/*      return "GDK_3BUTTON_PRESS"; */
/*    case GDK_BUTTON_RELEASE: */
/*      return "GDK_BUTTON_RELEASE"; */
/*    case GDK_KEY_PRESS: */
/*      return "GDK_KEY_PRESS"; */
/*    case GDK_KEY_RELEASE: */
/*      return "GDK_KEY_RELEASE"; */
/*    case GDK_ENTER_NOTIFY: */
/*      return "GDK_ENTER_NOTIFY"; */
/*    case GDK_LEAVE_NOTIFY: */
/*      return "GDK_LEAVE_NOTIFY"; */
/*    case GDK_FOCUS_CHANGE: */
/*      return "GDK_FOCUS_CHANGE"; */
/*    case GDK_CONFIGURE: */
/*      return "GDK_CONFIGURE"; */
/*    case GDK_MAP: */
/*      return "GDK_MAP"; */
/*    case GDK_UNMAP: */
/*      return "GDK_MAP"; */
/*    case GDK_PROPERTY_NOTIFY: */
/*      return "GDK_PROPERTY_NOTIFY"; */
/*    case GDK_SELECTION_CLEAR: */
/*      return "GDK_SELECTION_CLEAR"; */
/*    case GDK_SELECTION_REQUEST: */
/*      return "GDK_SELECTION_REQUEST"; */
/*    case GDK_SELECTION_NOTIFY: */
/*      return "GDK_SELECTION_NOTIFY"; */
/*    case GDK_PROXIMITY_IN: */
/*      return "GDK_PROXIMITY_IN"; */
/*    case GDK_PROXIMITY_OUT: */
/*      return "GDK_PROXIMITY_OUT"; */
/*    case GDK_DRAG_ENTER: */
/*      return "GDK_DRAG_ENTER"; */
/*    case GDK_DRAG_LEAVE: */
/*      return "GDK_DRAG_LEAVE"; */
/*    case GDK_DRAG_MOTION: */
/*      return "GDK_DRAG_MOTION"; */
/*    case GDK_DRAG_STATUS: */
/*      return "GDK_DRAG_STATUS"; */
/*    case GDK_DROP_START: */
/*      return "GDK_DROP_START"; */
/*    case GDK_DROP_FINISHED: */
/*      return "GDK_DROP_FINISHED"; */
/*    case GDK_CLIENT_EVENT: */
/*      return "GDK_CLIENT_EVENT"; */
/*    case GDK_VISIBILITY_NOTIFY: */
/*      return "GDK_VISIBILITY_NOTIFY"; */
/*    case GDK_NO_EXPOSE: */
/*      return "GDK_NO_EXPOSE"; */
/*    default: */
/*      return "other Event"; */
/*    } */
/*  } */

/*  static int gdk_event_handler(GtkWidget *widget, GdkEvent *event, gpointer data) { */
/*    GdkEvent *clone = (GdkEvent *) malloc(sizeof(GdkEvent)); */
/*    Thread *tt = oz_newThread(); */

/*    memcpy(clone, event, sizeof(GdkEvent)); */

/*    fprintf(stderr, "GDK_EVENT: %s\n", eventTypeToString(event)); */

/*    tt->pushCall((OZ_Term) data, RefsArray::make(OZ_makeForeignPointer(clone), OZ_newVariable())); */
/*    /*  fprintf(stderr, "processing gdk event type: %s.\n", eventTypeToString(event)); */
/*        OZ_send(signal_port, OZ_mkTuple(OZ_atom("gdk_event"), 2, */
/*        OZ_int((guint) data), */
/*        OZ_makeForeignPointer(event))); */
/*    */
/*    return FALSE; */
/*  } */

/*
 * 1. The callback function is always NULL. We use our marshaller instead.
 * 2. Signals are transmitted as atoms.
 * 3. the signal id of gtk_signal_connect_full is ignored.
 */

OZ_BI_define (alice_signal_connect, 3, 0) {
  GOZ_DECLARE_GTKOBJECT(0, object);
  OZ_declareTerm(1, name);
  OZ_declareInt(2, oz_id);
  gtk_signal_connect_full(GTK_OBJECT (object), (gchar *) OZ_virtualStringToC(name, NULL),
			  NULL, signal_marshal, (gpointer) oz_id, NULL, FALSE, FALSE);
  return OZ_ENTAILED;
} OZ_BI_end

/*  OZ_BI_define (alice_signal_connect, 3, 0) { */
/*    GOZ_DECLARE_GTKOBJECT(0, object); */
/*    OZ_declareTerm(1, name); */
/*    OZ_declareTerm(2, oz_proc); */
/*    OZ_protect(&oz_proc); */
/*    gtk_signal_connect(GTK_OBJECT (object), (gchar *) OZ_virtualStringToC(name, NULL), */
/*  		     GTK_SIGNAL_FUNC(gdk_event_handler), (gpointer) oz_proc); */
/*    return OZ_ENTAILED; */
/*  } OZ_BI_end */

/*
 * GDK Lowlevel Support Functions
 */

/* Color Allocation and Freeing */

OZ_BI_define (alice_allocate_gdk_color, 3, 1) {
  GdkColor *ret = (GdkColor *) malloc(sizeof(GdkColor));
  
  OZ_declareInt(0, red);
  OZ_declareInt(1, blue);
  OZ_declareInt(2, green);
  
  ret->red   = red;
  ret->blue  = blue;
  ret->green = green;
  
  OZ_out(0) = OZ_makeForeignPointer(ret);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_free_gdk_color, 1, 0) {
  GOZ_DECLARE_GTKOBJECT(0, object);
  free(GTK_OBJECT(object));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_free_gdk_rectangle, 1, 0) {
  OZ_declareForeignType(0, rect, GdkRectangle*);
  if (rect != NULL) {
    free(rect);
  }
  return OZ_ENTAILED;
} OZ_BI_end


/*
 * Other Support Functions (necessary due to some hacks within Gtk)
 */

OZ_BI_define (alice_null, 0, 1) {
  OZ_out(0) = OZ_makeForeignPointer(NULL);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_g_list_append, 2, 1) {
  OZ_declareForeignType(0, anchor, GList*);
  GOZ_DECLARE_VIRTUAL_STRING(1, value);
  OZ_out(0) = OZ_makeForeignPointer(g_list_append(anchor, value));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_combo_get_entry, 1, 1) {
  OZ_declareForeignType(0, combo, GtkCombo*);
  OZ_out(0) = OZ_makeForeignPointer(combo->entry);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_combo_get_list, 1, 1) {
  OZ_declareForeignType(0, combo, GtkCombo*);
  OZ_out(0) = OZ_makeForeignPointer(combo->list);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_file_selection_get_ok_button, 1, 1) {
  OZ_declareForeignType(0, selection, GtkFileSelection*);
  OZ_out(0) = OZ_makeForeignPointer(selection->ok_button);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_file_selection_get_cancel_button, 1, 1) {
  OZ_declareForeignType(0, selection, GtkFileSelection*);
  OZ_out(0) = OZ_makeForeignPointer(selection->cancel_button);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_color_selection_set_color, 5, 0) {
  OZ_declareForeignType(0, selection, GtkColorSelection*);
  OZ_declareFloat(1, r);
  OZ_declareFloat(1, g);
  OZ_declareFloat(1, b);
  OZ_declareFloat(1, o);
  double color[4];
  color[0] = r; color[1] = g; color[2] = b; color[3] = o;
  gtk_color_selection_set_color(selection, color);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_color_selection_get_color, 1, 1) {
  double color[4];
  OZ_declareForeignType(0, selection, GtkColorSelection*);
  gtk_color_selection_get_color(selection, color);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 4,
			 OZ_float(color[0]),
			 OZ_float(color[1]),
			 OZ_float(color[2]),
			 OZ_float(color[3]));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_widget_size_request, 3, 0) {
  GtkRequisition req;
  OZ_declareForeignType(0, widget, GtkWidget*);
  OZ_declareInt(1, width);
  OZ_declareInt(2, height);
  req.width  = width;
  req.height = height;
  gtk_widget_size_request(widget, &req);
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_widget_get_child_requisition, 1, 1) {
  GtkRequisition req;
  OZ_declareForeignType(0, widget, GtkWidget*);
  gtk_widget_get_child_requisition(widget, &req);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(req.width), OZ_int(req.height));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_widget_get_pointer, 1, 1) {
  OZ_declareForeignType(0, widget, GtkWidget*);
  int x, y;
  gtk_widget_get_pointer(widget, &x, &y);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(x), OZ_int(y));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_button_box_get_child_size_default, 0, 1) {
  int x, y;
  gtk_button_box_get_child_size_default(&x, &y);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(x), OZ_int(y));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_button_box_get_child_ipadding_default, 0, 1) {
  int x, y;
  gtk_button_box_get_child_ipadding_default(&x, &y);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(x), OZ_int(y));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_button_box_get_child_size, 1, 1) {
  int x, y;
  OZ_declareForeignType(0, widget, GtkButtonBox*);
  gtk_button_box_get_child_size(widget, &x, &y);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(x), OZ_int(y));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_button_box_get_child_ipadding, 1, 1) {
  int x, y;
  OZ_declareForeignType(0, widget, GtkButtonBox*);
  gtk_button_box_get_child_ipadding(widget, &x, &y);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_int(x), OZ_int(y));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_clist_get_text, 3, 1) {
  OZ_declareForeignType(0, clist, GtkCList*);
  OZ_declareInt(1, row);
  OZ_declareInt(2, column);
  char *text;
  int res = gtk_clist_get_text(clist, row, column, &text);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_mkByteString(text, strlen(text)), OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_clist_get_pixmap, 3, 1) {
  OZ_declareForeignType(0, clist, GtkCList*);
  OZ_declareInt(1, row);
  OZ_declareInt(2, column);
  GdkPixmap *pixmap;
  GdkBitmap *bitmap;
  int res = gtk_clist_get_pixmap(clist, row, column, &pixmap, &bitmap);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 3,
			 OZ_makeForeignPointer(pixmap),
			 OZ_makeForeignPointer(bitmap),
			 OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_clist_get_pixtext, 3, 1) {
  OZ_declareForeignType(0, clist, GtkCList*);
  OZ_declareInt(1, row);
  OZ_declareInt(2, column);
  char *text;
  guint8 spacing;
  GdkPixmap *pixmap;
  GdkBitmap *bitmap;
  int res = gtk_clist_get_pixtext(clist, row, column, &text, &spacing, &pixmap, &bitmap);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 5,
			 OZ_mkByteString(text, strlen(text)),
			 OZ_int((int) spacing),
			 OZ_makeForeignPointer(pixmap),
			 OZ_makeForeignPointer(bitmap),
			 OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end
  
OZ_BI_define (alice_clist_get_selection_info, 3, 1) {
  OZ_declareForeignType(0, clist, GtkCList*);
  OZ_declareInt(1, x);
  OZ_declareInt(2, y);
  int row, column;
  int res = gtk_clist_get_selection_info(clist, x, y, &row, &column);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 3, OZ_int(row), OZ_int(column), OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_ctree_node_get_text, 3, 1) {
  OZ_declareForeignType(0, ctree, GtkCTree*);
  OZ_declareForeignType(1, node, GtkCTreeNode*);
  OZ_declareInt(2, column);
  char *text;
  int res = gtk_ctree_node_get_text(ctree, node, column, &text);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_mkByteString(text, strlen(text)), OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_ctree_node_get_pixmap, 3, 1) {
  OZ_declareForeignType(0, ctree, GtkCTree*);
  OZ_declareForeignType(1, node, GtkCTreeNode*);
  OZ_declareInt(2, column);
  GdkPixmap *pixmap;
  GdkBitmap *bitmap;
  int res = gtk_ctree_node_get_pixmap(ctree, node, column, &pixmap, &bitmap);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 3,
			 OZ_makeForeignPointer(pixmap),
			 OZ_makeForeignPointer(bitmap),
			 OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_ctree_node_get_pixtext, 3, 1) {
  OZ_declareForeignType(0, ctree, GtkCTree*);
  OZ_declareForeignType(1, node, GtkCTreeNode*);
  OZ_declareInt(2, column);
  char *text;
  guint8 spacing;
  GdkPixmap *pixmap;
  GdkBitmap *bitmap;
  int res = gtk_ctree_node_get_pixtext(ctree, node, column, &text, &spacing, &pixmap, &bitmap);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 5,
			 OZ_mkByteString(text, strlen(text)),
			 OZ_int((int) spacing),
			 OZ_makeForeignPointer(pixmap),
			 OZ_makeForeignPointer(bitmap),
			 OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

static OZ_Term getBool(gboolean v) {
  return ((v == TRUE) ? OZ_true() : OZ_false());
}

OZ_BI_define (alice_ctree_get_node_info, 2, 1) {
  OZ_declareForeignType(0, ctree, GtkCTree*);
  OZ_declareForeignType(1, node, GtkCTreeNode*);
  char *text;
  guint8 spacing;
  GdkPixmap *pixmap_closed;
  GdkBitmap *bitmap_closed;
  GdkPixmap *pixmap_opened;
  GdkBitmap *bitmap_opened;
  gboolean is_leaf, expanded;
  int res = gtk_ctree_get_node_info(ctree, node, &text, &spacing,
				    &pixmap_closed, &bitmap_closed,
				    &pixmap_opened, &bitmap_opened,
				    &is_leaf, &expanded);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 9,
			 OZ_mkByteString(text, strlen(text)),
			 OZ_int((int) spacing),
			 OZ_makeForeignPointer(pixmap_closed),
			 OZ_makeForeignPointer(bitmap_closed),
			 OZ_makeForeignPointer(pixmap_opened),
			 OZ_makeForeignPointer(bitmap_opened),
			 getBool(is_leaf),
			 getBool(expanded),
			 OZ_int(res));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_gtk_image_get, 1, 1) {
  OZ_declareForeignType(0, image, GtkImage*);
  GdkImage *val;
  GdkBitmap *mask;
  gtk_image_get(image, &val, &mask);
  OZ_out(0) = OZ_mkTuple(OZ_atom("#"), 2, OZ_makeForeignPointer(val), OZ_makeForeignPointer(mask));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_alloc_args, 1, 1) {
  OZ_declareInt(0, size);
  OZ_out(0) = OZ_makeForeignPointer(malloc(sizeof(GtkArg) * size));
  return OZ_ENTAILED;
} OZ_BI_end

OZ_BI_define (alice_free_args, 1, 0) {
  OZ_declareForeignType(0, args, GtkArg*);
  free(args);
  return OZ_ENTAILED;
} OZ_BI_end

/*
 * Define Interface
 */

static OZ_C_proc_interface oz_interface[] = {
  {"initializeSignalPort", 1, 0, alice_initialize_signal_port},
  {"handlePendingEvents", 0, 1, alice_handle_pending_events},
  {"signalConnect", 3, 0, alice_signal_connect},
  {"allocateGdkColor", 3, 1, alice_allocate_gdk_color},
  {"freeGdkColor", 1, 0, alice_free_gdk_color},
  {"freeGdkRectangle", 1, 0, alice_free_gdk_rectangle},
  {"null", 0, 1, alice_null},
  {"gListAppend", 2, 1, alice_g_list_append},
  {"comboGetEntry", 1, 1, alice_combo_get_entry},
  {"comboGetList", 1, 1, alice_combo_get_list},
  {"fileSelectionGetOkButton", 1, 1, alice_file_selection_get_ok_button},
  {"fileSelectionGetCancelButton", 1, 1, alice_file_selection_get_cancel_button},
  {"colorSelectionSetColor", 2, 0, alice_color_selection_set_color},
  {"colorSelectionGetColor", 1, 1, alice_color_selection_get_color},
  {"widgetSizeRequest", 3, 0, alice_widget_size_request},
  {"widgetGetSizeRequisition", 1, 1, alice_widget_get_child_requisition},
  {"widgetGetPointer", 1, 1, alice_widget_get_pointer},
  {"buttonBoxGetChildSizeDefault", 0, 1, alice_button_box_get_child_size_default},
  {"buttonBoxGetChildIpaddingDefault", 0, 1, alice_button_box_get_child_ipadding_default},
  {"buttonBoxGetChildSize", 1, 1, alice_button_box_get_child_size},
  {"buttonBoxGetChildIpadding", 1, 1, alice_button_box_get_child_ipadding},
  {"clistGetText", 3, 1, alice_clist_get_text},
  {"clistGetPixmap", 3, 1, alice_clist_get_pixmap},
  {"clistGetPixtext", 3, 1, alice_clist_get_pixtext},
  {"clistGetSelectionInfo", 3, 1, alice_clist_get_selection_info},
  {"ctreeNodeGetText", 3, 1, alice_ctree_node_get_text},
  {"ctreeNodeGetPixmap", 3, 1, alice_ctree_node_get_pixmap},
  {"ctreeNodeGetPixtext", 3, 1, alice_ctree_node_get_pixtext},
  {"ctreeGetNodeInfo", 2, 1, alice_ctree_get_node_info},
  {"gtkImageGet", 1, 1, alice_gtk_image_get},
  {"allocArgs", 1, 1, alice_alloc_args},
  {"freeArgs", 1, 0, alice_free_args},
  {0, 0, 0, 0}
};

char oz_module_name[] = "GtkSignal";

OZ_C_proc_interface *oz_init_module() {
  gtk_init(0, 0);
  gdk_init(0, 0);
  return oz_interface;
}
