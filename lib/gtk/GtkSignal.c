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
 * User Data is transmitted using the GtkArg Array. Usually it consists of one Argument,
 * the GdkEvent Pointer. Additional Data will be ignored and should not be used.
 */

static void signal_marshal(GtkObject *object, gpointer oz_id, guint n_args, GtkArg *args) {
  OZ_send(signal_port, OZ_mkTuple(OZ_atom("event"), 2, OZ_int((guint) oz_id),
				  OZ_makeForeignPointer(args->d.object_data)));
}

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

/* Event Transformation */

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

OZ_BI_define (alice_get_gdk_event, 1, 1) {
  OZ_declareForeignType(0, event, GdkEvent*);

  switch (event->type) {
  case GDK_NOTHING:
    OZ_out(0) = OZ_atom("GDK_NOTHING");
    break;
  case GDK_DELETE:
    OZ_out(0) = OZ_atom("GDK_DELETE");
    break;
  case GDK_DESTROY:
    OZ_out(0) = OZ_atom("GDK_DESTROY");
    break;
  case GDK_EXPOSE:
    OZ_out(0) = createExposeEvent("GDK_EPOSE", (GdkEventExpose *) event); 
    break;
  case GDK_MOTION_NOTIFY:
    OZ_out(0) = createMotionEvent("GDK_MOTION_NOTIFY", (GdkEventMotion *) event);
    break;
  case GDK_BUTTON_PRESS:
    OZ_out(0) = createButtonEvent("GDK_BUTTON_PRESS", (GdkEventButton *) event);
    break;
  case GDK_2BUTTON_PRESS:
    OZ_out(0) = createButtonEvent("GDK_2BUTTON_PRESS", (GdkEventButton *) event);
    break;
  case GDK_3BUTTON_PRESS:
    OZ_out(0) = createButtonEvent("GDK_3BUTTON_PRESS", (GdkEventButton *) event);
    break;
  case GDK_BUTTON_RELEASE:
    OZ_out(0) = createButtonEvent("GDK_BUTTON_RELEASE", (GdkEventButton *) event);
    break;
  case GDK_KEY_PRESS:
    OZ_out(0) = createKeyEvent("GDK_KEY_PRESS", (GdkEventKey *) event);
    break;
  case GDK_KEY_RELEASE:
    OZ_out(0) = createKeyEvent("GDK_KEY_RELEASE", (GdkEventKey *) event);
    break;
  case GDK_ENTER_NOTIFY:
    OZ_out(0) = createCrossingEvent("GDK_ENTER_NOTIFY", (GdkEventCrossing *) event);
    break;
  case GDK_LEAVE_NOTIFY:
    OZ_out(0) = createCrossingEvent("GDK_LEAVE_NOTIFY", (GdkEventCrossing *) event);
    break;
  case GDK_FOCUS_CHANGE:
    OZ_out(0) = createFocusEvent("GDK_FOCUS_CHANGE", (GdkEventFocus *) event);
    break;
  case GDK_CONFIGURE:
    OZ_out(0) = createConfigureEvent("GDK_CONFIGURE", (GdkEventConfigure *) event);
    break;
  case GDK_MAP:
    OZ_out(0) = OZ_atom("GDK_MAP");
    break;
  case GDK_UNMAP:
    OZ_out(0) = OZ_atom("GDK_UNMAP");
    break;
  case GDK_PROPERTY_NOTIFY:
    OZ_out(0) = OZ_atom("GDK_PROPERTY_NOTIFY");
    break;
  case GDK_SELECTION_CLEAR:
    OZ_out(0) = OZ_atom("GDK_SELECTION_CLEAR");
    break;
  case GDK_SELECTION_REQUEST:
    OZ_out(0) = OZ_atom("GDK_SELECTION_REQUEST");
    break;
  case GDK_SELECTION_NOTIFY:
    OZ_out(0) = OZ_atom("GDK_SELECTION_NOTIFY");
    break;
  case GDK_PROXIMITY_IN:
    OZ_out(0) = OZ_atom("GDK_PROXIMITY_IN");
    break;
  case GDK_PROXIMITY_OUT:
    OZ_out(0) = OZ_atom("GDK_PROXIMITY_OUT");
    break;
  case GDK_DRAG_ENTER:
    OZ_out(0) = OZ_atom("GDK_DRAG_ENTER");
    break;
  case GDK_DRAG_LEAVE:
    OZ_out(0) = OZ_atom("GDK_DRAG_LEAVE");
    break;
  case GDK_DRAG_MOTION:
    OZ_out(0) = OZ_atom("GDK_DRAG_MOTION");
    break;
  case GDK_DRAG_STATUS:
    OZ_out(0) = OZ_atom("GDK_DRAG_STATUS");
    break;
  case GDK_DROP_START:
    OZ_out(0) = OZ_atom("GDK_DROP_START");
    break;
  case GDK_DROP_FINISHED:
    OZ_out(0) = OZ_atom("GDK_DROP_FINISHED");
    break;
  case GDK_CLIENT_EVENT:
    OZ_out(0) = OZ_atom("GDK_CLIENT_EVENT");
    break;
  case GDK_VISIBILITY_NOTIFY:
    OZ_out(0) = createVisibilityEvent("GDK_VISIBILITY_NOTIFY", (GdkEventVisibility *) event);
    break;
  case GDK_NO_EXPOSE:
    OZ_out(0) = createNoExposeEvent("GDK_NO_EXPOSE", (GdkEventNoExpose *) event);
    break;
  default:
    OZ_out(0) = OZ_atom("UNSUPPORTED");
    break;
  }
  return OZ_ENTAILED;
} OZ_BI_end

/*
 * Define Interface
 */

static OZ_C_proc_interface oz_interface[] = {
  {"initializeSignalPort", 1, 0, alice_initialize_signal_port},
  {"signalConnect", 3, 0, alice_signal_connect},
  {"allocateGdkColor", 3, 1, alice_allocate_gdk_color},
  {"freeGdkColor", 1, 0, alice_free_gdk_color},
  {"getGdkEvent", 1, 1, alice_get_gdk_event},
  {"freeGdkRectangle", 1, 0, alice_free_gdk_rectangle},
  {0, 0, 0, 0}
};

char oz_module_name[] = "GtkSignal";

OZ_C_proc_interface *oz_init_module() {
  gtk_init(0, 0);
  gdk_init(0, 0);
  return oz_interface;
}
