#ifndef _NATIVE_GTK_SPECIAL_HH_
#define _NATIVE_GTK_SPECIAL_HH_ { 2, NativeGtkSpecialInit }

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
  /*
  DECLARE_GLIST(argvlist, Properties::commandLineArguments, GList, g_list, 
		DECLARE_CSTRING);
  int argc = g_list_length(argvlist);
  printf("**length: %d", argc);
  char **argv = new (char*)[argc];
  for (int i = 0; i < argc; i++) {
    printf("**i: %d", i);
    argv[i] = reinterpret_cast<char*>(g_list_nth_data(argvlist, i));
  }
  gtk_init(&argc,&argv);
  delete [] argv;
  */

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

GType G_LIST_TYPE = g_type_from_name("GList");
GType G_SLIST_TYPE = g_type_from_name("GSList");

word create_param(int tag, word value) {
  TagVal *param = TagVal::New(tag,1);
  param->Init(0, value);
  return param->ToWord();
}

word create_object(GType t, gpointer p) {
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
      value = Store::UnmanagedPointerToWord(p);
  return create_param(tag, value);
}

void generic_marshaller(GClosure *closure, GValue *return_value, 
			guint n_param_values, const GValue *param_values, 
			gpointer , gpointer marshal_data) {

  int funid = GPOINTER_TO_INT(closure->data);
  //  g_print("event occured: %d\n", funid);
  //  g_print("event has %d params\n", n_param_values);

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
      //  g_print("NAFT AS POINTER: %p\n", g_value_peek_pointer(val));
      if (i==0)
	widget = g_value_peek_pointer(val);
      else
	value = create_object(G_VALUE_TYPE(val), g_value_peek_pointer(val));
    }
    if (!widget) push_front(&paramlist,value);
  }
  
  Tuple *tup = Tuple::New(3);
  tup->Init(0,Store::IntToWord(funid));
  tup->Init(1,Store::UnmanagedPointerToWord(widget));
  tup->Init(2,paramlist);
  
  push_front(&stream,tup->ToWord());

  oldfuture->ScheduleWaitingThreads();
  oldfuture->Become(REF_LABEL, stream);

  //if (G_VALUE_HOLDS(return_value, G_TYPE_BOOLEAN))
  //    g_value_set_boolean(return_value, FALSE);
}

DEFINE4(NativeGtk_signalConnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_CSTRING(signalname,x1);
  DECLARE_INT(funid,x2);
  DECLARE_BOOL(after,x3);

  GClosure *closure = g_cclosure_new(G_CALLBACK(generic_marshaller),
				     GINT_TO_POINTER(funid), NULL);
  g_closure_set_marshal(closure, generic_marshaller);
  gulong ret = g_signal_connect_closure(G_OBJECT(obj), signalname, 
					closure, after ? TRUE : FALSE); 
  
  RETURN(Store::IntToWord(static_cast<int>(ret)));
} END

DEFINE3(NativeGtk_signalConnectDelete) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_INT(funid,x1);

  GClosure *closure = g_cclosure_new(G_CALLBACK(generic_marshaller),
				     GINT_TO_POINTER(funid), NULL);
  g_closure_set_marshal(closure, generic_marshaller);
  gulong ret = g_signal_connect_closure(G_OBJECT(obj), "delete-event", 
					closure, FALSE); 

  RETURN(Store::IntToWord(static_cast<int>(ret)));
} END

DEFINE2(NativeGtk_signalDisconnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_INT(handler_id,x1);
  g_signal_handler_disconnect(G_OBJECT(obj), static_cast<gulong>(handler_id));
  RETURN_UNIT;
} END

#endif
