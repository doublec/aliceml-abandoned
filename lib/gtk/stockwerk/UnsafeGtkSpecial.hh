#ifndef _UNSAFE_GTK_SPECIAL_HH_
#define _UNSAFE_GTK_SPECIAL_HH_ { 2, UnsafeGtkSpecialInit }

DEFINE1(UnsafeGtk_TypeMismatch) {
  Constructor *ccVal = Constructor::FromWord(TypeMismatchConstructor);
  ConVal *conVal     = ConVal::New(ccVal, 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

void UnsafeGtkSpecialInit(Record *record) {
  record->Init("'TypeMismatch", TypeMismatchConstructor);
  INIT_STRUCTURE(record, "UnsafeGtk", "TypeMismatch",
		 UnsafeGtk_TypeMismatch, 1, true);
}

static word tail = 0;

DEFINE0(UnsafeGtk_init) {
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

DEFINE1(UnsafeGtk_setEventStream) {
  tail = x0;
  RootSet::Add(tail);
  RETURN_UNIT;
} END

DEFINE0(UnsafeGtk_null) {
  RETURN(Store::UnmanagedPointerToWord(NULL));
} END

DEFINE0(UnsafeGtk_gtkTrue) {
  RETURN(Store::IntToWord(TRUE));
} END

DEFINE0(UnsafeGtk_gtkFalse) {
  RETURN(Store::IntToWord(FALSE));
} END

void push_front(word *paramlist, word value) {
  TagVal *cons = TagVal::New(0,2);
  cons->Init(0,value);
  cons->Init(1,*paramlist);
  *paramlist = cons->ToWord();
}

void generic_marshaller(GClosure *closure, GValue *return_value, 
			guint n_param_values, const GValue *param_values, 
			gpointer , gpointer marshal_data) {

  int funid = GPOINTER_TO_INT(closure->data);
  g_print("event occured: %d\n", funid);
  g_print("event has %d params\n", n_param_values);

  Future *oldfuture = reinterpret_cast<Future*>(Store::WordToTransient(tail));
  tail = (Future::New())->ToWord();
  word paramlist = tail;
  word value;
  TagVal *cons;

  for (int i = n_param_values-1; i >= 0; i--) {
    value = Store::IntToWord(0);

    const GValue *val = param_values + i;

    GTypeQuery q;
    memset(&q, 0, sizeof(q));
    g_type_query(G_VALUE_TYPE(val), &q);
    g_print("Param #%d, Type: %ld, Name: %s\n", i, G_VALUE_TYPE(val), 
	    q.type_name);

    switch(G_VALUE_TYPE(val)) {
    case G_TYPE_CHAR:   
      g_print("CHAR = %c\n", g_value_get_char(val));
      value = Store::IntToWord(static_cast<int>(g_value_get_char(val)));
      break;
    case G_TYPE_UCHAR:  
      g_print("UCHAR = %c\n", g_value_get_uchar(val));
      break;
    case G_TYPE_BOOLEAN:
      g_print("BOOLEAN = %d\n", g_value_get_boolean(val));
      value = Store::IntToWord(static_cast<int>(g_value_get_boolean(val)));
      break;
    case G_TYPE_INT:    
      g_print("INT = %i\n", g_value_get_int(val));
      value = Store::IntToWord(g_value_get_int(val));
      break;
    case G_TYPE_UINT:   g_print("UINT = %i\n", g_value_get_uint(val));
                        break;
    case G_TYPE_LONG:   
      g_print("LNG = %li\n", g_value_get_long(val));
      break;
    case G_TYPE_ULONG:  
      g_print("ULONG = %li\n", g_value_get_ulong(val));
      break;
    case G_TYPE_INT64:  
      g_print("INT64 = %lli\n", g_value_get_int64(val));
      break;
    case G_TYPE_UINT64: 
      g_print("UINT64 = %lli\n", g_value_get_uint64(val));
      break;
    case G_TYPE_ENUM:   
      g_print("ENUM = %d\n", g_value_get_enum(val));
      break;
    case G_TYPE_FLAGS:  
      g_print("FLAGS = %d\n", g_value_get_flags(val));
      break;
    case G_TYPE_FLOAT:  
      g_print("FLOAT = %f\n", g_value_get_float(val));
      break;
    case G_TYPE_DOUBLE: 
      g_print("DOUBLE = %f\n", g_value_get_double(val));
      break;
    case G_TYPE_STRING: 
      g_print("STRING = %s\n", g_value_get_string(val));
      break;
    case G_TYPE_POINTER:
      g_print("POINTER = %p\n", g_value_get_pointer(val));
      break;
    case G_TYPE_OBJECT: 
      g_print("OBJECT = %p\n", g_value_get_object(val));
      break;
    case G_TYPE_PARAM:  
      g_print("PARAM = %s\n", g_value_get_param(val)->name);
      break;
    case G_TYPE_BOXED:  
      g_print("BOXED = %p\n", g_value_get_boxed(val));
      break;
    default:            
      g_print("NOT A FUNDAMENTAL TYPE\n");
    }
    push_front(&paramlist,value);
  }

  push_front(&paramlist, Store::IntToWord(n_param_values));
  push_front(&paramlist, Store::IntToWord(funid));
  
  oldfuture->ScheduleWaitingThreads();
  oldfuture->Become(REF_LABEL, paramlist);

  //  gtk_widget_ref(widget);
  //  gtk_widget_unref(widget);

  //if (G_VALUE_HOLDS(return_value, G_TYPE_BOOLEAN))
  //    g_value_set_boolean(return_value, FALSE);

}

DEFINE4(UnsafeGtk_signalConnect) {
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

DEFINE2(UnsafeGtk_signalDisconnect) {
  DECLARE_UNMANAGED_POINTER(obj,x0);
  DECLARE_INT(handler_id,x1);
  g_signal_handler_disconnect(G_OBJECT(obj), static_cast<gulong>(handler_id));
  RETURN_UNIT;
} END

#endif
