
functor MkSpecial(val space : Util.spaces) :> SPECIAL =
    struct
	open TypeTree

	val includeFiles =
	    case space of
		Util.GTK => ["NativeGtkSpecial.hh"]
	      | Util.GDK => ["NativeGdkSpecial.hh"]
	      | Util.GNOMECANVAS => nil

        (* ignoreFuns: do not generate any code for: *)
	val ignoreFuns = 
	    case space of
		Util.GTK => ["gtk_init",
			     "gtk_init_check", 
			     "gtk_true", 
			     "gtk_false"]
	      | Util.GDK => ["gdk_init",
			     "gdk_init_check",
			     "gdk_pixbuf_new_from_xpm_data",
			     "gdk_keymap_get_direction" (**)]
	      | Util.GNOMECANVAS => ["gnome_canvas_join_gdk_to_art", (**)
				     "gnome_canvas_cap_gdk_to_art" (**)]

        (* specialFuns: generate asig, but no code for: *)
	val specialFuns = case space of
	    Util.GTK => 
		[FUNC("gtk_init", VOID, nil),
		 FUNC("gtk_get_event_stream", POINTER VOID, nil),
		 FUNC("gtk_signal_connect", NUMERIC (false, false, LONG), 
		      [POINTER VOID, STRING true, BOOL]),
		 FUNC("gtk_signal_disconnect", VOID, 
		      [POINTER VOID, NUMERIC (false, false, LONG)]),
		 FUNC("gtk_null", POINTER VOID, nil),
		 FUNC("gtk_gtk_true", NUMERIC(true, false, INT), nil),
		 FUNC("gtk_gtk_false", NUMERIC(true, false, INT), nil)]
	 | Util.GDK =>
	       [FUNC("gdk_init", VOID, nil),
	        FUNC("gdk_pixbuf_new_from_xpm_data", POINTER VOID,
		     [ARRAY (NONE, STRING true)])]
	  | _ => nil

       (* changedFuns: generate different asig and code for: *)
       val changedFuns = case space of
	   Util.GTK =>
	       [FUNC("gtk_combo_set_popdown_strings", VOID, 
		      [POINTER (STRUCTREF "GtkCombo"),
		       LIST ("GList", STRING true)])]
	 | _ => nil

       (* ignoreSafeFuns: do not generate any safe code for: *)
       val ignoreSafeFuns =
	   case space of
	       Util.GTK => ["gtk_init",
			    "gtk_get_event_stream",
			    "gtk_main",
			    "gtk_signal_connect",
			    "gtk_signal_disconnect",
			    "gtk_null",
			    "gtk_gtk_true",
			    "gtk_gtk_false"]
	     | Util.GDK => ["gdk_init"]
	     | _ => nil

       fun isIgnored (FUNC (n,_,_)) = 
	   (List.exists (fn n' => n=n') ignoreFuns) orelse
           (List.exists (fn (FUNC (n',_,_)) => n=n' | _ => false) changedFuns)
	 | isIgnored _              = false

       fun isIgnoredSafe (FUNC (n,_,_)) = List.exists (fn n' => n=n') 
	                                    ignoreSafeFuns
	 | isIgnoredSafe _              = false

    end
