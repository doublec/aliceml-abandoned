(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

(*
  This functor defines the items for which a special binding
  (or no binding at all) should be generated.
*)

functor MkSpecial(val space : Util.spaces) :> SPECIAL =
    struct
	open TypeTree

	(* includeFile: file that is included in the generated native code.  *)
        (* The triple means: 1. name of the included file (or "" if none)    *)
        (*                   2. name of a possible init function in the file *)
        (*                   3. number of entries the init function adds to  *)
        (*                      the structure.                               *)
	val includeFile =
	    case space of
		Util.GTK => ("NativeGtkSpecial.hh", "", 0)
	      | Util.GDK => ("NativeGdkSpecial.hh", "", 0)
	      | Util.GNOMECANVAS => ("NativeGnomeCanvasSpecial.hh", "", 0)
	      | _ => ("", "", 0)

        (* ignoreItems: do not generate any code for: *)
	val ignoreItems = 
	    case space of
		Util.GTK => ["gtk_init",  
			     "gtk_init_check", 
			     "gtk_true",  
			     "gtk_false", 
			     "gtk_tree_store_new",
			     "gtk_type_init",
			     "gtk_signal_compat_matched",
			     "_GtkSocket",
			     "_GtkPlug"] (* not available for win32 *)
	      | Util.GDK => ["gdk_init",
			     "gdk_init_check",
			     "gdk_pixbuf_new_from_xpm_data"]
	      | Util.GNOMECANVAS => ["gnome_canvas_item_new",
				     "gnome_canvas_join_gdk_to_art",
				     "gnome_canvas_cap_gdk_to_art"]
	      | _ => nil

        (* specialFuns: for these functions, generate a full binding except
	   for the native wrapper code. (This code should then be written
           manually into the includeFile specified above.) *)
	val specialFuns = case space of
	    Util.GTK => 
		[FUNC("gtk_text_iter_new", POINTER (STRUCTREF "_GtkTextIter"), 
		      nil),
		 FUNC("gtk_tree_iter_new", POINTER (STRUCTREF "_GtkTreeIter"), 
		      nil),
		 FUNC("gtk_tree_store_new", POINTER VOID, nil),
		 FUNC("gtk_tree_view_get_selected_string", 
		      STRING true, [POINTER VOID])]

	 | Util.GDK =>
	       [FUNC("gdk_pixbuf_new_from_xpm_data", POINTER VOID,
		     [ARRAY (NONE, STRING true)]),
		FUNC("gdk_color_new", POINTER (STRUCTREF "_GdkColor"), 
		     [NUMERIC (false,false,INT),
		      NUMERIC (false,false,INT), NUMERIC (false,false,INT)]),
		FUNC("gdk_point_new", POINTER (STRUCTREF "_GdkPoint"),
		     [NUMERIC (true,false,INT), NUMERIC (true,false,INT)]),
		FUNC("gdk_rectangle_new", POINTER (STRUCTREF "_GdkRectangle"),
		     [NUMERIC (true,false,INT), NUMERIC (true,false,INT),
		      NUMERIC (true,false,INT), NUMERIC (true,false,INT)])]
	  | Util.GNOMECANVAS =>
	       [FUNC("gnome_canvas_points_set_coords", VOID,
		     [POINTER VOID, NUMERIC (true,false,INT), 
		      NUMERIC (true,false,INT)]),
		FUNC("gnome_canvas_item_new", POINTER VOID,
		     [POINTER VOID, NUMERIC (true,false,INT)])]
	  | _ => nil

       (* changedFuns: assume different type information for: *)
       val changedFuns = case space of
	   Util.GTK =>
	       [FUNC("gtk_combo_set_popdown_strings", VOID, 
		      [POINTER (STRUCTREF "GtkCombo"),
		       LIST ("GList", STRING true)])]
	 | _ => nil

       (* ignoreSafeItems: *)
       (* do not generate any code in the unsafe component for: *)
       val ignoreSafeItems =
	   case space of
	       Util.GTK => ["gtk_init",
			    "gtk_get_event_stream",
			    "gtk_main",
			    "gtk_signal_connect",
			    "gtk_signal_disconnect",
			    "gtk_null",
			    "gtk_gtk_true",
			    "gtk_gtk_false",
			    "gtk_g_object_unref",
			    "gtk_delete_unref"]
	     | Util.GDK => ["gdk_init"]
	     | _ => nil


       (* isIgnored: true if no binding should be generated for an item *)
       fun isIgnored (FUNC (n,_,_)) = 
	   (Util.contains n ignoreItems) orelse
           (List.exists (fn (FUNC (n',_,_)) => n=n' | _ => false) changedFuns)
	 | isIgnored (STRUCT (n, _)) = Util.contains n ignoreItems
	 | isIgnored (ENUM (n, _)) = Util.contains n ignoreItems
	 | isIgnored _ = false

       (* isIgnoredSafe: true if no binding in the unsafe component *)
       (*                should be generated for an item *)
       fun isIgnoredSafe (FUNC (n,_,_)) = Util.contains n ignoreSafeItems
	 | isIgnoredSafe (STRUCT (n, _)) = Util.contains n ignoreSafeItems
	 | isIgnoredSafe (ENUM (n, _)) = Util.contains n ignoreSafeItems
	 | isIgnoredSafe _              = false

    end
