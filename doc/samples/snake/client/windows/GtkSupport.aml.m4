(* Alice Snake 2.0 - GUI
*
*  Authors: Benedikt Grundmann / Sebastian Germesin
*
*  $Revision$
*
*  Last updated: $Date$ by $Author$
* 
*
*)
changequote([[,]])

import structure Gtk         from "x-alice:/lib/gtk/Gtk"
import structure Gdk         from "x-alice:/lib/gtk/Gdk"
ifdef([[GTK2]],[[
import structure GnomeCanvas from "x-alice:/lib/gtk/GnomeCanvas"
]],[[
import structure GtkCanvas   from "x-alice:/lib/gtk/GtkCanvas"
]])


ifdef([[GTK2]],[[
(* seam aka Gtk2 specifics *)
structure Canvas = 
struct

    open GnomeCanvas

    fun new _ = GnomeCanvas.new ()
    fun newAa _ = GnomeCanvas.newAa ()

    fun freeze _ = ()
    fun thaw   _ = ()
    fun createGroup (p, x, y) = 
		 itemCreate (p, groupGetType (), [("x", Gtk.REAL x),
						  ("y", Gtk.REAL y)])
    fun createEllipse (p, x1, y1, x2, y2, fillColor, outColor) = 
		 itemCreate (p, ellipseGetType (),
			     [("x1", Gtk.REAL x1),
			      ("y1", Gtk.REAL y1),
			      ("x2", Gtk.REAL x2),
			      ("y2", Gtk.REAL y2),
			      ("fill_color_gdk", Gtk.OBJECT fillColor),
			      ("outline_color_gdk", Gtk.OBJECT outColor)])
    fun createRect (p, x1, y1, x2, y2, fillColor, outColor) =
		 itemCreate (p, rectGetType (),
			     [("x1", Gtk.REAL x1),
			      ("y1", Gtk.REAL y1),
			      ("x2", Gtk.REAL x2),
			      ("y2", Gtk.REAL y2),
			      ("fill_color_gdk", Gtk.OBJECT fillColor),
			      ("outline_color_gdk", Gtk.OBJECT outColor)])
    fun createLine (p, points, fillColor, width) = 
		 itemCreate (p, lineGetType (),
			     [("points", Gtk.OBJECT (makePoints points)),
			      ("fill_color_gdk", Gtk.OBJECT fillColor),
			      ("width_pixels", Gtk.INT width)])
    fun createText (p, text, font, x, y, fillColor, anchor) = 
		 itemCreate (p, textGetType (),
			     [("text", STRING text),
			      ("font", STRING font),
			      ("x", REAL x),
			      ("y", REAL y),
			      ("fill_color_gdk", OBJECT fillColor),
			      ("anchor", INT (Gtk.GtkAnchorTypeToInt anchor))])

     fun requestRedraw _ = ()

     fun getScrollOffsets (object, _, _) = GnomeCanvas.getScrollOffsets object


end

structure Gtk =
struct
    open Gtk
    val null = Gtk.NULL
    val menuAppend = menuShellAppend
    val menuBarAppend = menuShellAppend

    val createTextWidget = textViewNew
			     
    fun textWidgetInsert (widget, text, color) = 
       let
	   val buffer = textViewGetBuffer widget
       in
	   textBufferInsertAtCursor (buffer, text, ~1)
       end
end

structure Gdk =
struct
    open Gdk
end

]],[[
(* mozart aka Gtk1.2 specifics *)
structure Canvas = 
struct
     open GtkCanvas

     val freeze = Gtk.layoutFreeze
     val thaw   = Gtk.layoutThaw
     fun createGroup (p, x, y) = 
	 itemNew (p, GROUP, [("x", Gtk.DOUBLE x),
			     ("y", Gtk.DOUBLE y)])
     fun createEllipse (p, x1, y1, x2, y2, fillColor, outColor) = 
	 itemNew (p, ELLIPSE,
		  [("x1", Gtk.DOUBLE x1),
		   ("y1", Gtk.DOUBLE y1),
		   ("x2", Gtk.DOUBLE x2),
		   ("y2", Gtk.DOUBLE y2),
		   ("fill_color_gdk", Gtk.OBJECT fillColor),
		   ("outline_color_gdk", Gtk.OBJECT outColor)])
     fun createRect (p, x1, y1, x2, y2, fillColor, outColor) =
	 itemNew (p, RECTANGLE,
		  [("x1", Gtk.DOUBLE x1),
		   ("y1", Gtk.DOUBLE y1),
		   ("x2", Gtk.DOUBLE x2),
		   ("y2", Gtk.DOUBLE y2),
		   ("fill_color_gdk", Gtk.OBJECT fillColor),
		   ("outline_color_gdk", Gtk.OBJECT outColor)])
     fun createLine (p, points, fillColor, width) = 
	 itemNew (p, LINE,
		  [("points", Gtk.POINTS points),
		   ("fill_color_gdk", Gtk.OBJECT fillColor),
		   ("width_pixels", Gtk.INT width)])
     fun createText (p, text, font, x, y, fillColor, anchor) = 
	 itemNew (p, TEXT,
		  [("text", STRING text),
		   ("font", STRING font),
		   ("x", DOUBLE x),
		   ("y", DOUBLE y),
		   ("fill_color_gdk", OBJECT fillColor),
		   ("anchor", INT anchor)])

     fun windowToWorld (can, w, h) = 
	            GtkCanvas.windowToWorld (can, w, h, 0.0, 0.0)

end

structure Gtk =
struct
    open Gtk
    val null = Gtk.null ()
    val widgetSetSizeRequest = widgetSetUsize

    fun createTextWidget () = textNew (null, null)
			     
    fun textWidgetInsert (widget, text, color) =
	let
	    val font = Gdk.fontLoad "-*-times-bold-*-*-*-12-*-*-*-*-*-*-*"
	    val stdFont = Gdk.fontLoad "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
	    val white = Gdk.allocColor (65535, 65535, 65535)
	    val black = Gdk.allocColor (0, 0, 0)
	in
	    if font <> null andalso color <> null
		then textInsert (widget, font, color, white, text, ~1)
	    else textInsert (widget, stdFont, black, white, text, ~1)
	end

    fun buttonSetLabel (bt, text) = 
        let
	    val lbl = Gtk.buttonGetFieldChild bt
        in
	    labelSetText (lbl, text)
 	end

    val radioButtonGetGroup = radioButtonGetFieldGroup

    fun windowGetSize obj =
        let
	    val alloc  = widgetGetFieldAllocation obj
	    val width  = allocationGetFieldWidth alloc
  	    val height = allocationGetFieldHeight alloc
	in
	   (width, height)
	end

end

structure Gdk =
struct
    open Gdk
    val colorNew = allocColor
end
]])





