(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

import structure OS from "x-alice:/lib/OS.ozf"
import val print from "x-alice:/lib/TextIO.ozf"
import structure GTK from "x-alice:/lib/gtk/GTK.ozf"

local
    fun Hello () = print "Hello, World!"
    fun DeleteEvent () = print "Delete Event occurred"
    fun Destroy () = print "Destroy Event occurred"
    val Window = GTK.windowNew GTK.WINDOW_TOPLEVEL
    val DestroyId = GTK.signalConnect(Window, "destroy", Destroy)
    val Button = GTK.buttonNewWithLabel "Hello, World!"
    val ClickedId = GTK.signalConnect(Button, "clicked", Hello)
in
    val _ = (GTK.containerSetBorderWidth(Window, 10);
	     GTK.containerAdd(Window, Button);
	     GTK.widgetShowAll(Window))
end
