#include <string.h>
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>

#define WIDTH 400
#define HEIGHT 200

static gboolean
quit_cb (GtkWidget *widget, GdkEventAny *event, gpointer dummy)
{
	gtk_main_quit ();

	return TRUE;
}

int main(int argc, char *argv[])
{
  gtk_init(&argc, &argv);
  GtkWidget *app = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  g_signal_connect (app, "delete_event",
		    G_CALLBACK (quit_cb), NULL);

  GtkWidget *canvas = gnome_canvas_new();
  GnomeCanvasGroup *rootGroup = gnome_canvas_root(GNOME_CANVAS(canvas));
  gnome_canvas_item_new(rootGroup,
			gnome_canvas_rect_get_type(),
			"x1", (double)10,
			"y1",(double)10,
			"x2",(double)(WIDTH-10),
			"y2",(double)(HEIGHT-10),
			"fill_color","white",
			"outline_color","black",
			NULL);

  gtk_container_add (GTK_CONTAINER (app), canvas);
  gtk_widget_show_all (app);

  gtk_main();

  return 0;
}
