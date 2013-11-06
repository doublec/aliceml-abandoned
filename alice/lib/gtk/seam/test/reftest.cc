#include <gtk/gtk.h>
#include <stdio.h>

int counter = 0;

gint eventDestroy(GtkObject*, gpointer) {
  counter = 1;
}

int main(int argc, char *argv[]) {
  
  gtk_init(&argc, &argv);

  GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_widget_ref(w);

  guint connid = g_signal_connect(G_OBJECT(w), "destroy", G_CALLBACK(eventDestroy), NULL);

  printf("%d - %d\n", connid, g_signal_handler_find(w,G_SIGNAL_MATCH_FUNC,0,0,NULL,eventDestroy,NULL));

  gtk_widget_show_all(w);

  while (G_IS_OBJECT(w)) {
    if (counter > 0) 
      if (++counter == 100000) {
	gtk_widget_unref(w);
      }
    if (gtk_events_pending() == TRUE)
      gtk_main_iteration();
  }

  return 0;
}
