#include <stdio.h>
#include <gtk/gtk.h>
//#include "generic/NativeAuthoring.hh"

#define PRINT(a) printf(a)
#define EXECUTE(f,x) f(x)

int test(float x) {
  return 3;
}

typedef int (*FuncType) (float x);

int main(int argc, char *argv[]) {
  gtk_init(&argc, &argv);
  GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  if (g_type_is_a(G_OBJECT_TYPE(w), 0) == FALSE)
    printf("!!\n");

  const char *arr[] = { "Test", "Test" };
  gdk_pixbuf_new_from_xpm_data (arr);

  guint x = 0;
  int* y = reinterpret_cast<int*>(&x);

  gchar c[100] = "Test";
  void *p = c;

  //  printf("%d\n", B);
  EXECUTE(PRINT,"4");
	       //  DECLARE_CARRAY(in0,x0,int,DECLARE_INT)
  //  FuncType f = static_cast<int(*)(float x)>(&test);
  void (*f)() = reinterpret_cast<void(*)()>(&test);
  int (*f2)(float x) = reinterpret_cast<int(*)(float x)>(f);
  printf("x%d\n",f2(4.3));
  //printf("x\n");
	       //  if (f == INVALID_POINTER) return 1;
  return 0;
}
