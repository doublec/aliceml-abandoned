<?php include("macros.php3"); ?>

<?php heading("Gtk Library", "gtk <br> library") ?>



<?php section("overview", "overview") ?>

  <P>
    A graphical user interface library, built on the GTK Toolkit, is
    available via the following components:
  </P>

  <UL>
    <LI>
      <TT><A name="gtkcore">structure</A>
      <A href="http://developer.gnome.org/doc/API/gtk/index.html">GtkCore</A>
      <BR>from "x-alice:/lib/gtk/GtkCore"</TT>
    </LI>
    <LI>
      <TT><A name="gtk">structure</A>
      <A href="http://developer.gnome.org/doc/API/gtk/index.html">Gtk</A>
      <BR>from "x-alice:/lib/gtk/GTK"</TT>
    </LI>
    <LI>
      <TT><A name="gdk">structure</A>
      <A href="http://developer.gnome.org/doc/API/gdk/index.html">Gdk</A>
      <BR>from "x-alice:/lib/gtk/GDK"</TT>
    </LI>
  </UL>

  <P>
    All modules together contain almost the full set of functions originally
    provided by the toolkit.  The functions are available under names
    following the so-called "camel casing" scheme, with the library
    prefix removed, i.e., <TT>gtk_foo_bar</TT> would be available as
    <TT>Gtk.fooBar</TT> and <TT>gdk_foo_baz</TT> as <TT>Gdk.fooBaz</TT>,
    respectively.
  </P>

  <P>
    Upon this, we provide the GnomeCanvas via the component:
  </P>

  <UL>
    <LI>
      <TT><A name="canvas">structure</A>
      <A href="http://developer.gnome.org/doc/API/libgnomeui/book1.html">Canvas</A>
      <BR>from "x-alice:/lib/gtk/Canvas"</TT>
    </LI>
  </UL>

  <P>
    The functions are also available under names following the "camel casing" scheme,
    with the <TT>gnome_canvas</TT> prefix removed, i. e., <TT>gnome_canvas_new</TT> would be
    available as <TT>Canvas.new</TT>.
  </P>

  <P>
    Besides reading the documentation provided by GTK itself, it is recommended to take
    to look into the <A HREF="examples.php3#gtk">samples</A>.
  </P>

<?php footing() ?>
