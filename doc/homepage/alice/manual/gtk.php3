<?php include("macros.php3"); ?>

<?php heading("Gtk Library", "gtk <br> library") ?>

  <P>
    This manual describes the GTK+ language binding for Alice.
  </P>
  <P>
    GTK+ is a widget set that allows to build graphical user interfaces.
    The language binding described herein enables Alice programmers to
    build graphical user interfaces using this widget set.
    We say that it is a ``language binding'' because it
    provides access to GTK+ functionality by lifting its C interface
    to the Alice level. In other words, the interface basically is the same,
    but Alice is used to invoke it. 
  </P>
  <P>
    This manual summarizes the basics of GTK+ and describes how it is
    mapped to Alice. The goal is to enable Alice programmers to fully
    make use of the original reference documentation
    to write GTK+ applications in Alice. 
  </P>

<?php section("GTK+ Basics", "GTK+ Basics") ?>

  <P>
    GTK+ is organized into visual units called <EM>widgets</EM>.  Some widget
    classes are defined by refinement of some parent widget class using
    inheritance.  GTK+ has an object-oriented organization
    but is implemented in C. Thus, it can be easily mapped
    to Alice.
  </P>
  <P>
    The general steps to create a GTK+ widget are:
    <UL>
      <LI>Instantiate a widget object using the appropriate constructor.
      <LI>Connect appropriate handlers to all signals and events we wish
	to react to.
      <LI>Set attributes of the widget.
      <LI>Pack the widget into a container.
      <LI>Make the widget visible.
    </UL>
  </P>

<?php subsection("Widget Creation", "Widget Creation"); ?>
  <P>
    A widget object is created by instantiating a widget class.
    The constructors are the functions ending with <TT>new</TT>; some
    widget classes have multiple constructors. In those cases the
    constuctor usually contains a hint on what is different to the
    default constuctor.
    For example,
  <PRE>
      val window = Gtk.buttonNewWithLabel "Hello, World!"
  </PRE>
    creates a new object for a button labelled <TT>"Hello, World!"</TT>, whereas
    <TT>Gtk.buttonNew</TT> would have created a button object with an empty
    label.
  <P>
    Object creation yields a freshly created widget object whose attributes
    are set to reasonable default values.  (It will not appear on the screen
    until it has been packed into a container which then is made visible.)
    The operations on the widget and its parent classes are available
    through the object's methods.
  </p>

<?php subsection("Signals and Callbacks", "Signals and Callbacks"); ?>
 <P>
    GTK+ is event-driven, which means that when an event occurs, control
    is passed to the appropriate function.
 </P>
 <P><B>Signals.</B>
    Events in GTK+ are implemented through <EM>signals</EM>.  (Note that
    these signals are unrelated to Unix system signals.)  When an event
    occurs, such as the press of a mouse button, the appropriate signal
    will be <EM>emitted</EM> by the corresponding widget.  Some signals
    are common to all widget classes, such as <TT>"delete-event"</TT>, while
    others are widget-specific, such as <TT>"toggled"</TT> for a toggle button.
 </P>
  <P><B>Connecting Handlers.</B>
    The <TT>Gtk.signalConnect</TT> function allows to catch signals and
    cause invocation of actions.  For instance,
  <PRE>
    val id = Gtk.signalConnect(widget, signal, callback)
  </PRE>
  <P>
    causes <TT>callback</TT> to be invoked each time that the signal named
    <TT>signal</TT> is emitted on <TT>widget</TT>.  <TT>id</TT>
    is a unique identifier which can be used to manually emit
    the signal or to remove the handler.
  <P><B>Callbacks.</B>
    Callbacks are defined as functions:
  <PRE>
   fun myCallBack args = ...
  </PRE>
  <P>
    where <TT>args</TT> is a list of the arguments associated with the signal.
  <P><B>Low-level Events.</B>
    In addition to the high-level signals described above, there is a set
    of events that reflect the X Window System event mechanism (simulated
    non-X platforms such as Windows).  These can be caught using the special
    signal <TT>"event"</TT>.  For more details, see reference.
<?php subsection("Widget Attributes", "Widget Attributes"); ?>
  <P>
    GTK+ widgets organize their data using <EM>attributes</EM>.  Some attributes
    are read-only, others are mutable.  The latter allow for the widget to
    be reconfigured after creation.
  </P>
  <P>
    Attributes are named by strings and have an associated (typed) value.
    Many widgets define accessors for commonly used attributes, but in
    general they can be read using the <TT>Gtk.widetGet</TT> function
    and written to using the <TT>Gtk.widgetSet</TT> function.  Trying to access
    a non-existing attribute yields an error.
  </P>
<?php subsection("Containers", "Containers"); ?>
  <P>
    Widgets are laid out on the screen using so-called containers.
    Container widgets themselves usually do not provide any visible
    information, but display their child widgets according to a built-in
    strategy.  For example, an <TT>hBox</TT> container will align its children
    horizontally on the screen.  A container can contain other container
    widgets.
  </P>
  <P>
    GTK+ provides a variety of different container types covering the
    "daily" needs, which all are descendants of the <TT>container</TT>
    class.
  <P><B>Bins.</B>
    <TT>window</TT> is a subclass of <TT>bin</TT>, which is the superclass
    of all containers accepting at most one child.  Bins do not do any
    layouting.  If our window had contained more than one child, we would
    have needed to create another container to lay out the children, which
    would then have been the window's single child.
<?php subsection("Visibility", "Visibility"); ?>
  <P>
    The last step to start working with widgets on the screen is to make
    them visible.  This can be done manually by a bottom-up traversal of the
    container tree and calling each container's <TT>show</TT> function.  This is
    what the topmost container's <TT>showAll</TT> method does automatically.
  </P>
  <P>
    With few exceptions, signals emitted by a widget are only caught while
    it remains visible.
<?php subsection("Error Handling", "Error Handling"); ?>
  <P>
    GTK+ widgets do a lot of error-checking internally, but errors are
    just reported to the screen instead of being raised as an Oz exception.
    Errors discovered in the language binding's code are reported as
    exceptions.
  </P>

<?php section("How the C API is mapped to Alice",
	      "How the C API is mapped to Alice"); ?>
  <P>
    This chapter describes the details on how the C API is mapped to Alice.
    This knowledge is required when you want to make use of the original
    reference documentation at <A HREF="http://www.gtk.org/api/">
    http://www.gtk.org/api/</A> with the binding.
    (For the canvas, the documentation is at
    <A HREF="http://developer.gnome.org/doc/API/libgnomeui/">
    http://developer.gnome.org/doc/API/libgnomeui/</A>;
    only look at the <TT>GnomeCanvas</TT>, <TT>GnomeCanvasItem</TT> and
    <TT>GnomeCanvasGroup</TT> widgets.)
  </P>

<?php subsection("Modules", "Modules"); ?>
  <P>
    GTK+ is organized into the following modules:
    <UL>
     <LI><TT>structure Gdk<br>
             from "x-alice://lib/gtk/Gdk"</TT>
     <LI><TT>structure Gtk<br>
             from 'x-alice://lib/gtk/Gtk'</TT>
     <LI><TT>structure GtkCanvas<br>
              from 'x-alice://lib/gtk/GtkCanvas'</TT>
    </UL>
    Each module represents a namespace.  The corresponding API constants
    and functions are mapped to constants and functions
    in these namespaces.
  </P>

<?php subsection("Name Translation", "Name Translation"); ?>
  <P>
    We will illustrate how C structure fields and C functions are mapped
    to methods by an example.  Consider the C API for <TT>GtkButton</TT>,
    which is derived from <TT>GtkBin</TT>:
  <PRE>
    struct _GtkButton {
      GtkBin bin;
      GtkWidget *child;
      guint in_button : 1;
      guint button_down : 1;
      guint relief : 2;
    };
    /* constructors */
    GtkWidget *gtk_button_new();
    GtkWidget *gtk_button_new_with_label(const gchar *label);
    /* signal emitters */
    void gtk_button_pressed(GtkButton *button);
    void gtk_button_released(GtkButton *button);
    void gtk_button_clicked(GtkButton *button);
    void gtk_button_enter(GtkButton *button);
    void gtk_button_leave(GtkButton *button);
    /* attribute accessors */
    void gtk_button_set_relief(GtkButton *button, GtkReliefStyle newstyle);
    GtkReliefStyle gtk_button_get_relief(GtkButton *button);</PRE>
You will find those functions within the <TT>Gtk</TT> structure:
  <PRE>
   (* fields *)
   val buttonGetFieldBin : object -> object
   val buttonGetFieldChild : object -> object
   val buttonGetFieldInButton : object -> int
   val buttonGetFieldButtonDown : object -> int
   val buttonGetFieldRelief : object -> int
   (* constructors *)
   val buttonNew : unit -> object
   val buttonNewWithLabel : string -> object
   (* signal emitters *)
   val buttonPressed : object -> unit
   val buttonReleased : object -> unit
   val buttonClicked : object -> unit
   val buttonEnter : object -> unit
   val buttonLeave : object -> unit
   (* attribute accessors *)
   val buttonSetRelief : object * int -> unit
   val buttonGetRelief : object -> int</PRE>
  <P><B>General Scheme.</B>
    The general scheme is that all underscored identifiers are translated to
    use camel-casing.  Since functions belong to a module,
    the module name prefixes are cut off to increase
    readability.  The first letter of each function is downcased.
  </P>
  <P><B>Field Accessors.</B>
    Field accessors are also camel-cased, but in contrast to the standard
    methods, they use the <TT>{className}GetField{FieldName}</TT> pattern.
    For example, the <TT>button_down</TT> field above is read
    using the <TT>buttonGetFieldButtonDown</TT> accessor function.
    Fields cannot be written to directly.
  </P>
  <P><B>Constants.</B>
    The members of enumerations and flags are translated to constants
    exported with all upper case names containing underscores, with the
    module prefix cut off.
  </P>

<?php subsection("Types", "Types"); ?>
  <TABLE>
    <TR><TH>C Type<TH>Alice Type
    <TR><TD><TT>gint</TT>, <TT>guint</TT>,
            <TT>glong</TT>, <TT>gulong</TT><Td>int
    <TR><TD><TT>gboolean</TT><Td><TT>0</TT> and <TT>1</TT>
    <TR><TD><TT>gchar</TT>, <TT>guchar</TT><Td>int
    <TR><TD><TT>gfloat</TT>, <TT>gdouble</TT><Td>float
    <TR><Td>enumerations, flags<Td>integer
    <TR><TD><TT>gchar *</TT>, <TT>guchar *</TT><Td>string
    <TR><TD><TT>gchar *[]</TT>, <TT>guchar *[]</TT>
    <Td>list of strings
    <TR><TD><TT>GdkEvent *</TT><Td>record
    <TR><TD><TT>GList *</TT><Td>list of objects
    <TR><TD><TT>double[4]</TT><Td>list of four floats
    <TR><TD>all other pointers (i.e., <TT>GtkWidget *</TT>)
    <Td>object
  </TABLE>
  <P>
    The above table shows the mapping of the C types onto Alice types.
    Values are converted back and forth transparently, preserving identity
    whenever possible.
  </P>
  <P><B>Flags.</B>
    Flags are translated to integer constants.  In general, several
    constants can be combined using addition (<<+>>).
  </P>
  <P><B>Inout Arguments.</B>
    The <TT>int *</TT> and <TT>double *</TT>
    types are considered to be <EM>inout</EM> arguments.
    They are mapped to a <TT>XIn</TT> and <TT>XOut</TT> pair
    where <TT>XIn</TT> is given as a function argument and
    and <TT>XOut</TT> is returned by the function after invokation.
  <P><B>Gdk Events.</B>
    Where a GDK event (a value of the <TT>GdkEvent</TT> union
    type) appears as argument to a callback, a record is constructed using
    the label to indicate which member it belongs to.  The features of the
    record represent the corresponding event structure members.
  </P>
<?php footing() ?>
