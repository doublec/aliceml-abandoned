<?php include("macros.php3"); ?>
<?php heading("Pickling", "pickling") ?>


<?php section("overview", "overview") ?>

<P>A <EM>pickle</EM> is a serialized and closed representation of a value,
stored in a file. Pickles can be used to exchange arbitrary data structures
between processes. Pickles may contain higher-order values (i.e. functions) as
well as complete module (e.g. abstract type implementations). Pickling is
type-safe: a pickle in fact is a <A href="packages.php3">package</A>, a pair of
a value and its type. Types are checked during unpickling.</P>


<?php section("semantics", "semantics") ?>

<P>Most values can be pickled. There are some semantic implications
and necessary restrictions, though. We distinguish three sorts of values:</P>

<UL>
<LI> <I>Functional</I>
     values do not contain any stateful objects. They can be
     freely pickled and unpickled. After putting a functional object in
     a pickle and reextracting it it is indistinguishable from the original
     object. </LI>

<LI> <I>Stateful</I>
     values do contain mutable objects like references or arrays. Stateful
     data can also be pickled freely. However, pickling of stateful objects
     has a copying semantics: each time such an object is extracted from
     a pickle a fresh copy of the object is created. Sharing between
     stateful object is maintained inside a pickle, though. </LI>

<LI> <I>Sited</I>
     objects are connected to a parent process. They refer to
     resources that are not available outside the process. Examples
     are input streams of open files or first-class threads.
     Sited objects may not be pickled.
     Any attempt to pickle values containing sited objects will result
     in a runtime exception. </LI>
</UL>

<P>Higher-order values (i.e. function closures) may contain stateful or sited
data without showing in their type. Moreover, functions that create sited
objects are also sited. Special care has to be taken to avoid runtime errors
from attempts of pickling such objects. This is particularly important when
pickling modules.</P>

<P><A href="futures.php3">Futures</A> are not pickled. Instead, the pickling
operation will <A href="futures.php3#request">request</A> all futures that
contained in the closure of the value to be pickled.</P>


<?php section("export", "exporting and importing modules") ?>

<P>Pickling is available through the structure <A
href="library/pickle.php3"><TT>Pickle</TT></A>. The canonical operation to
create a pickle is the operation</P>

<PRE class=code>
save : string * package -> unit</PRE>

<P>For example, the <TT>Int</TT> module can be exported as a pickle as
follows:</P>

<PRE class=code>
Pickle.save("Int." ^ Pickle.extension, pack Int :> INTEGER)</PRE>

<P>The package will be written into a file with the specified name. The string
<TT>Pickle.extension</TT> gives the file extension usually used for pickles on
the target platform. If the module contained references to any sited
objects, an <A href="library/io.php3"><TT>IO.Io</TT></A> exception would be
raised, with <TT>Sited</TT> indicating the cause of the
failure (<TT>Int</TT> is not sited, however).</P>

<P>The inverse operation is unpickling. For example:</P>

<PRE class=code>
structure Int' = unpack Pickle.load("Int." ^ Pickle.extension) : INTEGER</PRE>

<P>If unpickling is successful, <TT>Int'</TT> will be accessible as a structure
with signature <TT>INTEGER</TT>. Loading of a pickle may fail with an
<A href="library/io.php3"><TT>IO.Io</TT></A> exception.</P>


<?php subsection("export-sharing", "sharing") ?>

<P>Pickled modules can contain abstract types. Sometimes it is
necessary to express sharing between abstract types of different
pickles. The way to deal with this is using appropriate type constraints upon
unpacking the loaded package. For example, consider an abstract datatype</P>

<PRE class=code>
signature T =
sig
    type t
    val mk : int -> t
    val f : t -> int
end</PRE>

<P>that is stored in a pickle <TT>p1</TT>. Another pickle <TT>p2</TT> contains
a value of that type. Both can be loaded and used together:</P>

<PRE class=code>
structure T = unpack p1 : T
structure V = unpack p2 : (val x : T.t)

val n = T.f V.x
</PRE>

<P>See also the section on <A
href="packages.php3#package-sharing">sharing across packages</A>.</P>


<?php section("components", "unpickling components") ?>

<P>Pickles are closely related to <A href="components.php3">components</A>.
In fact, a pickle is a special case of a component, and arbitrary
components may be loaded as pickles.</P>

<P>A pickle is an <I>evaluated</I> component (and we will use both terms
as synonyms in the following). An evaluated component only
contains the fully evaluated export, it will not be executed during
load. This particularly means that loading it will neither generate
any side effects, nor produce new generative entities like types or
exceptions. An evaluated component does not import any other component.</P>

<P>
    A pickle is a component that just contains a module <TT>X</TT>
    which is the one passed to the <TT>Save</TT> functor. If necessary,
    you can thus import a pickle through an import announcement.
  </P>

  <P>
    On the other hand, arbitrary components may be loaded through
    <TT>Pickle.Load</TT> as long as they contain a module X
    with appropriate signature.
    The file name passed to the functor may actually be a
    full URI, which will be resolved just like the URI in an import
    announcement of a <A href="components.php3">component</A>.
    If the component at the URI is not a pickle, it will be
    executed before the functor returns. This may cause other
    components to be loaded, if respective imports are required.
  </P>

  <P>
    Note that execution
    causes the creation of new <A href="packages.php3#dynamic">generative
    types</A>. So if a non-pickle
    component is loaded twice, any generative type contained in
    its export will be incompatible between both instances.
    Similarly for exceptions.
  </P>

  <P>
    In some sense, import announcements may be seen as syntactic sugar for
    appropriate applications of <TT>Pickle.Load</TT>. However, there are
    important differences between both mechanisms:
  </P>

  <UL>
    <LI> The component's signature need not be specified explicitly in import
         announcements, since the compiler fetches it automatically from
	 the component during compilation. This requires that the component
	 exists at compile time, however, which is not the case for
	 the load mechanism.
    <LI> Linking of imports happens lazily, while the <TT>Load</TT>
         functor executes at least the root component eagerly. (Lazy execution
	 can be achieved through the use of the
	 <A href="laziness.php3#modules"><TT>ByNeed</TT></A> functor though.)
    <LI> Components imported via import announcements are only linked
         in once in a given process, regardless how many import announcements
	 exist for the same component. <TT>Load</TT> will always reload
	 the requested component.
    <LI> Import announcements allow the import of arbitrary and multiple
         entities from a component while the <TT>Load</TT> functor can only
	 load modules (whose name is fixed to <TT>X</TT>).
  </UL>

  <P>
    It may be desirable to unify both mechanisms even more. This is likely
    to be done for some future release.
  </P>

<?php footing() ?>
