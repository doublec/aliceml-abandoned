<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2 - Pickling", "pickling") ?>



<?php section("overview", "overview") ?>

  <P>
    A major feature of Stockhausen is <I>pickling</I>, the ability to
    make almost arbitrary user data structures persistent. This includes
    higher-order data, i.e. functions. But it does not stop there, as
    Stockhausen even allows pickling of complete modules.
  </P>

  <P>
    Pickling is type-safe in Stockhausen: a pickle in fact is a
    <A href="package.php3">package</A>, a pair of a value and its type.
    Types are checked during unpickling.
  </P>


<?php section("semantics", "semantics") ?>

  <P>
    Most Alice values can be pickled. There are some semantic implications
    and necessary restrictions, though. We distinguish three sorts of values:
  </P>

  <UL>
    <LI> <I>Functional</I>
	 values do not contain any stateful objects. They can be
	 freely pickled and unpickled. After putting a functional object in
	 a pickle and reextracting it it is indistinguishable from the original
	 object.

    <LI> <I>Stateful</I>
	 values do contain objects like references or arrays. Stateful
	 data can also be pickled freely. However, pickling of stateful objects
	 has a copying semantics: each time such an object is extracted from
	 a pickle a fresh copy of the object is created. Sharing between
	 stateful object is maintained inside a pickle, though.

    <LI> <I>Sited</I>
	 objects are connected to a parent process. They refer to certain
	 resources that are not available outside the process. An example
	 is the input stream of an open file.
	 Consequently, sited objects may not be pickled.
	 Any attempt to pickle values containing sited objects will result
	 in a runtime exception.
  </UL>

  <P>
    Note that higher-order values (ie. function closures) may contain
    stateful or sited data without showing in their type. Moreover,
    functions that create sited objects are also sited. Special care
    has to be taken to avoid runtime errors from attempts of pickling
    such objects. This is particularly true when pickling modules.
  </P>

  <P>
    <A href="futures.php3">Futures</A> are never pickled. Instead, the
    pickling operation will block on all unavailable futures that are
    contained in the value to be pickled. In the case of by-need futures
    this of course will force evaluation. A failed future will cause an
    exception, respectively.
  </P>


<?php section("use", "use") ?>

  <P>
    Very similar to <A href="packages.php3">packages</A>,
    the pickling service is available through a structure
    <TT>Pickle</TT> which contains several higher-order functors:
  </P>

  <PRE>
	structure Pickle :
	sig
	    exception Sited
	    exception Corrupt

	    val extension : string

	    functor Save(val file : string signature S structure X : S) : any
	    functor Load(val file : string signature S) : S

	    (* ... *)
	end
  </PRE>

  <P>
    By applying the <TT>Save</TT> functor a module can be written as
    a pickle. For example:
  </P>

  <PRE>
	structure _ = Pickle.Save(val file = "UrlPickle." ^ Pickle.extension
				  structure X = Url
	                          signature S = URL)
  </PRE>

  <P>
    This functor application will write the structure <TT>Url</TT> into
    a file with the specified name (the string <TT>Pickle.extension</TT>
    gives the file extension usually used for pickles on the target platform).
    The result signature <TT>any</TT> of the functor indicates that it
    does not return a useful result, we thus use a wildcard in the
    declaration.
  </P>

  <P>
    Just as for the <TT>Pack</TT> operation for
    <A href="packages.php3">packages</A>, the signature passed to the
    <TT>Save</TT> functor is the most specific signature that can
    be expected when the pickle is loaded again.
    Specifying a signature that is less specific
    than the actual module type implies abstraction taking place.
  </P>

  <P>
    If the Url module contained references to any sited objects,
    the functor would raise the exception <TT>Sited</TT> to
    indicate failure (<TT>Url</TT> is not sited, however).
  </P>

  <P>
    The inverse operation to saving is loading. For example:
  </P>

  <PRE>
	structure Url' = Pickle.Load(val file = "UrlPickle." ^ Pickle.extension
	                             signature S = URL)
  </PRE>

  <P>
    If loading is successful, <TT>Url'</TT> will be bound to a structure
    with signature <TT>URL</TT>.
  </P>

  <P>
    Loading of a pickle may fail for several reasons. There may be
    an ordinary I/O error, in which case an <TT>IO.Io</TT> exeption will occur.
    The pickle format may be erroneous, which causes a <TT>Corrupt</TT>
    exception. Or the pickles's signature may not match the one passed
    to the <TT>Load</TT> functor. In that case, the exception
    <TT>Package.Mismatch</TT> will be raised.
  </P>


<?php section("values", "pickling values") ?>

  <P>
    For convenience, two short-hand functors are available for
    dealing with plain core values:
  </P>

  <PRE>
	structure Pickle :
	sig
	    (* ... *)

	    functor SaveVal(val file: string type t val x: t) : any
	    functor LoadVal(val file: string type t) : (val x: t)
	end
  </PRE>

  <P>
    Files created with the <TT>SaveVal</TT> functor are ordinary
    pickles with signature
  </P>

  <PRE>
	sig  type t  val x : t  end
  </PRE>

  <P>
    The <TT>LoadVal</TT> functor simply assumes a pickle of the same
    type and returns the contained value <TT>x</TT>.
  </P>


<?php section("sharing", "sharing") ?>

  <P>
    Pickled modules can contain abstract types. Sometimes it is
    necessary to express sharing between abstract types of different
    pickles. The way to deal with this is the usual one in SML: by
    using appropriate <TT>where</TT> specifications. For example,
    consider two data structures of some abstract type. Imagine
    that they are stored in two different pickles, both having a
    signature like:
  </P>

  <PRE>
	signature S =
	sig
	    type t
	    val x : t
	    val f : t -> int
	end
  </PRE>

  <P>
    You don't have any knowledge of the representation of type <TT>t</TT>
    - they may even be abstract - but both pickles should contain the
    same type, so that they are compatible. To load the pickles, the
    following declarations can then be used:
  </P>

  <PRE>
	structure X1 = Pickle.Load(val file = file1 signature S = S)
	structure X2 = Pickle.Load(val file = file2
				   signature S = S where type t = X1.t)
  </PRE>

  <P>
    The latter application enforces, that the second pickle
    is compatible to the first. If loading succeeds, it is guaranteed
    that this is in fact the case, otherwise a <TT>Package.Mismatch</TT>
    exception will occur. Statically, the types <TT>X1.t</TT> and <TT>X2.t</TT>
    are the same, which allows us two perform something like
  </P>

  <PRE>
	val n = X2.f(X1.x)
  </PRE>

  <P class=note>
    A current <A href="packages.php3#limitations">implementation limitation</A>
    in the treatment of runtime types of Operette 2 may lead to
    unexpected results in connection with sharing, if the signatures passed
    to the <TT>Save</TT> functor are not specified carefully.
  </P>


<?php section("components", "components") ?>

  <P>
    Pickles are closely related to <A href="components.php3">components</A>.
    In fact, a pickle is a special case of a component, and arbitrary
    components may be loaded as pickles!
  </P>

  <P>
    A pickle is an <I>evaluated</I> component (and we will use both terms
    as synonyms in the following). Such a component only
    contains the fully evaluated export, it will not be executed during
    load. This in particularly means that loading it will neither generate
    any side effects, nor produce new generative entities like types or
    exceptions.
    An evaluated component also does not import any other component.
  </P>

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
    causes the creation of new generative types. So if a non-pickle
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
