<?php include("macros.php3"); ?>
<?php heading("The Component structure",
	      "The <TT>Component</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature COMPONENT
    structure Component : COMPONENT
  </PRE>

  <P>
    This structure provides components as first-class entities and
    operations on them.  We speak of unevaluated and evaluated components.
    An <EM>unevaluated</EM> component is a component which has not yet been
    linked and applied, that is, its declarations have not been executed.
    An <EM>evaluated</EM> component is similar to a package: it has empty
    import announcements, cannot be applied to produce side-effects, and
    as such is just a pair of a structure and its (export) signature.
  </P>

  <P>
    The linking and evaluation of components takes place in <EM>component
    managers</EM>.
  </P>

<?php section("import", "import") ?>

  <PRE>
    import signature COMPONENT from "x-alice:/lib/system/COMPONENT-sig"
    import structure Component from "x-alice:/lib/system/Component"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature COMPONENT =
    sig
	type component
	type t = component

	exception Sited
	exception Corrupt

	val extension: string

	functor Create(signature S  structure X : S) :
	    sig  val component : component  end

	val load: Url.t -> component
	val save: string * component -> unit
	val inf: component -> Inf.t option

	functor MkManager() : COMPONENT_MANAGER where type component = component
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type component</TT><BR>
      <TT>type t = component</TT>
    </DT>
    <DD>
      <P>The type of first-class components.</P>
    </DD>

    <DT>
      <TT>exception Sited</TT>
    </DT>
    <DD>
      <P>used by the <TT>save</TT> operation to indicate that a first-class
	component contains sited data structures.  This exception is never
	raised directly; it only appears as the <TT>cause</TT> of an
	<TT>IO.Io</TT> exception.</P>
    </DD>

    <DT>
      <TT>exception Corrupt</TT>
    </DT>
    <DD>
      <P>used by the <TT>load</TT> operation to indicate that the contents
	of a file did not represent a well-formed pickled component.
	This exception is never raised directly; it only appears as the
	<TT>cause</TT> of an <TT>IO.Io</TT> exception.</P>
    </DD>

    <DT>
      <TT>extension</TT>
    </DT>
    <DD>
      <P>is the string used on the current platform as extension part to
	name files containing pickled components.  This does not include
	the period commonly used to separate file names' base and extension
	parts.</P>
    </DD>

    <DT>
      <TT>Create(signature S = <I>S</I>  structure X = <I>X</I>)</TT>
    </DT>
    <DD>
      <P>returns an evaluated component (as <TT>val component</TT> of the
	resulting structure) representing structure&nbsp;<I>X</I> with export
	signature&nbsp;<I>S</I>.</P>
    </DD>

    <DT>
      <TT>load <I>url</I></TT>
    </DT>
    <DD>
      <P>localizes <I>url</I> using the <A href="resolver.php3">resolver</A>
	initialized from the <TT>ALICE_LOAD</TT> environment variable and
	attempts to unpickle a first-class component from the file found,
	which it returns upon success.  Raises <TT>IO.Io</TT> if resolving,
	loading or unpickling fails.  If resolving fails, the <TT>cause</TT>
	is <TT>Option</TT>.</P>
    </DD>

    <DT>
      <TT>save (<I>s</I>, <I>comp</I>)</TT>
    </DT>
    <DD>
      <P>pickles <I>comp</I> and saves it to a new file with
	name&nbsp;<I>s</I>.  Raises <TT>IO.Io</TT> is pickling or saving
	fails.</P>
    </DD>

    <DT>
      <TT>inf <I>comp</I></TT>
    </DT>
    <DD>
      <P>retrieves the export signature from <I>comp</I>.  May return
	<TT>NONE</TT> if the component represented by <I>comp</I> has
	no explicit export signature, as may be the case for a native
	or foreign component (that is, a component not implemented in
	Alice).</P>
    </DD>

    <DT>
      <TT>MkManager()</TT>
    </DT>
    <DD>
      <P>returns a new component manager with a component table empty but
	for the virtual machine's built-in components and those components
	that had to be loaded to initialize the system's boot component
	manager.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="component-manager.php3"><TT>COMPONENT_MANAGER</TT></A>,
    <A href="resolver.php3"><TT>Resolver</TT></A>
  </DD></DL>

<?php footing() ?>
