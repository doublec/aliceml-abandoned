<?php include("macros.php3"); ?>
<?php heading("The RESOLVER_HANDLER signature",
	      "The <TT>RESOLVER_HANDLER</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature RESOLVER_HANDLER</PRE>

  <P>
    This signature represents handlers as used by <A href="resolver.php3"
    >resolvers</A> to find resources such as components.  A <EM>handler</EM>
    is a function that takes a symbolic name for a resource and returns a
    URL with which to attempt to actually locate the resource.  A handler
    may or may not be <EM>applicable</EM>, which means that it actually
    may or may not return a URL.
  </P>

  <P>
    See also:
    <A href="resolver.php3"><TT>Resolver</TT></A>,
    <A href="url.php3"><TT>Url</TT></A>
  </P>

<?php section("import", "import") ?>

  <PRE>
    import signature RESOLVER_HANDLER from "x-alice:/lib/system/RESOLVER_HANDLER-sig"</PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature RESOLVER_HANDLER =
    sig
	type <A href="#handler">handler</A>
	type <A href="#t">t</A> = handler

	exception <A href="#Syntax">Syntax</A>

	val <A href="#default">default</A> : handler
	val <A href="#root">root</A> : string -> handler
	val <A href="#cache">cache</A> : string -> handler
	val <A href="#prefix">prefix</A> : string * string -> handler
	val <A href="#pattern">pattern</A> : string * string -> handler
	val <A href="#custom">custom</A> : string * (string -> <A href="url.php3#t">Url.t</A> option) -> handler

	val <A href="#parse">parse</A> : string -> handler list
    end</PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type <A name="handler">handler</A></TT><BR>
      <TT>type <A name="t">t</A> = handler</TT>
    </DT>
    <DD>
      <P>The type of handlers.</P>
    </DD>

    <DT>
      <TT>exception <A name="Syntax">Syntax</A></TT>
    </DT>
    <DD>
      <P>used by <TT><A href="#parse">parse</A></TT> to indicate that a string
	was not a well-formed representation of a list of handlers.</P>
    </DD>

    <DT>
      <TT><A name="default">default</A></TT>
    </DT>
    <DD>
      <P>is the handler that parses the resource name as a URL and
	returns that.  It causes an attempt to locate the resource
	directly under its symbolic name.  Only applicable for resources
	that represent a URL.</P>
    </DD>

    <DT>
      <TT><A name="root">root</A> <I>s</I></TT>
    </DT>
    <DD>
      <P>returns a handler that causes an attempt to locate the resource
	below a specific root path&nbsp;<TT><I>s</I></TT>.  Only applicable
	for resource names that represent a relative path name.</P>
    </DD>

    <DT>
      <TT><A name="cache">cache</A> <I>s</I></TT>
    </DT>
    <DD>
      <P>returns a handler that causes an attempt to locate the resource
	within a cache-style path structure.  Only applicable for resource
	names that represent a URL.  The constructed path is of the form</P>
      <PRE><I>s</I>/<I>scheme</I>/<I>authority</I>/<I>device</I>/<I>path</I
      ></PRE>
      <P>where path components for absent constituents are left out.</P>
    </DD>

    <DT>
      <TT><A name="prefix">prefix</A> (<I>s1</I>, <I>s2</I>)</TT>
    </DT>
    <DD>
      <P>returns a handler that causes an attempt to locate the resource
	under prefix replacement.  Only applicable for resource names
	that start with prefix <TT><I>s1</I></TT>.  The prefix is replaced
	by <TT><I>s2</I></TT> and the result is parsed as a URL.</P>
    </DD>

    <DT>
      <TT><A name="pattern">pattern</A> (<I>s1</I>, <I>s2</I>)</TT>
    </DT>
    <DD>
      <P>returns a handler that causes an attempt to locate the resource
	under pattern replacement.  <TT><I>s1</I></TT> is a pattern that
	contains variables of the form <TT>?{<I>x</I>}</TT>, where <TT><I
	>x</I></TT> is a string not containing a right brace character.
	Only applicable for resource names that match <TT><I>s1</I></TT>.
	Variables given in the pattern are bound to corresponding substrings
	of the resource name.  <TT><I>s2</I></TT> is returned, with
	occurrences of variables replaced by the substrings they are bound
	to, parsed as a URL.  Raises <TT><A href="#Syntax">Syntax</A></TT>
	if either <TT><I>s1</I></TT> or <TT><I>s2</I></TT> contain a
	non-terminated variable.</P>
    </DD>

    <DT>
      <TT><A name="parse">parse</A> <I>s</I></TT>
    </DT>
    <DD>
      <P>interprets <TT><I>s</I></TT> as a string representation for a list of
	handlers and returns this.  Raises <TT><A href="#Syntax">Syntax</A
	></TT> if the string is not well-formed.  The concrete syntax is
	specified by implementations of this signature.</P>
    </DD>
  </DL>

<?php footing() ?>
