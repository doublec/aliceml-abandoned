<?php include("macros.php3"); ?>
<?php heading("The Resolver structure", "The <TT>Resolver</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature RESOLVER
    structure Resolver : RESOLVER</PRE>

  <P>
    This structure provides functionality to map symbolic resource names
    to actual files or strings.  A <I>resolver</I> encapsulates a list
    of <I>handlers</I>, which are asked in succession to provide a URL
    under which to attempt to access the resource proper.  The act of
    querying the handlers and performing the resource access is called
    a <I>localization act</I>.
  </P>

  <P>
    The current implementation knows how to access resources under URLs
    with no scheme, the <TT>file</TT> scheme, or the <TT>http</TT> scheme.
    For the latter, it uses the functions of the <TT><A href="http-client.php3"
    >HttpClient</A></TT> structure.
  </P>

  <P>
    If the environment variable <TT>ALICE_TRACE_RESOLVER</TT> is set,
    then every localization act will output trace messages about what
    handlers were called on what resource and with what result.
    These messages are printed to the standard error output stream
    <TT>TextIO.stdErr</TT>.
  </P>

  <P>
    See also:
    <A href="resolver-handler.php3"><TT>RESOLVER_HANDLER</TT></A>,
    <A href="component.php3"><TT>Component</TT></A>,
    <A href="http-client.php3"><TT>HttpClient</TT></A>
  </P>

<?php section("import", "import") ?>

  <PRE>
    import structure Resolver from "x-alice:/lib/system/Resolver"
    import signature RESOLVER from "x-alice:/lib/system/RESOLVER-sig"</PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature RESOLVER =
    sig
	structure <A href="#Handler">Handler</A> : <A href="resolver-handler.php3">RESOLVER_HANDLER</A>

	type <A href="#resolver">resolver</A>
	type <A href="#t">t</A> = resolver

	datatype <A href="#result">result</A> =
	    FILE of string
	  | STRING of string

	val <A href="#new">new</A>: string * Handler.handler list -> resolver
	val <A href="#localize">localize</A>: resolver -> string -> result option
    end</PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>structure <A name="Handler">Handler</A></TT>
    </DT>
    <DD>
      <P>defines the handlers used by resolvers.  The
	<TT><A href="resolver-handler.php3#parse">parse</A></TT>
	function uses the following syntax:  Handler specifications
	are separated by <TT><A href="config.php3#pathSeparator"
	>Config.pathSeparator</A></TT> characters.  If last handler
	specification is the equals sign (<TT>=</TT>), then the
	<TT><A href="resolver-handler.php3#default">default</A></TT>
	handler is <EM>not</EM> appended, else it is.  The following
	handler specifications are accepted:</P>
      <TABLE align=center border=1>
	<TR><TH>Syntax<TH>Specified handler
	<TR>
	  <TD><TT>root=<I>s</I></TT>
	  <TD>The handler <TT><A href="resolver-handler.php3#root">root</A
	    > <I>s</I></TT>.
	<TR>
	  <TD><TT>cache=<I>s</I></TT>
	  <TD>The handler <TT><A href="resolver-handler.php3#cache">cache</A
	    > <I>s</I></TT>.
	<TR>
	  <TD><TT>prefix=<I>s1</I>=<I>s2</I></TT>
	  <TD>The handler <TT><A href="resolver-handler.php3#prefix">prefix</A
	    > (<I>s1</I>, <I>s2</I>)</TT>.
	<TR>
	  <TD><TT>pattern=<I>s1</I>=<I>s2</I></TT>
	  <TD>The handler <TT><A href="resolver-handler.php3#prefix">pattern</A
	    > (<I>s1</I>, <I>s2</I>)</TT>.
      </TABLE>
      <P>Characters within strings <TT><I>s</I></TT>, <TT><I>s1</I></TT>,
	<TT><I>s2</I></TT> above can be escaped using the <TT><A
	href="config.php3#pathEscape">Config.pathEscape</A></TT> character,
	if applicable.</P>
    </DD>

    <DT>
      <TT>type <A name="resolver">resolver</A></TT><BR>
      <TT>type <A name="t">t</A> = resolver</TT>
    </DT>
    <DD>
      <P>The type of resolvers.</P>
    </DD>

    <DT>
      <TT>datatype <A name="result">result</A> = FILE of string
	| STRING of string</TT>
    </DT>
    <DD>
      <P>The type of results of localization acts.  If localization
	of a resource succeeded and found a local file with name&nbsp;<TT><I
	>s</I></TT>, the result of the localization act is <TT>FILE&nbsp;<I
	>s</I></TT>; if the resource was found on a remote address, the
	resource is downloaded into a string&nbsp;<TT><I>s</I></TT> and the
	result of the localization act is <TT>STRING&nbsp;<I>s</I></TT>.</P>
    </DD>

    <DT>
      <TT><A name="new">new</A> (<I>name</I>, <I>handlers</I>)</TT>
    </DT>
    <DD>
      <P>returns a new resolver with name <TT><I>name</I></TT> and trying the
	given <TT><I>handlers</I></TT> in order.  The name is used to print
	tracing messages.</P>
    </DD>

    <DT>
      <TT><A name="localize">localize</A> <I>resolver</I> <I>name</I></TT>
    </DT>
    <DD>
      <P>performs a localization act, using <TT><I>resolver</I></TT>, to
	localize the resource with symbolic name <TT><I>name</I></TT>.
	Returns <TT>SOME _</TT>, if localization was successful, <TT>NONE</TT>
	otherwise.</P>
    </DD>
  </DL>

<?php footing() ?>
