<?php include("macros.php3"); ?>
<?php heading("The HttpClient structure",
	      "The <TT>HttpClient</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature HTTP_CLIENT
    structure HttpClient : HTTP_CLIENT
  </PRE>

  <P>
    This structure implements a simple client for the HTTP protocol
    as specified in RFC&nbsp;2616.  Connections are only created and
    maintained open for issuing a single request and reading a single
    response.
  </P>

  <P>
    Where a request is constructed, the current implementation inserts
    a <TT>User-Agent</TT> of <TT>Stockhausen/1.0</TT> and uses protocol
    version 1.1.
  </P>

<?php section("import", "import") ?>

  <PRE>
    import signature HTTP_CLIENT from "x-alice:/lib/system/HTTP_CLIENT-sig"
    import structure HttpClient from "x-alice:/lib/system/HttpClient"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature HTTP_CLIENT =
    sig
	type <A href="#document">document</A> = {contentType : string, body : string}

	exception <A href="#Authority">Authority</A>

	val <A href="#request">request</A> : Url.t * Http.request -> Http.response
	val <A href="#get">get</A> : Url.t -> Http.response
	val <A href="#post">post</A> : Url.t * document -> Http.response
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type <A name="document">document</A> =
	{contentType : string, body : string}</TT>
    </DT>
    <DD>
      <P>The type of documents as provided in a <TT>POST</TT> request.</P>
    </DD>

    <DT>
      <TT>exception <A name="Authority">Authority</A></TT>
    </DT>
    <DD>
      <P>indicates that a given URL did either not contain an authority
	or that it was not well-formed (for instance, a port number was
	supplied, but it was no valid integer).</P>
    </DD>

    <DT>
      <TT><A name="request">request</A> (<I>url</I>, <I>request</I>)</TT>
    </DT>
    <DD>
      <P>establishes a connection to the server specified in <I>url</I>,
	issues the <I>request</I>, and returns the response.  Closes the
	connection immediately after reading the response.  Raises
	<TT><A href="#Authority">Authority</A></TT> if <I>url</I> does
	not specify a well-formed authority.</P>
    </DD>

    <DT>
      <TT><A name="get">get</A> <I>url</I></TT>
    </DT>
    <DD>
      <P>establishes a connection to the server specified in <I>url</I>,
	issues a <TT>GET</TT> request, and returns the response.  Closes
	the connection immediately after reading the response.  Raises
	<TT><A href="#Authority">Authority</A></TT> if <I>url</I> does
	not specify a well-formed authority.</P>
    </DD>

    <DT>
      <TT><A name="post">post</A> (<I>url</I>, <I>doc</I>)</TT>
    </DT>
    <DD>
      <P>establishes a connection to the server specified in <I>url</I>,
	issues a <TT>POST</TT> request with <I>doc</I>, and returns the
	response.  Closes the connection immediately after reading the
	response.  Raises <TT><A href="#Authority">Authority</A></TT>
	if <I>url</I> does not specify a well-formed authority.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="url.php3"><TT>Url</TT></A>,
    <A href="http.php3"><TT>Http</TT></A>,
    <A href="http-server.php3"><TT>HttpServer</TT></A>,
    <A href="resolver.php3"><TT>Resolver</TT></A>
  </DD></DL>

<?php footing() ?>
