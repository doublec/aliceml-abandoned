<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE>Stockhausen Operette 1 - Transients</TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>

  <H1>
  stock<BR>
  hausen.<BR>
  <BR>
  -<BR>
  transients<BR>
  -
  </H1>

  <?php
    include ("menu.php3")
  ?>

  <H2>overview ___________________________</H2>
  <BR><BR>

  <P>
    One of the main novel concepts relative to SML are <I>transients</I>.
    Transients bring the basic idea of "logic variables" as found in
    logic programming languages like Oz into the functional world of ML.
  </P>

  <P>
    Transients are provided through the library structure 
    <A href="#sig"><TT>Transient</TT></A>. The structure
    <TT>Transient</TT> is opened by default, so all operations are available
    unqualified.
  </P>

  <P>
    Every transient comes with two variants: its <I>promise</I> and its
    <I>future</I>.  Programs never refer directly to a transient; rather, they
    refer to the future and the promise of a transient. When a regular
    operation accesses a future, it blocks. When a regular operation accesses a
    promise, it raises 
  </P>

  <PRE>
	exception Promise 
  </PRE>

  <P>
    The operation 
  </P>

  <PRE>
	val promise: unit -> 'a 
  </PRE>

  <P>
    creates a transient and returns its promise. The operation 
  </P>

  <PRE>
	val future: 'a -> 'a                    (* Future *) 
  </PRE>

  <P>
    expects a promise and returns its associated future. If the argument is a
    future, the operation blocks. Otherwise, it raises 
  </P>

  <PRE>
	exception Future 
  </PRE>

  <P>
    The operation 
  </P>

  <PRE>
	val fulfill: 'a * 'a -> unit             (* Fulfill *) 
  </PRE>

  <P>
    expects a promise as left argument. It globally replaces the promise and
    its associated future with the right argument, provided the left and right
    argument are not variants of the same transient. If the left argument is a
    future, <TT>fulfill</TT> blocks. Otherwise, it raises 
  </P>

  <PRE>
	exception Fulfill 
  </PRE>

  <P>
    The operation 
  </P>

  <PRE>
	val await: 'a -> 'a                      (* Promise *) 
  </PRE>

  <P>
    is the identity for all non-transient arguments. On futures <TT>await</TT>
    blocks and on promises <TT>await</TT> raises <TT>Promise</TT>.
  </P>

  <P>
    The operations 
  </P>

  <PRE>
	val isFuture:  'a -> bool 
	val isPromise: 'a -> bool 
  </PRE>

  <P>
    test whether their argument is a future or a promise, respectively. They
    do not block. 
  </P>

  <P>
    Last but not least, we have the operation 
  </P>

  <PRE>
	val byNeed: (unit -> 'a) -> 'a 
  </PRE>

  <P>
    that returns the future of a new by-need transient. As soon as a thread
    blocks on the future, the argument procedure is applied in a new thread. 
    If the application terminates with a result that is different from the
    future, the future is globally replaced with the result. If the application
    terminates with an exception <I>e</I>, the by-need transient is marked as
    failed and all operations accessing its future will raise 
  </P>

  <PRE>
	exception ByNeed of exn 
  </PRE>

  <P>
    with <I>e</I> as argument.  If the application terminates with the future
    of the by-need transient as result, the by-need transient is marked as
    failed and all operations accessing the future will raise the exception
    <TT>ByNeed(Fulfill)</TT>.
  </P>

  <P>
    Hence a by-need transient is in one of four possible states 
  </P>

  <P>
    &nbsp;&nbsp;&nbsp;&nbsp;
    passive &nbsp;&nbsp; -> &nbsp;&nbsp;
    active &nbsp;&nbsp; -> &nbsp;&nbsp;
    [succeeded, failed] 
  </P>

  <P>
    A freshly created by-need transient is passive.  It becomes active as soon
    as a thread blocks on it.  If the triggered computation terminates with a
    result that can replace the future, the by-need transient becomes
    succeeded.  If the triggered computation terminates but doesn't produce a
    result that can replace the future, the by-need transient becomes failed. 
  </P>



  <H2><A name=sig>signature __________________________</A></H2>
  <BR><BR>

  <PRE>
	structure Transient:
	    sig
		exception Promise
		exception Future
		exception Fulfill
		exception ByNeed of exn

		val promise:   unit -> 'a
		val fulfill:   'a * 'a -> unit     (* Fulfill *)

		val future:    'a -> 'a            (* Future *)
		val byNeed:    (unit -> 'a) -> 'a

		val await:     'a -> 'a            (* Promise *)

		val isPromise: 'a -> bool
		val isFuture:  'a -> bool
	    end
  </PRE>

  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       <A href="/~rossberg/">Andreas Rossberg</A>,
       <A href="/~smolka/">Gert Smolka</A> -
       last modified <?php echo date("Y/m/d") ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
