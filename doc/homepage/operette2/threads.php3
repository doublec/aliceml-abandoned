<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE>Stockhausen Operette 1 - Threads</TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>

  <H1>
  stock<BR>
  hausen.<BR>
  <BR>
  -<BR>
  threads<BR>
  -
  </H1>

  <?php
    include ("menu.php3")
  ?>

  <H2>overview ___________________________</H2>
  <BR><BR>

  <P>
    Alice provides concurrency as a fundamental language feature. The
    concurrency model is tightly coupled with
    <A href="transients.php3">transients</A>.
    The library design regarding threads is not finalized in Operette 1.
    Currently, the basic primitive is:
  </P>

  <PRE>
	val spawn: (unit -> 'a) -> 'a
  </PRE>

  <P>
    It immediately returns a fresh future. The procedure will be applied
    in a new thread. When the application terminates the future is replaced
    with the result.
<!--
    If the application
    terminates with an exception <I>e</I>, the by-need transient is marked as
    failed and all operations accessing its future will raise 
  </P>

  <PRE>
	exception Aborted of exn
  </PRE>

  <P>
    with <I>e</I> as argument.  If the application terminates with the future
    of the by-need transient as result, the by-need transient is marked as
    failed and all operations accessing the future will raise the exception
    <TT>ByNeed(Fulfill)</TT>.
    Note that <TT>spawn</TT> behaves very similar to <TT>byNeed</TT>.
-->
  </P>

  <P>
    Other operations are provided through the structure
    <A href="#sig"><TT>Thread</TT></A>.
  </P>


  <H2><A name=sig>signature __________________________</A></H2>
  <BR><BR>

  <PRE>
	structure Thread:
	    sig
	        type thread

		datatype state =
		    RUNNABLE
		  | BLOCKED
		  | TERMINATED

		exception Terminate

		val spawn:       (unit -> 'a) -> 'a

		val current:     unit -> thread
		val state:       thread -> state

		val yield:       thread -> unit
		val sleep:       int -> unit

		val raiseIn:     thread * exn -> unit
		val terminate:   thread -> unit

		val suspend:     thread -> unit
		val resume:      thread -> unit
		val isSuspended: thread -> bool
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
