<?php include("macros.php3"); ?>

<?php heading("Futures", "futures") ?>



<?php section("overview", "overview") ?>

  <P>
    One of the main novel concepts relative to SML are <I>futures</I>.
    Futures in Alice bring the basic idea of "logic variables" as found in
    logic programming languages into the typed functional world of ML.
  </P>



<?php section("future", "futures") ?>

  <P>
    Futures are provided through the library structure <TT>Future</TT>:
  </P>

  <PRE>
	structure Future :
	sig
	    exception Future of exn
	    exception Cyclic

	    val concur :	(unit -> 'a) -> 'a
	    val byneed :	(unit -> 'a) -> 'a
	    val alarm :		Time.time -> unit

	    val await :		'a -> 'a
	    val awaitOne :	'a * 'b -> 'a

	    val isFuture :	'a -> bool
	    val isFailed :	'a -> bool
	end
  </PRE>

  <P>
    This structure provides three basic primitives to create futures. The
    operation
  </P>

  <PRE>
	val concur : (unit -> 'a) -> 'a 
  </PRE>

  <P>
    applies the procedure in a new thread. It immediately returns a future
    associated with that thread. When the procedure terminates with a result
    that is different from the future, this future is globally replaced with
    the result. If the application terminates with an exception <I>e</I>, the
    future is marked as failed and all operations accessing it will
    raise <TT>Future(</TT><I>e</I><TT>)</TT>. If the application terminates
    returning the future itself, the future is marked as failed and all
    operations accessing it will raise <TT>Future(Cyclic)</TT>.
  <P>

  <P>
    The operation
  </P>

  <PRE>
	val byneed : (unit -> 'a) -> 'a 
  </PRE>

  <P>
    returns a by-need future. As soon as a thread blocks on the future, the
    argument procedure is applied in a new thread. Evaluation proceeds similar
    to <TT>concur</TT>. By-need futures can be used for
    <A href="laziness.php3">lazy evaluation</A>.
  </P>

  <P>
    Both these operations are in the top-level environment and can thus be
    used unqualified.
  </P>

  <P>
    Finally, the operation
  </P>

  <PRE>
	val alarm : Time.time -> unit
  </PRE>

  <P>
    creates a future that will be replaced by the value <TT>()</TT> after the
    given period of time (see structure
    <A href="http://www.dina.kvl.dk/~sestoft/sml/time.html"><TT>Time</TT></A>).
    This is useful for programming timeouts and the like.
  </P>

  <P>
    The operation 
  </P>

  <PRE>
	val await : 'a -> 'a
  </PRE>

  <P>
    is the identity for all non-future arguments. On futures <TT>await</TT>
    blocks until the future is replaced by a proper value. Similarly,
  </P>

  <PRE>
	val awaitOne : 'a * 'b -> 'a
  </PRE>

  <P>
    blocks until at least one if its arguments is a proper value. It then
    returns the first argument.
  </P>

  <P>
    Every future is in one of three possible states:
  </P>

  <UL>
    <LI> undetermined </LI>
    <LI> succeded </LI>
    <LI> failed </LI>
  </UL>

  <P>
    A freshly created future is undetermined. When it is replaced by a proper
    value it becomes succeeded. If the computation associated with a future
    created by <TT>concur</TT> or <TT>byneed</TT> is terminated with an
    exception, the future becomes failed.
  </P>

  <P>
    The operation
  </P>

  <PRE>
	val isFuture : 'a -> bool
  </PRE>

  <P>
    tests whether its argument is an undetermined future. It does not block.
    Similarly,
  </P>

  <PRE>
	val isFailed : 'a -> bool
  </PRE>

  <P>
    tests whether its argument is a failed future.
  </P>

  <P class=note>
    Note: In the current system, the operation <TT>isFuture</TT> does return
    <TT>true</TT> for failed futures, due to limitations
    of the Mozart virtual machine. Similarly, <TT>isFailed</TT> will always
    deliver <TT>false</TT>.
  </P>




<?php section("promise", "promises") ?>

  <P>
    A forth form of future is introduced by the structure <TT>Promise</TT>:
  </P>

  <PRE>
	structure Promise :
	sig
	    type 'a promise

	    exception Promise

	    val promise :	unit -> 'a promise
	    val future :	'a promise -> 'a

	    val fulfill :	'a promise * 'a  -> unit
	    val fail :		'a promise * exn -> unit
	end
  </PRE>

  <P>
    A promise is a handle for a future. The operation
  </P>

  <PRE>
	val promise : unit -> 'a promise
  </PRE>

  <P>
    creates a promise and thereby a fresh future. The operation 
  </P>

  <PRE>
	val future : 'a promise -> 'a
  </PRE>

  <P>
    returns the future associated with the promise. This future can become
    succeeded by applying
  </P>

  <PRE>
	val fulfill : 'a promise * 'a -> unit
  </PRE>

  <P>
    to the promise. It globally replaces the future with the right argument,
    provided it is not the future itself.  If the promise has already been
    fulfilled (or failed, see below), the exception <TT>Promise</TT> is
    raised. If the right arguments is the future associated with the promise,
    the exception <TT>Cyclic</TT> is raised.
  </P>

  <P>
    Dually, a promise and its future can be explicitly failed by applying
  </P>

  <PRE>
	val fail : 'a promise * exn -> unit
  </PRE>

  <P>
    to the promise. If a promise is failed with exception <I>e</I>, any
    subsequent attempt to access its future will cause the exception
    <TT>Future(</TT><I>e</I><TT>)</TT> to be raised. If the promise already
    had been fulfilled or failed, <TT>fail</TT> will raise the exception
    <TT>Promise</TT>.
  </P>


<?php footing() ?>
