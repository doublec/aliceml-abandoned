<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1 - Threads", "threads") ?>



<?php section("overview", "overview") ?>

  <P>
    Alice provides concurrency as a fundamental language feature. The
    concurrency model is tightly coupled with
    <A href="futures.php3">futures</A>.
    The library design regarding threads is not finalized in Operette 1.
    Currently, the basic primitive is available from the structure
    <A href="futures.php3#future"><TT>Future</TT></A>:
  </P>

  <PRE>
	val concur: (unit -> 'a) -> 'a
  </PRE>

  <P>
    It immediately returns a fresh future. The procedure will be applied
    in a new thread. When the application terminates the future is replaced
    with the result.
  </P>

  <P>
    Other operations are provided through the structure
    <A href="#sig"><TT>Thread</TT></A>.
  </P>



<?php section("sig", "signature") ?>

  <PRE>
	structure Thread:
	    sig
	        type thread

		datatype state = RUNNABLE
		               | BLOCKED
		               | TERMINATED

		exception Terminate

		val thread:      (unit -> unit) -> thread
		val spawn:       (unit -> 'a) -> thread * 'a

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


<?php footing() ?>
