<?php include("macros.php3"); ?>

<?php heading("Threads", "threads") ?>



<?php section("overview", "overview") ?>

  <P>
    Alice provides concurrency as a fundamental language feature. The
    concurrency model is tightly coupled with
    <A href="futures.php3">futures</A>.
    The library design regarding threads is not finalized yet.
    Currently, the basic primitive is available from the structure
    <A href="futures.php3#future"><TT>Future</TT></A>:
  </P>

  <PRE>
	val concur : (unit -> 'a) -> 'a
  </PRE>

  <P>
    It immediately returns a fresh future. The procedure will be applied
    in a new thread. When the application terminates the future is replaced
    with the result.
  </P>

  <P>
    Other operations on threads are provided through the structure
    <A href="#sig"><TT>Thread</TT></A>.
  </P>


<?php section("state", "state") ?>

  <P>
    To deal with state in a thread-safe way, the structure <TT>Ref</TT>
    provides an atomic exchange operation for references:
  </P>

  <PRE>
	val exchange : 'a ref * 'a -> 'a
  </PRE>

  <P>
    With <TT>exchange</TT>, the content of a cell can be
    replaced by a new value. The old value is returned. The <TT>exchange</TT>
    operation is atomic, and can thus be used for synchronisation.
    As an example, here is the implementation of a generic lock generator:
  </P>

  <PRE>
	(* mkLock : unit -> ('a -> 'b) -> ('a -> 'b) *)

	fun mkLock() =
	    let
		val r = Ref.ref()
	    in
		fn f => fn x =>
		let
		    val new = Promise.promise()
		    val old = Ref.exchange(r, Promise.future new)
		in
		    await old;
		    f x before
		    Promise.fulfill(new, ()))
		end
	    end
  </PRE>


<?php section("sig", "signature") ?>

  <PRE>
	structure Thread :
	sig
	    type thread

	    datatype state = RUNNABLE
		           | BLOCKED
		           | TERMINATED

	    exception Terminate

	    val thread :      (unit -> unit) -> thread
	    val spawn :       (unit -> 'a) -> thread * 'a

	    val current :     unit -> thread
	    val state :       thread -> state

	    val yield :       thread -> unit
	    val sleep :       int -> unit

	    val raiseIn :     thread * exn -> unit
	    val terminate :   thread -> unit

	    val suspend :     thread -> unit
	    val resume :      thread -> unit
	    val isSuspended : thread -> bool
	end
  </PRE>


<?php footing() ?>
