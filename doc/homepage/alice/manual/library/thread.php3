<?php include("macros.php3"); ?>
<?php heading("The Thread structure", "The <TT>Thread</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature THREAD
    structure Thread : THREAD
  </PRE>

  <P>
    The <TT>Thread</TT> structure provides access to first-class threads.
    A thread encapsulates a concurrent computation.  The <TT>concur</TT>
    keyword starts a new thread, as does triggering a by-need future.
    A thread is initially created in state <TT>RUNNABLE</TT>.  The
    runnable threads are scheduled in a round-robin fashion.  When a
    computation requests the value of a future, the thread becomes
    <TT>BLOCKED</TT> until the future is bound, whereafter it becomes
    <TT>RUNNABLE</TT> again.  When the computation has been fully
    performed (or an exception is raised for which there is no handler),
    the thread becomes <TT>TERMINATED</TT>; we say that the thread has
    <EM>died</EM>.
  </P>

  <P>
    The functions in this structure allow to observe a thread's status,
    raise an exception in a thread, and to explicitly suspend and
    resume threads.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature THREAD =
    sig
	type thread
	type t = thread
	datatype state = RUNNABLE | BLOCKED | TERMINATED

	exception Terminate
	exception Terminated

	val thread :		(unit -> unit) -> thread
	val spawnThread :	(unit -> 'a) -> thread * 'a

	val current :		unit -> thread
	val state :		thread -> state

	val yield :		thread -> unit
	val sleep :		Time.time -> unit

	val raiseIn :		thread * exn -> unit
	val terminate :		thread -> unit

	val suspend :		thread -> unit
	val resume :		thread -> unit
	val isSuspended :	thread -> bool
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type thread</TT><BR>
      <TT>type t = thread</TT>
    </DT>
    <DD>
      <P>The type of first-class threads.  A reference to a first-class
	thread can be used to observe and control execution of a
	concurrent computation.</P>
    </DD>

    <DT>
      <TT>datatype state = RUNNABLE | BLOCKED | TERMINATED</TT>
    </DT>
    <DD>
      <P>The type of thread states.</P>
    </DD>

    <DT>
      <TT>exception Terminate</TT>
    </DT>
    <DD>
      <P>This exception is raised by the <TT>terminate</TT> function
	to terminate a thread.  Should never be raised explicitly.</P>
    </DD>

    <DT>
      <TT>exception Terminated</TT>
    </DT>
    <DD>
      <P>indicates that a thread control operation was applied to
	a dead thread.  Should only be caught to perform cleanup
	actions, and should always be re-raised.</P>
    </DD>

    <DT>
      <TT>thread <I>f</I></TT>
    </DT>
    <DD>
      <P>spawns a new thread <I>thr</I> which computes <I>f</I>&nbsp;().
	Returns <I>thr</I>.</P>
    </DD>

    <DT>
      <TT>spawnThread <I>f</I></TT>
    </DT>
    <DD>
      <P>spawns a new thread <I>thr</I> which computes <I>f</I>&nbsp;().
	Returns a pair of <I>thr</I> and the result of applying&nbsp;<I>f</I
	>, which is a future bound when <I>f</I> returns.</P>
    </DD>

    <DT>
      <TT>current ()</TT>
    </DT>
    <DD>
      <P>returns the calling thread, that is, the thread
	executing <TT>current&nbsp;()</TT>.</P>
    </DD>

    <DT>
      <TT>state <I>thr</I></TT>
    </DT>
    <DD>
      <P>returns the current state of&nbsp;<I>thr</I>.</P>
    </DD>

    <DT>
      <TT>yield <I>thr</I></TT>
    </DT>
    <DD>
      <P>causes the scheduler to stop executing thread&nbsp;<I>thr</I>,
	if it is currently being executed, and select another thread
	for execution.  Has no effect if <I>thr</I> is not currently being
	executed.</P>
    </DD>

    <DT>
      <TT>sleep <I>t</I></TT>
    </DT>
    <DD>
      <P>causes the calling thread to stop executing and not be rescheduled
	for the time specified by&nbsp;<I>t</I>.  If <I>t</I> is zero or
	negative, immediately returns.  This is equivalent to</P>
      <PRE>Future.await (Future.alarm t)</PRE>
    </DD>

    <DT>
      <TT>raiseIn (<I>thr</I>, <I>ex</I>)</TT>
    </DT>
    <DD>
      <P>raises the exception <I>ex</I> in thread <I>thr</I>.
	If <I>thr</I> is terminated, instead raises <TT>Terminated</TT>
	in the calling thread.  If <I>thr</I> is blocked, makes <I>thr</I>
	runnable again.</P>
    </DD>

    <DT>
      <TT>terminate <I>thr</I></TT>
    </DT>
    <DD>
      <P>attempts to terminate <I>thr</I> by raising exception
	<TT>Terminated</TT> in it.  Can be implemented as</P>
      <PRE>raiseIn (<I>thr</I>, Terminated)</PRE>
    </DD>

    <DT>
      <TT>suspend <I>thr</I></TT>
    </DT>
    <DD>
      <P>suspends <I>thr</I>.  If <I>thr</I> is being executed, then
	it yields.  <I>thr</I> is not rescheduled until resumed again.
	Note that this does not change the status of <I>thr</I>,
	that is, <I>thr</I> can be suspended <EM>and</EM> at the same
	time be runnable, blocked, or terminated.</P>
    </DD>

    <DT>
      <TT>resume <I>thr</I></TT>
    </DT>
    <DD>
      <P>resumes <I>thr</I>.  If <I>thr</I> is runnable, makes <I>thr</I>
	available for scheduling again.  Does not change the status of
	<I>thr</I>.</P>
    </DD>

    <DT>
      <TT>isSuspended <I>thr</I></TT>
    </DT>
    <DD>
      <P>returns <TT>true</TT> if <I>thr</I> is suspended, <TT>false</TT>
	otherwise.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="future.php3"><TT>Future</TT></A>
  </DD></DL>

<?php footing() ?>
