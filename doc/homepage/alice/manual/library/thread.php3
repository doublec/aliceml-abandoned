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
    the thread becomes <TT>TERMINATED</TT>.
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
	type <A href="#thread">thread</A>
	type <A href="#t">t</A> = thread
	datatype <A href="#state">state</A> = RUNNABLE | BLOCKED | TERMINATED

	exception <A href="#Terminate-exn">Terminate</A>
	exception <A href="#Terminated">Terminated</A>

	val <A href="#thread">thread</A> :		(unit -> unit) -> thread
	val <A href="#spawnThread">spawnThread</A> :	(unit -> 'a) -> thread * 'a

	val <A href="#current">current</A> :		unit -> thread
	val <A href="#state">state</A> :		thread -> state

	val <A href="#yield">yield</A> :		thread -> unit
	val <A href="#sleep">sleep</A> :		Time.time -> unit

	val <A href="#raiseIn">raiseIn</A> :		thread * exn -> unit
	val <A href="#terminate">terminate</A> :		thread -> unit

	val <A href="#suspend">suspend</A> :		thread -> unit
	val <A href="#resume">resume</A> :		thread -> unit
	val <A href="#isSuspended">isSuspended</A> :	thread -> bool
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type <A name="thread">thread</A></TT><BR>
      <TT>type <A name="t">t</A> = thread</TT>
    </DT>
    <DD>
      <P>The type of first-class threads.  A reference to a first-class
	thread can be used to observe and control execution of a
	concurrent computation.</P>
    </DD>

    <DT>
      <TT>datatype <A name="state">state</A> = RUNNABLE | BLOCKED | TERMINATED</TT>
    </DT>
    <DD>
      <P>The type of thread states.</P>
    </DD>

    <DT>
      <TT>exception <A name="Terminate-exn">Terminate</A></TT>
    </DT>
    <DD>
      <P>This exception is raised by <TT><A href="#terminate">terminate</A
	></TT> to terminate a thread.  Should never be raised explicitly.</P>
    </DD>

    <DT>
      <TT>exception <A name="Terminated">Terminated</A></TT>
    </DT>
    <DD>
      <P>indicates that a thread control operation was applied to
	a terminated thread.  Should only be caught to perform cleanup
	actions, and should always be re-raised.</P>
    </DD>

    <DT>
      <TT><A name="thread">thread</A> <I>f</I></TT>
    </DT>
    <DD>
      <P>spawns a new thread <I>thr</I> which computes <I>f</I>&nbsp;().
	Returns <I>thr</I>.</P>
    </DD>

    <DT>
      <TT><A name="spawnThread">spawnThread</A> <I>f</I></TT>
    </DT>
    <DD>
      <P>spawns a new thread <I>thr</I> which computes <I>f</I>&nbsp;().
	Returns a pair of <I>thr</I> and the result of applying&nbsp;<I>f</I
	>, which is a future which will be bound to the result of
	<I>f</I>&nbsp;().</P>
    </DD>

    <DT>
      <TT><A name="current">current</A> ()</TT>
    </DT>
    <DD>
      <P>returns the calling thread, that is, the thread
	executing <TT>current&nbsp;()</TT>.</P>
    </DD>

    <DT>
      <TT><A name="state">state</A> <I>thr</I></TT>
    </DT>
    <DD>
      <P>returns the current state of&nbsp;<I>thr</I>.</P>
    </DD>

    <DT>
      <TT><A name="yield">yield</A> <I>thr</I></TT>
    </DT>
    <DD>
      <P>causes the scheduler to stop executing thread&nbsp;<I>thr</I>,
	if it is currently being executed, and select another thread
	for execution.  Has no effect if <I>thr</I> is not currently being
	executed.</P>
    </DD>

    <DT>
      <TT><A name="sleep">sleep</A> <I>t</I></TT>
    </DT>
    <DD>
      <P>causes the calling thread to stop executing and not be rescheduled
	for the time specified by&nbsp;<I>t</I>.  If <I>t</I> is zero or
	negative, immediately returns.  This is equivalent to</P>
      <PRE><A href="future.php3#await">Future.await</A
	> (<A href="future.php3#alarm">Future.alarm</A> t)</PRE>
    </DD>

    <DT>
      <TT><A name="raiseIn">raiseIn</A> (<I>thr</I>, <I>ex</I>)</TT>
    </DT>
    <DD>
      <P>raises the exception <I>ex</I> in thread <I>thr</I>.
	If <I>thr</I> is terminated, instead raises <TT><A href="#Terminated"
	>Terminated</A></TT> in the calling thread.  If <I>thr</I> is blocked,
	makes <I>thr</I> runnable again.</P>
    </DD>

    <DT>
      <TT><A name="terminate">terminate</A> <I>thr</I></TT>
    </DT>
    <DD>
      <P>attempts to terminate <I>thr</I> by raising exception
	<TT><A href="#Terminated">Terminated</A></TT> in it.  Can be
	implemented as</P>
      <PRE><A href="#raiseIn">raiseIn</A> (<I>thr</I>, <A href="#Terminated"
	>Terminated</A>)</PRE>
    </DD>

    <DT>
      <TT><A name="suspend">suspend</A> <I>thr</I></TT>
    </DT>
    <DD>
      <P>suspends <I>thr</I>.  If <I>thr</I> is being executed, then
	it yields.  <I>thr</I> is not rescheduled until resumed again.
	Note that this does not change the status of <I>thr</I>,
	that is, <I>thr</I> can be suspended <EM>and</EM> at the same
	time be runnable, blocked, or terminated.</P>
    </DD>

    <DT>
      <TT><A name="resume">resume</A> <I>thr</I></TT>
    </DT>
    <DD>
      <P>resumes <I>thr</I>.  If <I>thr</I> is runnable, makes <I>thr</I>
	available for scheduling again.  Does not change the status of
	<I>thr</I>.</P>
    </DD>

    <DT>
      <TT><A name="isSuspended">isSuspended</A> <I>thr</I></TT>
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
