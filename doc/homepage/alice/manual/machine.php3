<?php include("macros.php3"); ?>
<?php heading("Virtual machine", "virtual\nmachine") ?>


<?php section("overview", "overview") ?>

<P>The virtual machine is required to execute Alice applications. It takes the
URL of the application's <A href="components.php3">root component</A> and
evaluates it.</P>


<?php section("synopsis", "synopsis") ?>

<DL>
  <DT><TT>alicerun</TT> <I>&lt;url&gt;</I>
    <I>&lt;args&gt;</I> ...</DT>
</DL>


<?php section("description", "description") ?>

<P>Executes the application given by <I>&lt;url&gt;</I>, denoting a compiled
component. The component is located, loaded and evaluated using the root <A
href="library/component-manager.php3">component manager</A>. The component may
be a Mozart component (<A
href="http://www.mozart-oz.org/documentation/apptut/node3.html">Mozart
"functor"</A>).</P>

<P>The application can access the remaining command line arguments via the <A
href="library/command-line.php3"><TT>CommandLine</TT></A> library structure. 
To terminate an application, the <TT>OS.Process.terminate</TT> function must be
invoked. The virtual machine will <EM>not</EM> be terminated automatically,
even if no live threads are left.</P>


<?php footing() ?>
