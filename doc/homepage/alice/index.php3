<?php
  include("/services/ps/httpd/html/pslab.php");

  pslab_bibheader("Alice");

  $baseurl = "/alice";

  $suburls =
    array(array("text" => "Download",
		"url"  => "download.php3"),
	  array("text" => "Manual",
		"url"  => "manual/"),
	  array("text" => "Bugs",
		"url"  => "bugzilla/"),
	  array("text" => "Papers",
		"url"  => "papers.php3"),
	  array("text" => "People and Contact",
		"url"  => "people.php3"));
?>

<IMG align=right src="alice.gif">

<P class=margin>
  <?php pslab_uni(); ?>
  <?php pslab_menu(); ?>
  <BR><BR>
  <A href="http://www.coli.uni-sb.de/sfb378/"
    ><IMG src="/images/sfb378.gif" border=0 vspace=8></A>
</P>

<H2>Overview</H2>

  <P>
    <I>Alice</I> is a functional programming language based on
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>,
    extended with support for concurrent, distributed,
    and constraint programming. Alice extends Standard ML with
    several new features:
  </P>

  <UL>
    <LI> <A href="manual/laziness.php3"><I>Laziness</I></A>:
	 combining strict and transparent lazy functional programming
    <LI> <A href="manual/futures.php3"><I>Futures</I></A>:
	 "logic variables" and concurrency
    <LI> <A href="manual/modules.php3"><I>Higher-order modules</I></A>:
	 a more powerful module language
    <LI> <A href="manual/components.php3"><I>Components</I></A>:
	 platform-independence and type-safe dynamic loading of modules
    <LI> <A href="manual/packages.php3"><I>Packages</I></A>:
	 integrating static with dynamic typing and first class modules
    <LI> <A href="manual/pickling.php3"><I>Pickling</I></A>:
	 higher-order and platform-independent, type-safe persistence
    <LI> <A href="manual/constraints.php3"><I>Constraints</I></A>:
	 solving hard problems using constraints and programmable search
  </UL>

  <P>
    The Alice system (in development) is a powerful programming system
    featuring the following tools:
  </P>

  <UL>
    <LI> <A href="manual/usage.php3#interactive"><I>Interactive system</I></A>:
	 an interpreter-like interactive toplevel
    <LI> <A href="manual/usage.php3#compiler"><I>Batch compiler</I></A>:
	 separate compilation
    <LI> <A href="manual/usage.php3#linker"><I>Static linker</I></A>:
	 type-safe bundling of components
    <LI> <A href="manual/inspector.php3"><I>Inspector</I></A>:
	 a tool for dynamically inspecting Alice data structures
    <LI> <A href="manual/explorer.php3"><I>Explorer</I></A>:
	 a tool for interactively investigating search problems
    <LI> <A href="manual/gtk.php3"><I>Gtk+</I></A>:
	 a binding for the Gnome toolkit GUI library
  </UL>

  <P>
    Alice builds on our experience with developing the
    <A href="http://www.mozart-oz.org/">Mozart</A> system.
    The current version of Alice is based on the Mozart virtual machine.
    Alice programs can therefore <A href="manual/interop.php3"
    >interoperate</A> with Oz.
  </P>

<H2>News</H2>

  <UL>
    <LI> 2002/04/25: <A href="download.php3#preview">technology preview</A> version available
    <LI> 2002/03/20: first version of new "Stockwerk" VM is working
    <LI> 2001/04/24: <A href="logos/">logos</A> online
    <LI> 2001/04/20: <A href="bugzilla/">bug-tracking system</A> online
    <LI> 2001/03/14: new Alice home page
  </UL>


<?php pslab_footer("Stockhausen"); ?>
