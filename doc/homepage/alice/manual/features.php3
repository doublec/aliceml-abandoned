<?php include("macros.php3"); ?>

<?php heading("Features", "features") ?>


<?php section("overview", "overview") ?>

  <P>
    <I>Alice</I> is a functional programming language based on
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>,
    extended with support for concurrent, distributed,
    and constraint programming. The <A href="language.php3">Alice language</A>
    extends Standard ML with several new features:
  </P>

  <UL>
    <LI> <A href="futures.php3#lazy"><I>Laziness</I></A>:
	 combining strict and transparent lazy functional programming
    <LI> <A href="futures.php3"><I>Futures</I></A>:
	 "logic variables" and concurrency
    <LI> <A href="modules.php3"><I>Higher-order modules</I></A>:
	 a more powerful module language
    <LI> <A href="components.php3"><I>Components</I></A>:
	 platform-independence and type-safe dynamic loading of modules
    <LI> <A href="packages.php3"><I>Packages</I></A>:
	 integrating static with dynamic typing and first class modules
    <LI> <A href="pickling.php3"><I>Pickling</I></A>:
	 higher-order and platform-independent, type-safe persistence
    <LI> <A href="constraints.php3"><I>Constraints</I></A>:
	 solving combinatorical problems using constraints and programmable
	 search
  </UL>

  <P>
    The Alice system is a powerful programming system
    featuring the following tools:
  </P>

  <UL>
    <LI> <A href="interactive.php3"><I>Interactive system</I></A>:
	 an interpreter-like interactive toplevel
    <LI> <A href="compiler.php3"><I>Batch compiler</I></A>:
	 separate compilation
    <LI> <A href="linker.php3"><I>Static linker</I></A>:
	 type-safe bundling of components
    <LI> <A href="inspector.php3"><I>Inspector</I></A>:
	 a tool for interactively inspecting data structures
    <LI> <A href="explorer.php3"><I>Explorer</I></A>:
	 a tool for interactively investigating search problems
    <LI> <A href="gtk.php3"><I>Gtk+</I></A>:
	 a binding for the Gnome toolkit GUI library
  </UL>

  <P>
    Alice builds on our experience with developing the
    <A href="http://www.mozart-oz.org/">Mozart</A> system.
    The current version of Alice is based on the Mozart virtual machine.
    Alice programs can therefore <A href="interop.php3"
    >interoperate</A> with Oz.
  </P>

<?php footing() ?>
