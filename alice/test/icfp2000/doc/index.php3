<?php
  function section($tag, $name)
  {
    $n = 40 - strlen($name);

    for ($bar = ""; $n > 0; $n--)
    {
	$bar .= "_";
    };

    echo("<H2><A name=" . $tag . ">" . $name . " " . $bar .
	 "</A></H2> <BR><BR>");
  };
?>


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE> Team Helikopter </TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>


  <H1>
  team<BR>
  helikopter.<BR>
  </H1>

  <IMG align="right" src="examples/heli-ani.gif">
  <BR clear=all>

  <P>
    Welcome to the submission home page of team "Helikopter", formed for
    participation at the
    <A href="http://www.cs.cornell.edu/icfp/">ICFP 2000 Functional Programming
    Contest</A>.
  </P>

  <P>
    In this year's contest the
    <A href="http://www.cs.cornell.edu/icfp/task.htm">task</A> was to write
    a raytracing engine together with an interpreter for its Postscript-like
    description language within 72 hours. With the resulting program you are
    able to model and render arbitrary 3D sceneries. A special feature of the
    rendering language was its procedural texturing where you pass
    higher-order functions to describe the surface of each individual object.
  </P>

  <P>
    We are astonished to see that we've been declared winner of the
    <B>Judge's Prize</B> for one of our demo sceneries! It's a complete
    <A href="examples/chess.jpg">chess board</A> and makes use of almost
    all features of the rendering language GML. Another demo scenery we
    submitted was the animated helikopter you see above.
  </P>



<?php section("members", "members") ?>

  <P>
    Our team just consisted of two people:
  </P>

  <UL>
    <LI> <A href="/~kornstae/">Leif Kornstaedt</A> </LI>
    <LI> <A href="/~rossberg/">Andreas Rossberg</A> </LI>
  </UL>

  <P>
    Originally, we were a team of four, but one member dropped out after
    seeing the task spec, and the last member didn't bother to give any
    sign of life at all until monday evening...
  </P>

  <P>
    In case you wonder: our team has been named after the famous
    <A href="http://www.stockhausen.org/">"Helikopter-Streichquartett"</A>
    by Karlheinz Stockhausen, for reasons you can find out below.
  </P>



<?php section("sources", "sources") ?>

  <P>
    You can take a look at our submission (with the bug fixes mentioned below):
  </P>

  <UL>
    <LI> <A href="src/">SML source of our renderer.</A> </LI>
    <LI> <A href="examples/chess.gml">GML source of the chess board.</A> </LI>
    <LI> <A href="examples/helikopter-ani.gml">GML source of the animated
					  helikopter.</A> </LI>
  </UL>



<?php section("description", "description") ?>

  <P>
    Although we won the Judge's Prize for our chess scenery, you still might
    be interested in our renderer, written in Standard ML.
  </P>

  <P>
    Originally we planned to participate with our own evolving
    implementation of an SML dialect, codenamed
    <A href="/stockhausen/operette1/">Stockhausen</A>. It provides all sorts of
    nice features not found in SML, but unfortunately none of them would have
    been of much use for this particular task. And our floating point
    performance is lousy... :-( So we had to stick to SML/NJ.
  </P>

  <P>
    Our submission in general:
  </P>

  <UL>
    <LI>
      We implemented all three tiers. Most stuff works fine, but there 
      are some stupid bugs (see below).
    </LI>
    <LI>
      We tried to do a clean (modular, strongly typed & functional) design,
      efficiency was only of secondary concern. We did not have the time for
      any serious optimizations and our rendering algorithm seems to be
      suboptimal - we are slooow compared to some other entries. :-(
    </LI>
    <LI>
      Error messages are simplistic.
      Error positions during parsing are only given in absolute character
      positions.
    </LI>
    <LI>
      Total LOC is around 1300 (not counting generated files) which seems
      pretty short compared to the other entries we have seen (even the Haskell
      one...).
    </LI>
  </UL>

  <P>
    Modularization:
  </P>

  <UL>
    <LI>
      Of course, the renderer kernel is completely independent of the abstract
      machine (actually the machine could be functorized over the renderer).
      Surface functions are passed as ordinary SML functions.
    </LI>
    <LI>
      Likewise, the Machine completely encapsulates knowledge of the renderer.
    </LI>
    <LI>
      The set of builtin operations (including names) is encapsulated in the
      machine. The parser does not know about it.
    </LI>
    <LI>
      Toplevel control loop for rendering is factored out of the renderer
      (would allow future optimizations of the rendering process by
      recalculating bounding boxes for example).
    </LI>
  </UL>

  <P>
    Frontend and GML Machine:
  </P>

  <UL>
    <LI>
      Parser and lexer have been generated with ML-Yacc/ML-Lex.
    </LI>
    <LI>
      The parser does complete static binding analysis.
    </LI>
    <LI>
      Most primitive operations are encoded in a datatype carrying the
      corresponding SML implementations as higher-order functions, the
      actual evaluator is therefore very short and simple.
      False, true, if, and apply are treated as ordinary operators.
    </LI>
    <LI>
      Constant surface functions are recognized and optimized. Strangely
      enough though, this does not seem to have any positive effect on runtime
      whatsoever. :-(
    </LI>
  </UL>

  <P>
    Renderer:
  </P>

  <UL>
    <LI>
      Cubes, cones, and cylinders are decomposed into intersections
      of simpler objects (planes, infinite cones & cylinders).
    </LI>
    <LI>
      All rotational objects are treated uniformingly by parameterization.
    </LI>
    <LI>
      Dealing with rounding errors:
      <UL>
        <LI> Intersection ignores objects closer than some eps. </LI>
        <LI> Difference ignores eps sized slices. </LI>
      </UL>
    </LI>
    <LI>
      Optimizations:
      <UL>
        <LI> Illumination equation simplified for kd=0 or ks=0. </LI>
        <LI> Intersection and difference perform shortcut evaluation. </LI>
      </UL>
    </LI>
  </UL>


  <P>
    Bugs (The Embarassing Part):
  </P>

  <UL>
    <LI>
      In the submitted version, the trigonometric operators did not work
      correctly - because we accidently deleted the conversion from degrees
      to radiant during some code restructuring 8-} (who chose degrees for
      GML anyway?). This is fixed.
    </LI>
    <LI>
      There was a bug with shadow rays in the submitted version, caused by
      using a wrong signed epsilon for tolerance against rounding errors.
      Caused strange shadows with pointlights sometimes. Fixed.
    </LI>
    <LI>
      Cones seemed to produce a mysterious ghost circle at some distance below
      - as a workaround for the submission we avoided this bug by intersecting
      all cones with a surrounding sphere ;-) We still don't know what the
      cause is...
    </LI>
    <LI>
      And then this stupid Domain exception that is thrown under obscure
      circumstances... Obviously, at some boundary conditions, we get a NaN
      somewhere - no idea. And out of false pride we did not wrap the renderer
      into some exception handler to sneak out of such conditions...
    </LI>
    <LI>
      Last but not least: we are way too slow!
    </LI>
  </UL>


  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       last modified 2000/09/15
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
