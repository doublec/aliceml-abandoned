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

  <IMG align="right" src="heli-ani.gif">
  <BR clear=all>

  <P>
    Welcome to the "Helikopter" home page, the <A href="/">Programming Systems
    Lab</A>'s team for ICFP Programming Contests.
  </P>

  <P>
    In <A href="http://cristal.inria.fr/ICFP2001/prog-contest/">this year's
    Contest</A> the <A
    href="http://cristal.inria.fr/ICFP2001/prog-contest/task.html">task</A> was
    to write an optimizer for documents in a simple HTML-like markup language.
    Optimization has to be performed under given time constraints.
    As always, the task had to be finished within 72 hours.
  </P>

  <P>
    We already participated in <A href="http://www.cs.cornell.edu/icfp/">last
    year's Contest</A> where the <A
    href="http://www.cs.cornell.edu/icfp/task.htm">task</A> was to write a an
    interpreter and raytracer for a Postscript-like 3D description language and
    <A href="2000/">our entry</A> was chosen for the Judge's Prize.
  </P>



<?php section("members", "members") ?>

  <P>
    This time our team consisted of three people:
  </P>

  <UL>
    <LI> <A href="/~duchier/">Denys Duchier</A> </LI>
    <LI> <A href="/~kornstae/">Leif Kornstaedt</A> </LI>
    <LI> <A href="/~rossberg/">Andreas Rossberg</A> </LI>
  </UL>

  <P>
    Denys did not want to participate at first ("Uh, I have these important
    deadlines..."), but then somehow got hooked by the problem - which was
    very good because of his expertise in constraints! :-) For Andreas and Leif
    the contest days were kind of a crash course into the deeper secrets
    (and abysses!) of using advanced constraint techniques...
  </P>


<?php section("description", "description") ?>

  <P>
    Our entry is hybrid <A href="/alice/">Alice</A>/<A
    href="http://www.mozart-oz.org">Oz</A> program that uses a constraint
    programming approach to tackle the problem.
  </P>

  <P>
    First we parse the input, of course. The parser removes redundant spaces
    and constructs a compact representation of the document's meaning, where
    input characters have been grouped into strings with equal or compatible
    properties. It also calculates the <I>cost</I> of the input document (see
    below). Moreover, it constructs a simple version of the document that can
    be output in case we do not find a better solution in time (essentially the
    input with whitespace compression). Then we try to construct a cheaper
    document out of the meaning.
  </P>

  <P>
    The main idea of our approach is as follows: An SML/NG document essentially
    represents a tree with the tags being inner nodes and the actual text (or
    the meaning) being its leaves. The root is a special node without a tag.
    All nodes can be annotated by a property record: the root's property is the
    initial context, an inner node's property is the context implied by its
    parents, and a leaf's property is determined by the document meaning. We
    can also assign a cost value to each inner node, corresponding to the
    textual length of its tag. The problem now is to find a tree with minimal
    total cost that has consistent property annotations. An upper bound for the
    cost (and thus the number of inner nodes) is the one calculated for the
    input document.
  </P>

  <P>
    To find such a tree we create a set of constraint variables representing
    the nodes of the tree, their relations, and their annotations and impose
    appropriate constraints enforcing properties like treeness, consistency of
    annotations, and correct meaning. In order to reduce the search space to a
    tractable size additional redundant constraints are necessary. They are needed
    to maximize propagation and thus minimize search. The main difficulty is to
    find suitable constraints for this purpose that interact in useful ways and
    are well-suited to a bottom-up search strategy (which is best
    for this particular problem since we only know about the leaves). We relied
    on the following kinds of constraints (the latter two are unique to the
    Mozart system):
  </P>

  <UL>
    <LI> Finite Domain Constraints </LI>
    <LI> Finite Set Constraints </LI>
    <LI> Selection Constraints </LI>
    <LI> Sequenced Union Constraints (that part did not make it into the
         submission, though) </LI>
  </UL>

  <P>
    A detailed formal description of our problem formulation with constraints
    is available as a <A href="readme.ps">Postscript document</A>.
  </P>

  <P>
    Unfortunately, the constraint set used in the submission is much too weak to
    really reduce the search space to a tractable size for realistic input.
    Though we had a lot of ideas for additional constraints we could not
    implement them in time (a few are mentioned in the Postscipt doc). As a
    consequence, the submitted entry is likely to behave rather badly on large
    inputs :-(
  </P>

  <P>
    Since the size of the search space can still be exponential in the cost of
    the input even with good propagation, large documents are split into
    several parts. For each part we concurrently search for a good solution
    until the timeout is reached (with bad impact on memory consumption - some
    scheduling in a round robin fashion would have been much much better...).
    When we do not find any solution for some part we use the simple one with
    white space compression only which is constructed by the parser. Otherwise
    we use the best solution we found so far.
  </P>

  <P>
    In summary, we concentrated much too strongly on our constraint formulation
    - because we found that being the only really interesting thing - and
    completely neglected other important aspects of the program.
  </P>


<?php section("sources", "sources") ?>

  <P>
    You can take a look at our submission, a program we named "Smurf" because
    we did not find a better name for the module doing the main work:
  </P>

  <UL>
    <LI> <A href="src/">Alice/Oz source of our renderer</A>. </LI>
    <LI> <A href="readme.ps">A description of the constraint formulation</A>
         (see above). </LI>
  </UL>

  <P>
    Note that we make use of the <A
    href="http://www.ps.uni-sb.de/~duchier/mogul/info/duchier/select.html">
    selection constraint module</A> which can be found in the <A
    href="http://www.mozart-oz.org/mogul/">MOGUL archive</A> for Mozart.
  </P>


  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       last modified 2001/07/30
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
