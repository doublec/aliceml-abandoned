<?php include("macros.php3"); ?>

<?php heading("Explorer", "explorer") ?>



<?php section("overview", "overview") ?>

  <P>
    The <I>Alice Explorer</I>
    is a graphical and interactive  tool to visualize
    and analyze search trees.  Access to the Explorer
    is provided through the following structure:
  </P>

  <UL>
    <LI>
      <TT>structure Explorer
      <BR>from "x-alice:/lib/constraints/Explorer"</TT>
    </LI>
  </UL>

  <PRE>
	structure Explorer :
	sig
            val exploreOne : (unit -> 'a) -> unit
            val exploreOneBAB : (unit -> 'a) * ('a * 'a -> unit) -> unit
            val exploreAll : (unit -> 'a) -> unit
            val exploreAllBAB : (unit -> 'a) * ('a * 'a -> unit) -> unit
            val exploreBest : (unit -> 'a) * ('a * 'a -> unit) -> unit
	end</PRE>

  <P>
    The Explorer is invoked by applying it to a <EM>script</EM> as follows:
  </P>
  <UL>
    <LI>
      <TT>exploreOne</TT> explores the search
      tree up to the first solution.
    </LI>
    <LI>
      <TT>exploreOneBAB</TT> explores the search
      tree up to the first solution. It follows a branch and bound strategy with respect
      to the function given as second argument.
    </LI>
    <LI>
     <TT>exploreAll</TT> explores the entire search
     tree.
    </LI>
    <LI>
      <TT>exploreAllBAB</TT> explores the entire search
      tree. It follows a branch and bound strategy with respect
      to the function given as second argument.
    </LI>
    <LI>
     <TT>exploreBest</TT> explores the entire search
     tree following a branch and bound strategy. Best solution is performed with respect
     to the function given as second argument.
    </LI>
  </UL>
  <P>
    For further details, we refer to the
    <A HREF="http://www.mozart-oz.org/documentation/explorer/index.html">
    Mozart Documentation</A> and the
    <A HREF="examples.php3#explorer">Alice Examples</A>, respectively.
  </P>

<?php footing() ?>
