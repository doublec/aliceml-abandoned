<?php include("macros.php3"); ?>
<?php heading("The Search structure", "The <TT>Search</TT> structure"); ?>

<?php section("synopsis", "synopsis"); ?>
  <PRE>
    signature SEARCH
    structure Search : SEARCH</PRE>

  <P>
    The <TT>Search</TT> structure gives access to predefined inference
    engines, including one solution, all solution and best solution search.
  </P>

<?php section("import", "import"); ?>

  <PRE>
    import signature SEARCH from "x-alice:/lib/constraints/SEARCH-sig"
    import structure Search from "x-alice:/lib/constraints/Search"</PRE>

<?php section("interface", "interface"); ?>
  <PRE>
    signature SEARCH =
    sig
	type 'a pruner = 'a * 'a -> unit

	val searchOne : (unit -> 'a) -> 'a option
	val searchAll : (unit -> 'a) -> 'a list
	val searchBest : (unit -> 'a) * 'a pruner -> 'a option
    end</PRE>

<?php section("description", "description"); ?>

  <DL>
    <DT>
      <TT>type 'a pruner = 'a * 'a -> unit</TT>
    </DT>
    <DD>
      <P>Pruning function type.
      </P>
    </DD>

    <DT>
      <TT>searchOne <I>p</I></TT>
    </DT>
    <DD>
      <P>This inference engine takes a constraint
         problem script <I>p</I> and returns a solution of the problem.
         If no solution exists, <TT>NONE</TT> is returned.
      </P>
    </DD>

    <DT>
      <TT>searchAll <I>p</I></TT>
    </DT>
    <DD>
      <P>This inference engine takes a constraint
         problem script <I>p</I> and returns a list containing all
         solutions of the problem. If no solution exists, the empty list
         is returned.
      </P>
    </DD>

    <DT>
      <TT>searchBest (<I>p</I>, <I>o</I>)</TT>
    </DT>
    <DD>
      <P>This inference engine takes a constraint
         problem script <I>p</I> together with a ordering function <I>o</I>
         and returns the best solutions of the problem according to
         the given order. If no solutions exists, <TT>NONE</TT> is returned.
      </P>
    </DD>
  </DL>

<?php footing(); ?>
