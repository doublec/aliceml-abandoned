<?php include("macros.php3"); ?>

<?php heading("Inspector", "inspector") ?>



<?php section("overview", "overview") ?>

  <P>
    The <I>Alice Inspector</I> is a tool that allows to interactively
    display and inspect Alice data structures.  Access to the Inspector
    is provided through the following structure:
  </P>

  <UL>
    <LI>
      <TT>structure Inspector
      <BR>from "x-alice:/lib/tools/Inspector"</TT>
    </LI>
  </UL>

  <PRE>
	structure Inspector :
	sig
	    val inspect :   'a -> unit
	    val inspectN :  int * 'a -> unit

	    (* ... *)
	end</PRE>

  <P>
    An application of the <TT>inspect</TT> function will display an arbitrary
    Alice value in the Inspector window.
  </P>

  <P>
    In the <A href="usage.php3#interactive">interactive toplevel</A> the
    inspector is readily available without further import announcements.
  </P>


<?php section("configuration", "configuration") ?>

  <P>
    Displaying options of the Inspector can be configured interactively
    as well as through Alice:
  </P>

  <PRE>
	structure Inspector :
	sig
	    (* ... *)

	    type value

	    datatype color =
		KEEP_COLOR
	      | SET_COLOR       of {red : int, green : int, blue : int}
	    datatype width =
		KEEP_WIDTHS
	      | REPLACE_WIDTHS  of int vector
	      | APPEND_WIDTH    of int
	      | REMOVE_WIDTH    of int
	    datatype depth =
		KEEP_DEPTHS
	      | REPLACE_DEPTHS  of int vector
	      | APPEND_DEPTH    of int
	      | REMOVE_DEPTH    of int
	    datatype action =
		KEEP_ACTIONS
	      | REPLACE_ACTIONS of (string * (value -> unit)) vector
	      | APPEND_ACTION   of string * (value -> unit)
	      | REMOVE_ACTION   of string

	    datatype option =
		NUMBER            of color *                 action
	      | FUNCTION          of color *                 action
	      | STRING            of color *                 action
	      | HOLE              of color *                 action
	      | FUTURE            of color *                 action
	      | CONSTRUCTOR       of color *                 action
	      | REFERENCE         of                         action
	      | FD                of                         action
	      | FSET              of                         action
	      | TUPLE             of color * width * depth * action
	      | RECORD            of color * width * depth * action
	      | LIST              of color * width * depth * action
	      | CONSTRUCTED_VALUE of         width * depth * action
	      | VECTOR            of color * width * depth * action
	      | RECORD_LABEL      of color
	      (* relation mode *)
	      | ALIAS_DEFINITION  of color
	      | ALIAS_REFERENCE   of color
	      (* ellipses *)
	      | WIDTH_ARROW       of color
	      | DEPTH_ARROW       of color
	      | PARENTHESES       of color
	      | MISC              of color

	    exception ConfigurationError

	    val configure : option vector -> unit   (* ConfigurationError *)
	end</PRE>


<?php footing() ?>
