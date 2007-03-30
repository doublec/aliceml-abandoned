<?php

  function prep_title($name)
  {
    $name = str_replace(" ", "&nbsp;", $name);
    $name = str_replace("\n", "<BR>", $name);
    return $name;
  }

  function heading($title, $chapter)
  {
    $chapter2 = prep_title($chapter);
?>
<!-- THESE HTML PAGES ARE GENERATED. DO NOT EDIT DIRECTLY!   -->
<!-- See directory doc/manual/ in the Alice CVS for sources. -->

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
    <TITLE> <?php echo "Alice Manual - $title" ?></TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>


  <!-- margin-color: #83a2eb -->

  <DIV class=margin>

  <H1>
  alice<BR>
  manual.<BR>
  <BR><BR>
  </H1>

  <?php include("menu.html") ?>

  <A href="http://www.ps.uni-sb.de/alice/">
  <IMG src="logo-small.gif"
       border=0
       alt="Alice Project">
  </A>

  </DIV>

  <H1>
  <?php echo($chapter2) ?>
  </H1>

<?php
  };
 
  function footing()
  {
  $trace = debug_backtrace();
  $file = substr_replace($trace[0]['file'], "", -strlen(".static.in"));
  $lastmod = date("Y/m/d H:i", filemtime($file));
?>
  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       last modified <?php echo("$lastmod"); ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
<?php
  };

  function section($tag, $name)
  {
    $n = 60 - strlen($name);
    $name = prep_title($name);

    for ($bar = ""; $n > 0; $n--)
    {
	$bar .= "_";
    };

    echo("<BR><A name=" . $tag . ">" .
	 "<H2><SUP><TT>________&nbsp;</TT></SUP>" . ucfirst($name) .
	 "<SUP><TT>&nbsp;" . $bar . "</TT></SUP></H2></A>");
  };

  function subsection($tag, $name)
  {
    echo("<H3><A name=" . $tag . ">" . ucfirst($name) . "</A></H3>");
  };
?>
