<?php

  function heading($title, $chapter)
  {
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE> <?php echo "Alice Manual - $title" ?></TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>


  <H1>
<!--
  <TABLE cellpadding=8 bgcolor=white>
    <TR><TD>
      <IMG class=logo src="alice.gif">
    </TD></TR>
  </TABLE>
  <BR>
-->
  alice<BR>
  manual.<BR>
  <BR>
  <BR>
  <?php echo($chapter) ?>
  <BR>
  </H1>

  <IMG align="right" src="alice.gif">

  <?php
    include("menu.php3")
  ?>
<?php
  };

  function footing()
  {
?>
  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       last modified <?php echo(date("Y/m/d H:i", getlastmod())) ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
<?php
  };

  function section($tag, $name)
  {
    $n = 40 - strlen($name);

    for ($bar = ""; $n > 0; $n--)
    {
	$bar .= "_";
    };

    echo("<H2><A name=" . $tag . ">" . $name . "&nbsp;" . $bar .
	 "</A></H2> <BR><BR>");
  };
?>
