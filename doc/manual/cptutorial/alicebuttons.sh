#!/bin/sh
 
for file in *.html;
do sed -e 's/<BODY >/<BODY ><DIV class=margin><H1>alice<BR>tutorial.<BR><BR><BR><\/H1><P class="menu"><A href="index.html">Contents<\/A><BR><A href="tour.html">Tour<\/A><BR><A href="language.html">Language<\/A><BR><A href="library\/index.html">Library<\/A><BR><A href="tools.html">Tools<\/A><BR><A href="sitemap.html">Index<\/A><BR><\/P><A href="http:\/\/www.ps.uni-sb.de\/alice\/"><IMG src="logo-small.gif" border=0 alt="Alice Project"><\/A><\/DIV><H1><\/H1>/' $file > tmpfile.txt;mv -f tmpfile.txt $file;done


#<BODY ><DIV class=margin><H1>alice<BR>tutorial.<BR><BR><BR><\/H1>
#<P class="menu"><A href="index.html">Contents<\/A><BR><A href="tour.html">Tour<\/A><BR><A href="language.html">Language
#<\/A><BR><A href="library\/index.html">Library<\/A><BR><A href="tools.html">Tools<\/A><BR><A href="sitemap.html">Index<\/A>
#<BR><\/P><A href="http:\/\/www.ps.uni-sb.de\/alice\/"><IMG src="logo-small.gif" border=0 alt="Alice Project"><\/A><\/DIV>
#<H1><\/H1>