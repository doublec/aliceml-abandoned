The Tutorial contains the following files:
- alicetutorial.tex
- makefile
- alicebuttons.sh
- html.sty
- style.css
- logo-small.gif

additionally it contains the folders:
- Beispielprogramme
- figs
- figs2

The html code is produced by running latex2html with the following parameters: 
-split 5  -no_subdir -local_icons -info 0 -white -no_math -html_version 4.0,table,math aliceTutorial.tex

The following command is used to give the tutorial the alice typical appearence:
cp aliceTutorial.css tmp.css
cat tmp.css style.css > aliceTutorial.css

The command ./alicebuttons.sh is a hack to insert the links to the Library,Tools,Index,..




When having produced the html code you maybe to update the foolowing:

- The contents button of the tutorial and the alice-index may overlap
- The links to my folders www.ps.uni-sb.de/~brill/..... have to be changed