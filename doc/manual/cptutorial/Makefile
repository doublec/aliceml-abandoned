all: aliceTutorial.pdf
	latex2html -split 5  -no_subdir -local_icons -info 0   -white -no_math -html_version 4.0,table,math aliceTutorial.tex
	cp aliceTutorial.css tmp.css
	cat tmp.css style.css > aliceTutorial.css
	./alicebuttons.sh


aliceTutorial.dvi: aliceTutorial.tex 
	latex aliceTutorial.tex 
	latex aliceTutorial.tex 



aliceTutorial.pdf: aliceTutorial.dvi 
	dvipdf aliceTutorial.dvi 


clean:
	rm -rf aliceTutorial aliceTutorial.xref aliceTutorial.lof aliceTutorial.toc aliceTutorial.dvi aliceTutorial.pdf aliceTutorial.aux aliceTutorial.log aliceTutorial.out aliceTutorial.lol  aliceTutorial.css images.* WARNINGS *.pl *.png *.html makefile~ tmp.css *.*~