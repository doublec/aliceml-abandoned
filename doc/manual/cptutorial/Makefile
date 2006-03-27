all: aliceTutorial.pdf
	mkdir -p html
	latex2html -init_file aliceTutorial.pl \
	           aliceTutorial.tex
	cp ../bg.gif ../logo-small.gif style.css html/

aliceTutorial.dvi: aliceTutorial.tex 
	latex aliceTutorial.tex 
	latex aliceTutorial.tex 



aliceTutorial.pdf: aliceTutorial.dvi 
	dvipdf aliceTutorial.dvi 


clean:
	rm -rf html aliceTutorial aliceTutorial.xref aliceTutorial.lof aliceTutorial.toc aliceTutorial.dvi aliceTutorial.pdf aliceTutorial.aux aliceTutorial.log aliceTutorial.out aliceTutorial.lol WARNINGS makefile~ *.*~
