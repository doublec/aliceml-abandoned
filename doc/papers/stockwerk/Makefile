all:
	latex main
	bibtex main
	latex main
	bibtex main
	latex main
	latex main
	dvips -o main.ps main.dvi

view:
	gv main.ps

clean:
	rm -f main.dvi main.ps *.aux *.log *.bbl *.blg *.toc

distclean: clean
	rm -f *~
