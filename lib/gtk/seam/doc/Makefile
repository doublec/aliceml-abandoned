DOCS = report manual
DVIS = $(DOCS:%=%.dvi)
PDFS = $(DOCS:%=%.pdf)

all: $(DVIS) $(PDFS)

%.dvi: %.tex
	latex $<

%.pdf: %.tex
	pdflatex $<

clean:
	rm -f $(DVIS) $(PDFS) *.log *.aux


distclean: clean
