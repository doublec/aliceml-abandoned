DOCS = report manual report_final
DVIS = $(DOCS:%=%.dvi)
PSS  = $(DOCS:%=%.ps)
PDFS = $(DOCS:%=%.pdf)
IMAGES = events-c events-alice layout generator
EPSS = $(IMAGES:%=%.eps)


all: $(DVIS) $(PSS) $(PDFS)

%.dvi: %.tex
	(latex $< && latex $< && latex $<)  # for cross-refs

%.ps: %.dvi
	dvips -Ppdf $< -o $@

#%.pdf: %.tex
#	pdflatex $<

%.pdf: %.ps
	ps2pdf $<

report_final.dvi: $(EPSS)

%.eps: %.fig
	fig2dev -L eps $< > $@

clean:
	rm -f $(DVIS) $(PSS) $(EPSS) $(PDFS) *.log *.aux *.toc


distclean: clean
