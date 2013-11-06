

all: documentation_binding.ps

documentation_binding.dvi: documentation_binding.tex
	latex documentation_binding.tex

documentation_binding.ps: documentation_binding.dvi
	dvips documentation_binding.dvi -o documentation_binding.ps

	
