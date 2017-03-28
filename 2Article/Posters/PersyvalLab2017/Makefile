TEX = pdflatex 
BIB = bibtex

.PHONY : all view clean clean_knitr

all : main.pdf

view : main.pdf
	evince main.pdf&

main.pdf : main.tex main.bbl main.blg
	$(TEX) main.tex

main.bbl main.blg : main.bib main.aux
	$(BIB) main

main.aux : main.tex
	$(TEX) main.tex

main.bib : main.tex
	$(TEX) main.tex

main.tex : main.Rnw
	R -e 'library(knitr);knit("main.Rnw")'

clean : 
	rm -rf *.toc *.log *.nav *.snm *.aux *.out *~ main-tikzDictionary *.bbl *.blg

clean_knitr: 
	rm -rf cache/ figure/ main.tex main.vrb
