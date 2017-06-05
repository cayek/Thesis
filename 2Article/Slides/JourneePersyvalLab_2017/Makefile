LATEXMK = latexmk -pdf -bibtex -pdflatex="pdflatex -interaction=nonstopmode" 

.PHONY: all view clean

all: main.pdf

view: main.pdf
	evince main.pdf&

main.pdf: main.tex
	$(LATEXMK) main.tex

clean:
	latexmk -CA
	rm -f *.bbl *.blg *.aux *.end *.fls *.log *.out *.fdb_latexmk *.nav *.snm
