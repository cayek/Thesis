SRC = draft.tex intro.tex references.tex results.tex tables_figures.tex appendix.tex discussion.tex method.tex draft.bib
TEX = latexmk -pdf -pdflatex="pdflatex -shell-escape -interaction=nonstopmode -file-line-error"
BIB = bibtex

.PHONY: all view error draft

all : draft

view:
	okular draft.pdf

draft: 
	$(TEX) draft.tex

biblio: 
	$(BIB) draft


error:
	 ack --passthru --color "[eE]rror" draft.log
