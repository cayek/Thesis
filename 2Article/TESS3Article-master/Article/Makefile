################################################################################
# for color output
RED = \033[0;31m
GREEN =  \033[0;32m
NC = \033[0m # No Color
PRINT = @echo "$(GREEN)Building $@ $(NC)"

LATEX = latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode"
SRC = appendix.tex draft.tex intro.tex results.tex method.tex tables_figures.tex discussion.tex references.tex
MAIN = draft

.PHONY: all OldVersion

all: draft.pdf

view: draft.pdf
		evince draft.pdf&

draft.pdf: $(SRC)
		$(LATEX) draft.tex

clean:
	$(LATEXMK) -C $(MAIN)
	rm -f $(MAIN).pdfsync
	rm -rf *~ *.tmp
	rm -f *.bbl *.blg *.aux *.end *.fls *.log *.out *.fdb_latexmk

################################################################################
# Revision version

TAG=v0.2.1

OldVersion:
	mkdir OldVersion
	git --work-tree=./OldVersion/ checkout $(TAG) -- ./Article

diff.tex: $(SRC)
	latexdiff --math-markup=0 OldVersion/Article/draft.tex draft.tex --flatten > diff.tex

diff.pdf: diff.tex
		$(LATEX) diff.tex
