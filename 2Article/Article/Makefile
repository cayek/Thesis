SRC = draft.tex intro.tex references.tex results.tex tables_figures.tex appendix.tex discussion.tex method.tex draft.bib
TEX = latexmk -pdf -pdflatex="pdflatex -shell-escape -interaction=nonstopmode -file-line-error"
BIB = bibtex

.PHONY: all view error draft OldVersion clean_draft clean_diff clean diff

all : draft

view:
	okular draft.pdf

draft: 
	$(TEX) draft.tex

biblio: 
	$(BIB) draft


error:
	 ack --passthru --color "[eE]rror" draft.log


################################################################################
# Revision version

TAG=2Article_v1.0.1

OldVersion:
	rm -rf OldVersion/
	mkdir OldVersion
	git --work-tree=./OldVersion/ checkout $(TAG) -- ./2Article/Article


diff.tex: OldVersion/2Article/Article/draft.tex
	cd OldVersion/2Article/Article;	$(TEX) draft.tex; cd ../../../
	latexdiff --math-markup=0 OldVersion/2Article/Article/draft.tex draft.tex --flatten > diff.tex

diff: diff.tex
	pdflatex -shell-escape -interaction=nonstopmode -file-line-error diff.tex



################################################################################
# Clean

clean_draft:
	latexmk -CA draft.tex

clean_diff:
	latexmk -CA diff.tex
	rm diff.tex

clean: clean_diff clean_draft
