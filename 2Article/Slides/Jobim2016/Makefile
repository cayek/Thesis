TEX = pdflatex
BIB = bibtex
LATEXMK = latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make
RSCRIPT = Rscript
PRINT = @echo -e "\e[1;34mBuilding $@\e[0m"
FIGS = barplot.pdf Q_K3.pdf KSelection.pdf variogram.pdf G_K6_sigma1_5.png Q_K6_sigma1_5.pdf map.pdf

.PHONY: all view clean clean_knitr figures clean_figures

all: main.pdf

figures: $(FIGS) 

view: main.pdf
	evince main.pdf&

main.pdf: main.tex
	$(LATEXMK) main.tex

main.tex: main.Rnw
	R -e 'library(knitr);knit("main.Rnw")'

clean:
	latexmk -CA
	rm -f *.bbl *.blg *.aux *.end *.fls *.log *.out *.fdb_latexmk *.nav *.snm

clean_knitr:
	rm -rf cache/ figure/ main.tex main.vrb

clean_figures:
	rm -f $(FIGS) 

################################################################################
# figures

map.pdf: map.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<

Q_K3.pdf: Q_K3.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<

barplot.pdf: barplot.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<
	
KSelection.pdf: KSelection.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<
	
variogram.pdf: variogram.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<
	
G_K6_sigma1_5.png Q_K6_sigma1_5.pdf: K6_sigma1.5.R PlotParams.R
	$(PRINT)
	$(RSCRIPT) $<
	
	