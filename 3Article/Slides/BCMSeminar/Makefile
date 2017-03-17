.PHONY: all view Images/Athaliana_B.png

all: main.pdf

view:
	okular main.pdf

main.pdf: main.tex
	pdflatex main.tex

main.tex: main.org header.tex
	emacs main.org --batch -f org-beamer-export-to-latex --kill


