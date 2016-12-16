BIB=bibtex
TEX=lualatex -shell-escape --synctex=1 -interaction=nonstopmode

all: paper presentation

paper: paper.tex paper.bib
	$(TEX) --jobname paper "\input" paper.tex
	$(BIB) paper
	$(TEX) --jobname paper "\input" paper.tex
	$(TEX) --jobname paper "\input" paper.tex

presentation: presentation.tex
	$(TEX) --jobname presentation "\input" presentation.tex
