BIB=bibtex
TEX=lualatex -shell-escape --synctex=1 -interaction=nonstopmode

define tex
	$(TEX) --jobname $(1) "\input" $(1).tex
endef

define bib
	$(BIB) $(1)
endef

all: paper presentation

paper: paper.tex paper.bib
	$(call tex,paper)
	$(call bib,paper)
	$(call tex,paper)
	$(call tex,paper)

presentation: presentation.tex
	$(call tex,presentation)
