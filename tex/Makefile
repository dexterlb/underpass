.DEFAULT_GOAL := main.pdf

SRC:=			  $(wildcard *.pdf)
PDF=			  $(SRC:.tex=.pdf)
RM=			      rm
LATEXMK =		  latexmk
LATEXMK_OPTIONS = -xelatex \
				  -halt-on-error

all : $(PDF)

%.pdf : %.tex
	$(LATEXMK) $(LATEXMK_OPTIONS) $<

clean:
	$(LATEXMK) -C

watch:
	$(LATEXMK) $(LATEXMK_OPTIONS) main.tex -pvc

.PHONY: all clean
