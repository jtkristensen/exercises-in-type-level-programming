# Requirements
# - you need a variety of things from texlive
# - lytex
# - Pygments (pip install)

all:
	$(MAKE) clean
	$(MAKE) recompile

recompile:
	pdflatex --shell-escape main.tex
	pdflatex --shell-escape main.tex

clean:
	rm -f *.log *.pdf *.out *.snm *.toc *.vrb *.nav *.pyg *.aux
	rm -rf _minted-main
