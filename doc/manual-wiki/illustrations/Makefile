
DENSITY=300

all: compilation.png
	ln -s $(shell basename $(CURDIR))/$< ..

%.pdf: %.pgf
	pdflatex --jobname=$(basename $<) "\documentclass{standalone}\usepackage{my-pgf}\begin{document}\input{$<}\end{document}"

%.png: %.pdf
	convert -density $(DENSITY) $< $@


.PHONY:
clean:
	rm -rf *.aux *.log
