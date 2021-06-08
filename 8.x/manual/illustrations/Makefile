
DENSITY=300

.PHONY: all clean
all: compilation.png

%.pdf: %.pgf
	pdflatex --jobname=$(basename $<) "\documentclass{standalone}\usepackage{my-pgf}\begin{document}\input{$<}\end{document}"

%.png: %.pdf
	convert -density $(DENSITY) $< $@

clean:
	rm -rf *.aux *.log
