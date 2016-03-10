OUTDIR = out
TEX = TEXINPUTS=risethesis/: pdflatex -output-directory $(OUTDIR)
BIB = BSTINPUTS=risethesis/: bibtex

.PHONY: all aux bib thesis clean outdir

all: outdir bib thesis

thesis: outdir thesis.tex thesis.bbl
	$(TEX) thesis.tex
	$(TEX) thesis.tex
	mv $(OUTDIR)/thesis.pdf .

bib: outdir aux $(OUTDIR)/thesis.aux references.bib
	$(BIB) $(OUTDIR)/thesis
	mv $(OUTDIR)/thesis.bbl .

aux: | out/thesis.aux

out/thesis.aux:
	$(TEX) thesis.tex

outdir: | out

out:
	mkdir -p $(OUTDIR)

clean:
	rm *.pdf
	rm -rf out
