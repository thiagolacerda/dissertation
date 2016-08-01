OUTDIR = out
TEX = TEXINPUTS=risethesis/: pdflatex -output-directory $(OUTDIR)
BIB = BSTINPUTS=risethesis/: bibtex

.PHONY: all aux bib dissertation clean outdir

all: outdir bib dissertation

dissertation: outdir dissertation.tex dissertation.bbl
	$(TEX) dissertation.tex
	$(TEX) dissertation.tex
	mv $(OUTDIR)/dissertation.pdf .

bib: outdir aux $(OUTDIR)/dissertation.aux references.bib
	$(BIB) $(OUTDIR)/dissertation
	mv $(OUTDIR)/dissertation.bbl .

aux: | out/dissertation.aux

out/dissertation.aux:
	$(TEX) dissertation.tex

outdir: | out

out:
	mkdir -p $(OUTDIR)

clean:
	rm -rf *.pdf
	rm -rf out
