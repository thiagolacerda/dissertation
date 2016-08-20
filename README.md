Masters Dissertation of Thiago de Barros Lacerda

Written in latex. Just type make in order to generate the pdf file.

Another way: pdflatex dissertation.tex && bibtex dissertation && pdflatex dissertation.tex && pdflatex dissertation.tex

== Notes ==

Some style files (like psbox psboxit) may not be available with standard tex installations. You will need to install it
manually in the texmf folder, usually in /usr/local/texlive/texmf-local for Unix like systems, and then run mktexlsr (or
texhash).
You might also run into an issue where abnt.bst is not found (even though it is in the template folder). You will have
to copy the abnt.bst file to the bibtx search path folder (usually in /usr/local/texlive/texmf-local/bibtex/bst/) and
run mktexlsr again.

psboxit: https://www.ctan.org/pkg/psboxit?lang=en
