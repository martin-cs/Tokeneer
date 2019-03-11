# Makefile for the TIS Formal Functional Specification
#
# Assume that both "fuzz" and "latex" tools
# are installed and configured with the Praxis Z styles,
# document class and fonts, and that both tools appear
# on PATH
#

# Location of Praxis Z tracing and index tools
# Re-set this as appropriate.
PXTOOLDIR := d:\sparkdev\tokeneer\tools

all: typecheck typeset

typecheck:
	fuzz -d 41_2_Intro.tex 41_2_TIS.tex 41_2_Internal.tex \
	41_2_Interfaces.tex 41_2_UserEntry.tex 41_2_Enclave.tex \
	41_2_Start.tex 41_2_Whole.tex

typeset:
	cp empty.txt z-index.tex
	cp empty.txt req-index.tex
	cp empty.txt trace-index.tex
	perl $(PXTOOLDIR)\zlat2 -d 41_2
	perl $(PXTOOLDIR)\zlat2 -x 41_2
	latex 41_2
	latex 41_2
	perl $(PXTOOLDIR)\Zmakeindex 41_2.idx
	latex 41_2
	dvips 41_2

# You _must_ do a "make clean" before checking .tex files back into CM
clean:
	perl $(PXTOOLDIR)\zlat2 -d 41_2
	rm -f *.aux *.log *.bak *.ps *.dvi req-index.tex z-index.tex trace-index.tex 41_2.toc 41_2.idx 41_2.pdf


