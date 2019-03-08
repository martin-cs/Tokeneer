# Makefile for the TIS Security Properties
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

# The security properties themselves are written in terms of types,
# states and schemas declared in 41.2 - the Formal Functional Specification.
# Therefore, most of 41.2 has to be read in to FUZZ before the actual
# security properties can be checked.
typecheck:
	fuzz -d ../41_2_Formal_Functional_Specification/41_2_Intro.tex \
		../41_2_Formal_Functional_Specification/41_2_TIS.tex \
		../41_2_Formal_Functional_Specification/41_2_Internal.tex \
		../41_2_Formal_Functional_Specification/41_2_Interfaces.tex \
		../41_2_Formal_Functional_Specification/41_2_UserEntry.tex \
		../41_2_Formal_Functional_Specification/41_2_Enclave.tex \
		../41_2_Formal_Functional_Specification/41_2_Start.tex \
		../41_2_Formal_Functional_Specification/41_2_Whole.tex \
		40_4_SecurityProperties.tex

typeset:
	cp empty.txt z-index.tex
	cp empty.txt req-index.tex
	cp empty.txt trace-index.tex
	perl $(PXTOOLDIR)\zlat2 -d 40_4
	perl $(PXTOOLDIR)\zlat2 -x 40_4
	latex 40_4
	latex 40_4
	perl $(PXTOOLDIR)\Zmakeindex 40_4.idx
	latex 40_4
	dvips 40_4

# You _must_ do a "make clean" before checking .tex files back into CM
clean:
	perl $(PXTOOLDIR)\zlat2 -d 40_4
	rm -f *.aux *.log *.bak *.ps *.dvi req-index.tex z-index.tex trace-index.tex 40_4.toc 40_4.idx 40_4.pdf


