# Makefile for the TIS Formal Design
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

# The formal design needs access to some types,
# states and schemas declared in 41.2 - the Formal Functional Specification.
# Therefore, fragments of 41.2 have to be read in to FUZZ before the actual
# formal design proper
typecheck:
	fuzz -d ../41_2_Formal_Functional_Specification/41_2_Intro \
	../41_2_Formal_Functional_Specification/41_2_TIS \
	../41_2_Formal_Functional_Specification/41_2_Internal \
	50_1_Intro \
	50_1_TIS \
	50_1_Internal \
	50_1_Interfaces \
	50_1_UserEntry \
	50_1_Enclave \
	50_1_Start \
	50_1_Whole \
	50_1_Summary \
	50_1_Abstraction \
	50_1_Refinement

typeset:
	cp empty.txt z-index.tex
	cp empty.txt req-index.tex
	cp empty.txt trace-index.tex
	perl $(PXTOOLDIR)\zlat2 -d 50_1
	perl $(PXTOOLDIR)\zlat2 -x 50_1
	latex 50_1
	latex 50_1
	perl $(PXTOOLDIR)\Zmakeindex 50_1.idx
	latex 50_1
	dvips 50_1

# You _must_ do a "make clean" before checking .tex files back into CM
clean:
	perl $(PXTOOLDIR)\zlat2 -d 50_1
	rm -f *.aux *.log *.bak *.ps *.dvi req-index.tex z-index.tex trace-index.tex 50_1.toc 50_1.idx 50_1.pdf


