INSTRUCTIONS FOR CREATING TIS FORMAL SPECIFICATION 41_2.
========================================================

This is a Z laTeX document with four embedded diagrams.

The document is broken up into several files: the document map is
given below:

  41_2.tex
        41_2_Intro.tex
                41_2_certs.eps
        41_2_TIS.tex
        41_2_Interfaces.tex
        41_2_Internal.tex
        41_2_UserEntry.tex
                41_2_entry.eps
        41_2_Enclave.tex
                41_2_enrol.eps
                41_2_admin.eps
                41_2_adminOp.eps
        41_2_Start.tex
        41_2_Whole.tex
        41_2_Zbasics.tex
	41_2_Summary.tex
	41_2_Pre.tex
	41_2_SRSTrace.tex
	41_2_STTrace.tex
        z-index.tex
        trace-index.tex
        req-index.tex

The final 3 documents are auto-generated.
The .EPS files are generated from Visio diagrams.

Updating the Diagrams
--------------------- 
The diagrams were created in Visio. 

Perform the updates in Visio by editing the .VSD files.

Convert the Visio diagrams to Embedded Postscript by saving as .EPS
files from Visio. 

The .EPS files are imported by LaTeX.

Updating the Z indexing
-----------------------
In order to make reading the Z easier, Z cross-references are
generated in the final document.

The process for generating these cross-references involves using two
scripts "zlat2" and "Zmakeindex" - these have been tested on Boole.

Run the following:

> zlat2 -d 41_2       % this removes previous cross references.
> zlat2 -x 41_2       % this adds the new cross-references.
> latex 41_2
> latex 41_2
> Zmakeindex 41_2.idx
> latex 41_2

Note that latex will require the following files to be present
"z-index.tex" "trace-index.tex" "req-index.tex". Create empty copies
of these files to start with, then Zmakeindex will generate
replacements.

Returning Documents to doctools
---------------------------------
Before any of the TEX files are returned to configuration management
the z cross-references should be removed, they make the files large
and difficult to edit.

Run the following to remove cross-refs.

> zlat2 -d 41_2

Type Checking
-------------

This document is type checked with fuzz. To type check the complete
document use the command:

> fuzz -d 41_2_Intro.tex 41_2_TIS.tex 41_2_Internal.tex
  41_2_Interfaces.tex 41_2_UserEntry.tex 41_2_Enclave.tex
  41_2_Start.tex 41_2_Whole.tex

the -d option is required (there is one use before declaration!).

Again typechecking has been tested on Boole.
