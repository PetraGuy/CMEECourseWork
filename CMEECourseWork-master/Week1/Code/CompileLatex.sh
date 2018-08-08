#!/bin/bash
# Author pg5117@ic.ac.uk
# Script: CompileLatex
# Desc: 
# runs the commands to compile the latex document then opens the document. 
# Make sure you've created the bibliography
# Arguments:Requires FirstExample a tex file in Data directory
#					 FirstBiblio.bib in Data directory		
# #Date October 2017

# I put FirstExample in Data dir
#Remember you need the bibliography to be created
cd ../Data
pdflatex $1.tex
pdflatex $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex
evince $1.pdf &

#clean up - this deletes all the files created to make the document
#rm *~ # too scary
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc
#added these last two on, because I could see these had been
# created hope they're no important? check this later coz StackExchange says
#you need some of the above, like .aux
rm *.blg 
rm *.bbl

