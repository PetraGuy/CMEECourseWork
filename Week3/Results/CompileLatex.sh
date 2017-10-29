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
#Remember you need the biblio graphy to be created

pdflatex Autocorrlatex2.tex # does something
pdflatex Autocorrlatex2.tex #compiles it
#pdflatex $1.tex # if this was bibtex, you'd need 2 more statements again
#pdflatex $1.tex
#pdflatex $1.tex

evince Autocorrlatex2.pdf &

#clean up - this deletes all the files created to make the documentd
#rm *~ # too scary
rm *.aux
#rm *.dvi
rm *.log
#rm *.nav
rm *.out
#rm *.snm
#rm *.toc
#added these last two on, because I could see these had been
# created hope they're no important? check this later coz StackExchange says
#you need some of the above, like .aux
#rm *.blg 
#rm *.bbl

