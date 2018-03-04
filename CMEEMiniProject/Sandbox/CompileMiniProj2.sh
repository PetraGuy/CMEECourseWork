#!\bin\bash

cd ../Documents 
mv Sorbusaucuparia.pdf ../Code
mv bibliography.bib ../Code
cd ../Code
cd ../Results 
rm -r figure
cd ../Code
 
Rscript -e "library(knitr); knit('MiniProj3.Rmd')"

Rscript -e "library(rmarkdown); render('MiniProj3.md')"

mv Sorbusaucuparia.pdf ../Documents
mv figure ../Results
mv MiniProj3.md ../Documents
mv bibliography.bib ../Documents
#os.system("Rscript -e rmarkdown::render('MiniProjMainDocument.Rmd')")
