#!\bin\bash

Rscript -e "library(knitr); knit('MiniProj3.Rmd')"

Rscript -e "library(rmarkdown); render('MiniProj3.md')"
