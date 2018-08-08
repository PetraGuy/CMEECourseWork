#!usr/bin/bash

export RSTUDIO_PANDOC=/usr/lib/rstudio/bin/pandoc
#RScript -e "Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio/bin/pandoc")"
Rscript -e "rmarkdown::render('MiniProjMainDocument.Rmd')"
