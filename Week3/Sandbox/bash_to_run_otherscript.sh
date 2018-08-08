#!/bin/bash

#bash script to run python and R scripts

cd ../Code
Rscript basic_io.R trees.csv
Rscript boilerplate.R
Rscript get_TreeHeight.R trees.csv
Rscript TreeHeight.R trees.csv
python get_TreeHeight.py trees.csv
Rscript Control.R
Rscript apply1.R
Rscript apply2.R
Rscript break.R
Rscript next.R
Rscript Vectorize1.R
Rscript Vectorize2.R
python Vectorize1.py
python Vectorize2.py
Rscript Sample.R
Rscript try.R
Rscript browse.R
Rscript PP_Lattice.R #uses /ecol Archives.csv
Rscript TAutoCorr.R #uses/Data/KeyWest.csv
Rscript GPDD.R #uses GPDD.csv
Rscript PP_Regress.R #uses /ecol Archives.csv
bash CompileLatex.sh
#PP_Regress_loc.R.
#DataWrang.R
#DataWrangTidy.R.

#23





