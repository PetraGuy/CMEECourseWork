#!/bin/bash

#bash script to run python and R scripts

cd ../Code
python LV1.py
python LV2.py 0.1 0.1 0.1 0.1
python LV3.py 1 0.1 1 1
python LV4.py 1 0.1 1 1 10 5
python LV5.py 0.1 0.1 0.1 0.1 1 1
python blackbirds.py
python DrawFW.py
python profileme.py
python regexs.py
python timeitme.py
python TestR.py
python Nets.py
Rscript Nets.R
Rscript TestR.R
python timeitem.py
python using os.py
python run_fmr_R.py

