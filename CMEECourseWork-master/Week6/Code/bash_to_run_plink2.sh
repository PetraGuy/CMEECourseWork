#!bin/bash
#running from code directory



cd ../Data

plink --bfile ME_Dataset1 --freqx --out ME_Dataset1

cd ../Code
perl frqx2geno.pl ../Data/ME_Dataset1.frqx ../Data/ME_Dataset1.geno
Rscript Ob_v_Ex_het.R ME_Dataset1.geno ME_Dataset1.pdf
Rscript Moving_F.R ME_Dataset1.geno MA.pdf
cd ../Data
plink --bfile  ME_Dataset1 --hardy --out $var1
sort -k9 ME_Dataset1.hwe | tail -n 50 > ME_Dataset1.txt
