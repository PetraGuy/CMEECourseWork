#!bin/bash
#running from code directory

echo "input file?"

read var1
cd ../Data

plink --bfile $var1 --freqx --out $var1
cd ../Code
perl frqx2geno.pl ../Data/$var1 ../Data/$var1.geno
Rscript Ob_v_Ex_het.R $var1.geno $var1.pdf
Rscript Moving_F.R $var1.geno MA.pdf
cd ../Data
plink --bfile  $var1 --hardy --out $var1
sort -k9 $var1.hwe | tail -n 50 > $var1.txt
