#Author pg5117@ic.ac.uk
# Script: UnixPrac1
# Desc: 
# 1) Count How many lines in each file
# 2) Print everything starting from the second line in E.coli genome
# 3) Count sequence length of E. coli genome
# 4) Count matches for ATGC
# 5) Find ratio of A and T to G and C
# Arguments: requires soem fasta's AND E.coli.fasta in Data directory 
#October 2017



#cd ../Data # send to Data dir using relative path
#1 wc -l *.fasta
#2 cat E.coli.fasta | tail -78103
#3 tr -d "\n" < E.coli.fasta | wc -m 
#4 tr -d "\n" < E.coli.fasta | grep ATGC -o |  wc -l 
#5 tr -d "\n" < E.coli.fasta | tail -78103 | echo "scale=3 ; $( grep 'A\|T' -o E.coli.fasta|  wc -l)/$(grep 'C\|G' -o E.coli.fasta|  wc -l) " | bc



