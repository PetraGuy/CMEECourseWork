#!/bin/bash
#Author pg5117@ic.ac.uk
# Script: ConcatenateTwoFiles.sh
# Desc: takes texisting files and adds one to end of the other in new file
# Arguments : Requires file1.txt and file2.txt in Data
# Ouput: a filename specified by user
#October 2017
#
echo ' enter filenames to be merged'
echo 'for this example, file1.txt and file2.txt have been created in Data'
read var1 var2 
echo 'enter name for merged file' 
read mergedvar
cd ../Data

cat $var1 > $mergedvar
cat $var2 >> $mergedvar
echo "Merged File is"
cat $mergedvar
