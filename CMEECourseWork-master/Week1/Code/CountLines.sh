#!/bin/bash
#Author pg5117@ic.ac.uk
# Script: CountLines.sh
# Desc:  Count and output number of lines in a file input by user
#Arguments: Requires any file to count, enter any file name in Data dir
#October 2017
#

echo 'enter a filename to count the lines of'
echo 'eg, spawannxs.txt exists in Data'
echo 'or any of these '
cd ../Data
ls
read filename
NumLines=$(wc -l <$filename)
echo "The file" $filename 'has' $NumLines 'lines'

