#!/bin/bash
# Author pg5117@ic.ac.uk
# Script: csvtospace
# Desc: #take csv and convert to space separated
# keep input and save as new file
# Arguments: Requires csv file in Data dir
# Output, a new filename
#Date October 2017



echo ' ALL /Data/Temperatures will be copied and converted to space delimited'
cd ../Data/Temperatures
# this bit was to accept entered names
#echo 'enter filenames to convert'# this bit was to accept entered names
#read filename
#cat $filename | tr -s "," " " >> testspace # this for entering filenames

# or this loops through files in the directory
list="$(ls)"
for i in $list; do 
	cat "$i"  | tr -s "," " " >> $i.txt ;
done
ls





