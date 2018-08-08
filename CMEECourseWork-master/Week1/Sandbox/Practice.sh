#!/bin/bash
#Practising tr commands
#Not part of coursework

$ echo "Remove
excess
spaces." | tr -s "\b" " "
Remove excess spaces.
$ echo "remove all the as" | tr -d "a"
remove ll the s
$ echo "set to uppercase" | tr [:lower:] [:upper:]
SET TO UPPERCASE
$ echo "10.00 only numbers 1.33" | tr -d [:alpha:] | tr -s "\b" ","
10.00,1.33
