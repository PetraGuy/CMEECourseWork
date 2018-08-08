#!/bin/bash
#Author pg5117@ic.ac.uk
# Script: variables.sh
# Desc: # Shows use of variables in bash - NO SPACES!
#October 2017

MyVar='a string'
echo 'the current value of the variable is' $MyVar
echo 'Please enter a new string'
read MyVar
echo 'the current value of the variable is' $MyVar
## Reading multiple values
echo 'Enter two numbers separated by space(s)'
read a b
echo 'you entered' $a 'and' $b '. Their sum is:'
mysum=`expr $a + $b`
echo $mysum
