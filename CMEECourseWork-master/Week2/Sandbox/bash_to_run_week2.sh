#!/bin/bash

#bash script to run Weeks2 scripts


cd ../Code
python basic_io.py test.txt tesp.p
python basic_csv.py testcsv.csv
python boilerplate.py
python using_name.py
python sysargv.py
python scope.py
python control_flow.py
python cfexercises.py
python loops.py
python oaks.py
python lc1.py
python lc2.py
python dictionary.py
python tuple.py
python align_seqs.py
python align_seqs_fasta.py
python -m doctest -v test_control_flow.py
python debugme.py
python -m doctest -v test_oaks.py




