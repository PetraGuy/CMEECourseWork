#!usr/bin/python
# Week 7 Advanced python



___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

import subprocess

subprocess.Popen("/usr/bin/env Rscript --verbose TestR.R > \
 ../Results/TestR.Rout 2> ../Results/TestR_errFile.Rout", shell=True).wait()

print "TestR output saved to Results"


