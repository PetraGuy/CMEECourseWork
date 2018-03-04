#!usr/bin python

import subprocess

subprocess.call(["mv ../Documents/bibliography.bib ../Code"], shell=True)
subprocess.call(["mv ../Documents/Sorbusaucuparia.pdf ../Code"], shell=True)
subprocess.call(["mv ../Documents/bashrender.sh ../Code"], shell=True)

subprocess.Popen(["bash bashrender.sh"], shell=True).wait()

subprocess.call(["mv bibliography.bib ../Documents"], shell=True)
subprocess.call(["mv Sorbusaucuparia.pdf ../Documents"], shell=True)
subprocess.call(["mv bashrender.sh ../Documents"], shell=True)
subprocess.call(["mv MiniProjMainDocument.pdf ../Documents"], shell=True)
subprocess.call(["evince ../Documents/MiniProjMainDocument.pdf"],shell=True)


