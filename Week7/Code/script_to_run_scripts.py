#!usr/bin python

import subprocess



subprocess.call("python LV1.py", shell=True)
subprocess.Popen("python LV2.py 0.1 0.1 0.1 0.1", bufsize=400, shell=True)
subprocess.Popen("python LV3.py 1 1 0.1 5",shell=True)
subprocess.Popen("python LV4.py 0.1 0.1 0.1 0.1 1 1",shell=True).wait()
subprocess.Popen("python LV5.py 0.1 0.1 0.1 0.1 1 1",shell=True).wait()


# the script needed shell=True and the prog name and arguments in one string.
#the .wait had to occur on the final line otherwise on executing, you
#dont get back to command prompt...no - when I added LV5 needed .wait on both

