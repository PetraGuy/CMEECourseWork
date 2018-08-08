
Week 1 summary

Documents/CMEECourseWork/Week1/Code contains 8 bash files. 

UnixPrac1, a commented out bash file, the chapter 1 practical. I couldn't get expr to work , kept getting 0, I guess because I wasnt handling the decimal places, so I used bc.

6 files are the exercises set during chapter 2; 

ConcatenateTwoFiles - contents of one file added to the end of another (>>)

CountLines - counts lines, wc -l, saved to a variable called NumLines. Not sure why I needed $(wc -l <$filename) when example script had 'wc-1...' 

MyExampleScript - outputs Hello to the terminal user ($USER),

tabtocsv - removes tabs \t ad replaces them commas using tr. This script could be improved by not hard coding the output filename. I got as far as removing the .txt extension, but didnt work out how to addd .csv to the entered filename.
	
variables - shows how to use variables in bash - varname=Bob - no spaces!!

csvtospace is the end of chapter 2 practical. The bash rewrote files in my /Data/Temperatures directory, and saved them in the same directory. Note that I have fist copied the files so as not to overwrite them, but it would be better if I'd cut of the extenstion, so the output filename is 1800.csv.txt - bit ugly.Also, as this bash stands, it will take everything in the temperature directory and make copies, so if there are other files, they'll be copied too. 



And ComplileLatex.sh, which repeates the commands needed to create the pdf from the latex file. (Note to self, make sure you've saved the file and created the bibliography - just create the file name and paste the citation in before compiling the latex file)

I have tried to ensure all data is in Data directory and therefore bash scripts look there for everything.

My program files could be better laid out in a standard format and the comments are not neat. I will improve that in the following weeks.








