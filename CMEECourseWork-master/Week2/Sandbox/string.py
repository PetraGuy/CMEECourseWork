
teststring = "blag blahAATCTAT"
print teststring

import re

teststring = re.sub(r'blah', '', teststring)

print teststring
