#!usr/bin/python
# Week 7 Advanced python



___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

def a_useless_function(x):
    y = 0
    for i in range(100000000):
        y = y + i
    return 0
def another_useless_function(x):
    y = 0
    z = 0
    while z <= 100000000:
        y = y + x
        z += 1
    return 0

def a_less_useless_function(x):
    y = 0
    for i in range(100000):
        y = y + i
    return 0
def some_function(x):
    print x
    a_useless_function(x)
    another_useless_function(x)
    a_useless_function(x)
    return 0

some_function(1000)

print 'profileme'