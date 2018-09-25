"""
Author: Andrew Jarombek
Date: 9/19/2018
"""

from Town import Town
from Run import Run
from copy import deepcopy

"""
Testing the Town class
"""
greenwich = Town('Greenwich', 1640, 62396, ['Riverside', 'Cos Cob', 'Old Greenwich', 'Greenwich'])

# Calls to print() and str() invoke the __repr__() special method
print(greenwich)
assert str(greenwich) == 'Town of Greenwich: Founded in 1640 with a Population of 62396'

# Calls to len() invoke the __len__() special method
print('Greenwich is %r Years Old' % len(greenwich))
assert len(greenwich) == 378

# Indexing an object invokes the __getitem__() special method
print('The Best District in Greenwich is %r' % greenwich[0])
assert greenwich[0] == 'Riverside'
assert greenwich[-2] == 'Old Greenwich'

# Implementing __getitem__() also allows for iteration over an object
for district in greenwich:
    print(district)
    assert district in ['Riverside', 'Cos Cob', 'Old Greenwich', 'Greenwich']

# Create a second town of Rye
rye = Town('Rye', 1660, 45928, ['Port Chester', 'Rye'])

# Test out the __eq__() special method
assert (greenwich == rye) is False

greenwich_clone = deepcopy(greenwich)
assert (greenwich == greenwich_clone) is True

"""
Testing the Run class
"""

shaker = Run(2.3, 16, 13)

# Calls to print() and str() invoke the __repr__() special method
print('Shaker: %s' % shaker)
assert str(shaker) == '2.3 miles in 16:13'

palmer_hill_run = Run(4.3, 30, 12)

# Calls to bool() invoke the __bool__() special method
assert bool(shaker)
assert bool(palmer_hill_run)

empty_run = Run()
assert not bool(empty_run)

extra_mile = Run(1, 6, 55)

# Adding two Run objects together invokes the __add__() special method
long_shaker = shaker + extra_mile

print('Long Shaker: %s' % long_shaker)
assert str(long_shaker) == '3.3 miles in 23:08'

invalid_run = Run(-10, -6, 109)

print('Invalid Run: %s' % invalid_run)
assert str(invalid_run) == '0 miles in 1:49'

# Applying the left shift operator to a Run invokes the __lshift__() special method
valid_run = invalid_run << 0.15

print('Valid Run: %s' % valid_run)
assert str(valid_run) == '0.15 miles in 1:49'
